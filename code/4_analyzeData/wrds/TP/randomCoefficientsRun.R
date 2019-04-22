# Running multinomial models
library(mlogit, lib.loc = "~/lib/R")
library(data.table)
library(stargazer)
library(car)
library(ggplot2)
library(ggthemes)
library(mvtnorm)
library(ggridges)
threads <- 8
fullChoice <- fread("/scratch/upenn/hossaine/TPMLogit.csv", nThread = threads)
fullChoice <- na.omit(fullChoice, cols = "household_code")
fullChoice[, c("p", "pCents", "unitCost") := .(pCents / 100, NULL, unitCost / 100)]
fullChoice[type_of_residence == "Mobile", "type_of_residence" := "Multi-Family"]

# Computing the cumulative spending up to that point (excluding current trip)
fullChoice[, "cumSpend" := cumSpend - total_spent]

brandKey <- data.table(brand = c("Angel Soft", "Charmin", "Other", "Cottonelle",
                                 "Qltd Ntn", "Scott"),
                       topBrand = c(506045, 526996, 536746, 581898, 624459, 635074))
fullChoice <- merge(fullChoice, brandKey, by = "topBrand")
Purch <- fullChoice[, .(choice = sum(choice)), by = trip_code_uc][choice == 1]$trip_code_uc
fullChoice <- fullChoice[trip_code_uc %in% Purch]

# Collapsing products that are not uniquely defined by a brand-size
fullChoice <- fullChoice[, .(size = mean(size),
                             choice = sum(choice),
                             p = mean(p),
                             unitCost = mean(unitCost)),
                         by = .(alt, topBrand, brand, pkgSize, household_code,
                                trip_code_uc, week_end, projection_factor,
                                household_income, type_of_residence, market,
                                college, white, household_income_cts, rate,
                                total_spent, cumSpend)]
Purch <- fullChoice[, .(choice = sum(choice)), by = trip_code_uc][choice == 1]$trip_code_uc
fullChoice <- fullChoice[trip_code_uc %in% Purch]
fullChoice[, "charmin" := ifelse(brand == "Charmin", 1L, 0L)]
fullChoice[, "cotton" := ifelse(brand == "Cottonelle", 1L, 0L)]
fullChoice[, "other" := ifelse(brand == "Other", 1L, 0L)]
fullChoice[, "qn" := ifelse(brand == "Qltd Ntn", 1L, 0L)]
fullChoice[, "scott" := ifelse(brand == "Scott", 1L, 0L)]
fullChoice[, "sizeGamma" := size ^ 0.4]
fullChoice[, "pkgSize2" := pkgSize ^ 2]

# Restricting to trips with at least 10 options
fullChoice[, "nChoice" := .N, by = trip_code_uc]
fullChoice <- fullChoice[nChoice > 10]

# Adding in indicator for if it is the start or end of the month as a liquidity
# shifter as in Orhun and Palazzolo
fullChoice[, "day" := as.integer(substr(week_end, 7, 8))]
fullChoice[, "monthStart" := (day <= 7)]

# Testing for multicollinearity
# Variance Inflation Factor test (<5 or 10 is good)
vif(lm(data = fullChoice, size ~ pkgSize + unitCost + brand))
vif(lm(data = fullChoice, pkgSize ~ size + unitCost + brand))
vif(lm(data = fullChoice, unitCost ~ size + pkgSize + brand))

# Adding in demographic and product characteristic interactions for observed preference heterogeneity
fullChoice[, ':=' (p2550 = (household_income_cts >= 25 & household_income_cts < 50) * p,
                   p50100 = (household_income_cts >= 50 & household_income_cts < 100) * p,
                   pOver100 = (household_income_cts >= 100) * p,
                   pApt = (type_of_residence != "Single-Family") * p,
                   pCollege = college * p,
                   pWhite = white * p,
                   pRate = rate * p,
                   pCumSpend = cumSpend * p,
                   pCumSpend2550 = cumSpend * (household_income_cts >= 25 & household_income_cts < 50) * p,
                   pCumSpend50100 = cumSpend * (household_income_cts > 50 & household_income_cts <= 100) * p,
                   pCumSpendOver100 = cumSpend * (household_income_cts >= 100) * p,
                   unitCost2550 = (household_income_cts >= 25 & household_income_cts < 50) * unitCost,
                   unitCost50100 = (household_income_cts >= 50 & household_income_cts < 100) * unitCost,
                   unitCostOver100 = (household_income_cts >= 100) * unitCost,
                   unitCostApt = (type_of_residence != "Single-Family") * unitCost,
                   unitCostCollege = college * unitCost,
                   unitCostWhite = white * unitCost,
                   unitCostRate = rate * unitCost,
                   unitCostMonthStart = monthStart * unitCost,
                   unitCostMonthStart2550 = monthStart * (household_income_cts >= 25 & household_income_cts < 50) * unitCost,
                   unitCostMonthStart50100 = monthStart * (household_income_cts > 50 & household_income_cts <= 100) * unitCost,
                   unitCostMonthStartOver100 = monthStart * (household_income_cts >= 100) * unitCost,
                   unitCostCumSpend = cumSpend * unitCost,
                   unitCostCumSpend2550 = cumSpend * (household_income_cts >= 25 & household_income_cts < 50) * unitCost,
                   unitCostCumSpend50100 = cumSpend * (household_income_cts > 50 & household_income_cts <= 100) * unitCost,
                   unitCostCumSpendOver100 = cumSpend * (household_income_cts >= 100) * unitCost,
                   size2550 = (household_income_cts >= 25 & household_income_cts < 50) * size,
                   size50100 = (household_income_cts >= 50 & household_income_cts < 100) * size,
                   sizeOver100 = (household_income_cts >= 100) * size,
                   sizeApt = (type_of_residence != "Single-Family") * size,
                   sizeCollege = college * size,
                   sizeWhite = white * size,
                   sizeRate = rate * size,
                   pkgSize2550 = (household_income_cts >= 25 & household_income_cts < 50) * pkgSize,
                   pkgSize50100 = (household_income_cts >= 50 & household_income_cts < 100) * pkgSize,
                   pkgSizeOver100 = (household_income_cts >= 100) * pkgSize,
                   pkgSizeApt = (type_of_residence != "Single-Family") * pkgSize,
                   pkgSizeCollege = college * pkgSize,
                   pkgSizeWhite = white * pkgSize,
                   pkgSizeRate = rate * pkgSize,
                   charmin2550 = (household_income_cts >= 25 & household_income_cts < 50) * charmin,
                   charmin50100 = (household_income_cts >= 50 & household_income_cts < 100) * charmin,
                   charminOver100 = (household_income_cts >= 100) * charmin,
                   charminApt = (type_of_residence != "Single-Family") * charmin,
                   charminCollege = college * charmin,
                   charminWhite = white * charmin,
                   charminRate = rate * charmin,
                   cotton2550 = (household_income_cts >= 25 & household_income_cts < 50) * cotton,
                   cotton50100 = (household_income_cts >= 50 & household_income_cts < 100) * cotton,
                   cottonOver100 = (household_income_cts >= 100) * cotton,
                   cottonApt = (type_of_residence != "Single-Family") * cotton,
                   cottonCollege = college * cotton,
                   cottonWhite = white * cotton,
                   cottonRate = rate * cotton,
                   other2550 = (household_income_cts >= 25 & household_income_cts < 50) * other,
                   other50100 = (household_income_cts >= 50 & household_income_cts < 100) * other,
                   otherOver100 = (household_income_cts >= 100) * other,
                   otherApt = (type_of_residence != "Single-Family") * other,
                   otherCollege = college * other,
                   otherWhite = white * other,
                   otherRate = rate * other,
                   qn2550 = (household_income_cts >= 25 & household_income_cts < 50) * qn,
                   qn50100 = (household_income_cts >= 50 & household_income_cts < 100) * qn,
                   qnOver100 = (household_income_cts >= 100) * qn,
                   qnApt = (type_of_residence != "Single-Family") * qn,
                   qnCollege = college * qn,
                   qnWhite = white * qn,
                   qnRate = rate * qn,
                   scott2550 = (household_income_cts >= 25 & household_income_cts < 50) * scott,
                   scott50100 = (household_income_cts >= 50 & household_income_cts < 100) * scott,
                   scottOver100 = (household_income_cts >= 100) * scott,
                   scottApt = (type_of_residence != "Single-Family") * scott,
                   scottCollege = college * scott,
                   scottWhite = white * scott,
                   scottRate = rate * scott)]

  topShoppers <- fullChoice[, uniqueN(trip_code_uc), by = .(household_code, market)]
  setorder(topShoppers, -V1)
############################ TESTING ###########################################
getMNL <- function(city) {
  # topIDs <- topShoppers[V1 > 10 & V1 < 25 & market == city]$household_code
  # focusSet <- fullChoice[household_code %in% topIDs[1:250]]
  focusSet <- fullChoice[market == city]
  draws <- 100

  # Running MNL regression
  focusM <- mlogit.data(data = focusSet, choice = "choice", shape = "long",
                        alt.var = "alt", chid.var = "trip_code_uc",
                        id.var = "household_code")

  # Regular MNL (including price, unitCost, and package size)
  reg1 <- mlogit(choice ~ unitCost + size + pkgSize +
                   charmin + cotton + other + qn + scott | 0,
                 data = focusM)
  print("Reg 1 done")

  # Mixed Logit (no observed heterogeneity)
  reg2 <- mlogit(choice ~ unitCost + size + pkgSize +
                   charmin + cotton + other + qn + scott | 0,
                 data = focusM,
                 rpar = c(unitCost = "n", size = "n", pkgSize = "n",
                          charmin = "n", cotton = "n", other = "n", qn = "n", scott = "n"),
                 R = draws, halton = NA, panel = TRUE, correlation = TRUE)
  print("Reg2: Mixed Logit, no observed hetero")
  print(summary(vcov(reg2, what = "rpar", type = "cor")))

  # MNL with observed heterogeneity
  reg3 <- mlogit(choice ~ unitCost +
                   unitCost2550 + unitCost50100 + unitCostOver100 + unitCostApt +
                   unitCostCollege + unitCostWhite + unitCostRate +
                   unitCostMonthStart + unitCostMonthStart2550 + unitCostMonthStart50100 + unitCostMonthStartOver100 +
                   size +
                   size2550 + size50100 + sizeOver100 + sizeApt +
                   sizeCollege + sizeWhite + sizeRate +
                   pkgSize +
                   pkgSize2550 + pkgSize50100 + pkgSizeOver100 + pkgSizeApt +
                   pkgSizeCollege + pkgSizeWhite + pkgSizeRate +
                   charmin +
                   charmin2550 + charmin50100 + charminOver100 + charminApt +
                   charminCollege + charminWhite + charminRate +
                   cotton +
                   cotton2550 + cotton50100 + cottonOver100 + cottonApt +
                   cottonCollege + cottonWhite + cottonRate +
                   other +
                   other2550 + other50100 + otherOver100 + otherApt +
                   otherCollege + otherWhite + otherRate +
                   qn +
                   qn2550 + qn50100 + qnOver100 + qnApt +
                   qnCollege + qnWhite + qnRate +
                   scott +
                   scott2550 + scott50100 + scottOver100 + scottApt +
                   scottCollege + scottWhite + scottRate | 0,
                 data = focusM)
  print("Reg 3 done")

  # Mixed Logit with Observed Heterogeneity (demographics, with correlation)
  reg4 <- mlogit(choice ~ unitCost +
                   unitCost2550 + unitCost50100 + unitCostOver100 + unitCostApt +
                   unitCostCollege + unitCostWhite + unitCostRate +
                   unitCostMonthStart + unitCostMonthStart2550 + unitCostMonthStart50100 + unitCostMonthStartOver100 +
                   size +
                   size2550 + size50100 + sizeOver100 + sizeApt +
                   sizeCollege + sizeWhite + sizeRate +
                   pkgSize +
                   pkgSize2550 + pkgSize50100 + pkgSizeOver100 + pkgSizeApt +
                   pkgSizeCollege + pkgSizeWhite + pkgSizeRate +
                   charmin +
                   charmin2550 + charmin50100 + charminOver100 + charminApt +
                   charminCollege + charminWhite + charminRate +
                   cotton +
                   cotton2550 + cotton50100 + cottonOver100 + cottonApt +
                   cottonCollege + cottonWhite + cottonRate +
                   other +
                   other2550 + other50100 + otherOver100 + otherApt +
                   otherCollege + otherWhite + otherRate +
                   qn +
                   qn2550 + qn50100 + qnOver100 + qnApt +
                   qnCollege + qnWhite + qnRate +
                   scott +
                   scott2550 + scott50100 + scottOver100 + scottApt +
                   scottCollege + scottWhite + scottRate | 0,
                 data = focusM,
                 rpar = c(unitCost = "n", size = "n", pkgSize = "n",
                          charmin = "n", cotton = "n", other = "n", qn = "n", scott = "n"),
                 R = draws, halton = NA, panel = TRUE, correlation = TRUE)
  print("Reg4: Mixed Logit, Demographic Heterogeneity")
  print(summary(vcov(reg4, what = "rpar", type = "cor")))

  stargazer(reg1, reg2, reg3, reg4, type = "text")
  save(reg4, file = paste0("/home/upenn/hossaine/regs/", city, ".rda"), compress = TRUE)
}

getMNL("Boston")
# getMNL("Dallas")
# getMNL("Philadelphia")
# getMNL("Los Angeles")
# getMNL("Chicago")
# getMNL("Phoenix")
# getMNL("Minneapolis")
# getMNL("Columbus")
# getMNL("Charlotte")
# getMNL("Denver")

############################### WTP ###########################################
# Plots distribution of WTP given a regression, demographics, and parameter
###############################################################################
coefUnitCost <- c(unitCost = -4.674,
                  unitCost2550 = 0,
                  unitCost50100 = 0.597,
                  unitCostOver100 = 0.950,
                  unitCostApt = 0.620,
                  unitCostCollege = 0,
                  unitCostWhite = 1.802,
                  unitCostRate = -2.036)

coefPkgSize <- c(pkgSize = -0.084,
                  pkgSize2550 = 0,
                  pkgSize50100 = 0.032,
                  pkgSizeOver100 = 0.040,
                  pkgSizeApt = -0.016,
                  pkgSizeCollege = 0,
                  pkgSizeWhite = 0,
                  pkgSizeRate = -0.042)



# Plotting simulated densities
nDraws <- 100000
sdUnitCost <- 0.906
sdPkgSize <- 0.025

finalTable <- NULL

# <25k
set.seed(121)
below25 <- c(1, 0, 0, 0, 0, 1, 1, 0.25)
unitCost <- sum(below25 * coefUnitCost)
pkgSize <- sum(below25 * coefPkgSize)
wtp <- rnorm(nDraws, pkgSize, sdPkgSize) / rnorm(nDraws, unitCost, sdUnitCost)
finalTable <- rbind(finalTable, quantile(wtp, c(0.01, 0.10, 0.25, 0.5, 0.75, 0.90, 0.99)))

# 25-50k
set.seed(121)
below25 <- c(1, 1, 0, 0, 0, 1, 1, 0.25)
unitCost <- sum(below25 * coefUnitCost)
pkgSize <- sum(below25 * coefPkgSize)
wtp <- rnorm(nDraws, pkgSize, sdPkgSize) / rnorm(nDraws, unitCost, sdUnitCost)
finalTable <- rbind(finalTable, quantile(wtp, c(0.01, 0.10, 0.25, 0.5, 0.75, 0.90, 0.99)))

#50-100k
set.seed(121)
below25 <- c(1, 0, 1, 0, 0, 1, 1, 0.25)
unitCost <- sum(below25 * coefUnitCost)
pkgSize <- sum(below25 * coefPkgSize)
wtp <- rnorm(nDraws, pkgSize, sdPkgSize) / rnorm(nDraws, unitCost, sdUnitCost)
finalTable <- rbind(finalTable, quantile(wtp, c(0.01, 0.10, 0.25, 0.5, 0.75, 0.90, 0.99)))

# > 100k
set.seed(121)
below25 <- c(1, 0, 0, 1, 0, 1, 1, 0.25)
unitCost <- sum(below25 * coefUnitCost)
pkgSize <- sum(below25 * coefPkgSize)
wtp <- rnorm(nDraws, pkgSize, sdPkgSize) / rnorm(nDraws, unitCost, sdUnitCost)
finalTable <- rbind(finalTable, quantile(wtp, c(0.01, 0.10, 0.25, 0.5, 0.75, 0.90, 0.99)))

round(finalTable, 4)

fullDems <- rbind(1, dems, dems["income", ] * dems["monthStart", ])
muP <- colSums(fullDems * matrix(coefP, ncol = 4, nrow = length(coefP)))
muPar <- colSums(fullDems * matrix(coefPar, ncol = 4, nrow = length(coefPar)))

parDraw <- rmvnorm(nDraws, muPar, diag(sdPar, 4, 4))
wtp <- data.table(wtp = - parDraw / matrix(muP, nrow = nDraws, ncol = 4, byrow = TRUE))
setnames(wtp, c("25k", "50k", "75k", "100k"))
wtpLong <- melt(wtp, variable.name = "income", value.name = "wtp")

ggplot(data = wtpLong, aes(x = wtp)) +
  geom_density(aes(linetype = income)) +
  geom_vline(xintercept = 0) +
  labs(title = inputTitle) +
  scale_x_continuous(limits = c(-15, 5)) +
  scale_y_continuous(limits = c(0, 0.65)) +
  theme_fivethirtyeight()
ggsave(paste0("./code/5_figures/", fileName))

WTPTable <- NULL
for (i in 1:ncol(dems)) {
  set.seed(121)
  wtp <- getWTP(coefP, coefPar = coefSmall, sdPar = sdSmall, dems[, i],
                nDraws = 10000, inputTitle = titles[i], xrange = c(-10, 5), yrange = c(0, 4))
  WTPTable <- rbind(WTPTable, wtp)
}

#################### WTP MANUAL CALCULATIONS FOR PHILLY FED ####################
pCoefs <- matrix(c(-4.468, 0, 0.468, 0.629, 0.613, 0, 1.812, -1.911), ncol = 1)
hhtypes <- t(data.table(Apt25     = c(1, 0, 0, 0, 1, 1, 1, 0.25),
                      Apt2550   = c(1, 1, 0, 0, 1, 1, 1, 0.25),
                      Apt50100  = c(1, 0, 1, 0, 1, 1, 1, 0.25),
                      Apt100    = c(1, 0, 0, 1, 1, 1, 1, 0.25),
                      Home25    = c(1, 0, 0, 0, 0, 1, 1, 0.25),
                      Home2550  = c(1, 1, 0, 0, 0, 1, 1, 0.25),
                      Home50100 = c(1, 0, 1, 0, 0, 1, 1, 0.25),
                      Home100   = c(1, 0, 0, 1, 0, 1, 1, 0.25)))
colnames(hhtypes) <- c("Int", "Inc2550", "Inc50100", "Inc100", "Apt", "College", "White", "Rate")
pHH <- hhtypes %*% pCoefs

sizeCoefs <- matrix(c(-0.084, 0, 0.032, 0.040, -0.017, 0, 0, -0.042), ncol = 1)
sizeHH <- hhtypes %*% sizeCoefs

wtpSize <- sizeHH / pHH

# Plotting distributions of WTPs
set.seed(121)
pDraws <- rnorm(10000, -4.468, 0.896)
sizeDraws <- rnorm(10000, -0.084, 0.024)
pCoefs <- cbind(pDraws, 0, 0.468, 0.629, 0.613, 0, 1.812, -1.911)
sizeCoefs <- cbind(sizeDraws, 0, 0.032, 0.040, -0.017, 0, 0, -0.042)

wtpDist <- matrix(NA, nrow = 10000, ncol = 8)

for (i in 1:8) {
  pHH <- pCoefs %*% matrix(hhtypes[i, ], ncol = 1)
  sizeHH <- sizeCoefs %*% matrix(hhtypes[i, ], ncol = 1)

  wtpDist[, i] <- - sizeHH / pHH / 0.69 * 100 # Relative to median unit price
}

graphData <- as.data.table(wtpDist)
setnames(graphData, c("Apt_25", "Apt_2550", "Apt_50100", "Apt_100",
                      "Home_25", "Home_2550", "Home_50100", "Home_100"))
graphData <- melt(graphData)
graphData[, c("Home", "Income") := tstrsplit(variable, "_", fixed = TRUE)]
graphData[, "Income" := factor(Income, levels = c("25", "2550", "50100", "100"),
                               labels = c("<25k", "25-50k", "50-100k", ">100k"),
                               ordered = TRUE)]
setnames(graphData, "value", "Willingness to Pay")

ggplot(graphData, aes(x = `Willingness to Pay`, y = Income)) +
  geom_density_ridges() +
  facet_grid(cols = vars(Home)) +
  scale_x_continuous(limits = c(-15, 0)) +
  labs(title = "Willingness to Pay for Smaller Sizes Decreases in Income",
       x = "Willingness to Pay (Relative to Median Unit Price)",
       y = "Income",
       caption = paste0("Source: Author calulations.")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text())
ggsave(filename = "./code/5_figures/wtpRidges.png")

################################################################################
################### PREDICTIONS ################################################
################################################################################
city <- "Boston"
load(paste0("./regs", city, ".rda"))
masterKey <- unique(fullChoice[, .(alt, brand, sizeCat)])
setorder(masterKey, alt)

topIDs <- topShoppers[V1 > 10 & V1 < 25 & market == city]$household_code
focusSet <- fullChoice[household_code %in% topIDs[1:250]][sizeCat == "small", "p" := 9999]
predictDataM <- mlogit.data(data = focusSet,
                           choice = "choice", shape = "long",
                           alt.var = "alt", chid.var = "trip_code_uc",
                           id.var = "household_code")
object <- reg4
newdata <- predictDataM
returnData <- FALSE
predict.mlogit <- function(object, newdata = NULL, returnData = FALSE, ...){
  # if no newdata is provided, use the mean of the model.frame
  if (is.null(newdata)) newdata <- mean(model.frame(object))
  # if newdata is not a mlogit.data, it is coerced below
  if (! inherits(newdata, "mlogit.data")){
    rownames(newdata) <- NULL
    lev <- colnames(object$probabilities)
    J <- length(lev)
    choice.name <- attr(model.frame(object), "choice")
    if (nrow(newdata) %% J)
      stop("the number of rows of the data.frame should be a multiple of the number of alternatives")
    attr(newdata, "index") <- data.frame(chid = rep(1:(nrow(newdata) %/% J ), each = J), alt = rep(lev, J))
    attr(newdata, "class") <- c("mlogit.data", "data.frame")
    if (is.null(newdata[['choice.name']])){
      newdata[[choice.name]] <- FALSE
      newdata[[choice.name]][1] <- TRUE # probit and hev requires that one (arbitrary) choice is TRUE
    }
  }

  # if the updated model requires the use of mlogit.data, suppress all
  # the relevant arguments
  m <- match(c("choice", "shape", "varying", "sep",
               "alt.var", "chid.var", "alt.levels",
               "opposite", "drop.index", "id", "ranked"),
             names(object$call), 0L)
  if (sum(m) > 0) object$call <- object$call[ - m]
  # update the model and get the probabilities
  newobject <- update(object, start = coef(object, fixed = TRUE), data = newdata, iterlim = 0, print.level = 0)
  #    newobject <- update(object, start = coef(object), data = newdata, iterlim = 0, print.level = 0)

  result <- newobject$probabilities
  if (nrow(result) == 1){
    result <- as.numeric(result)
    names(result) <- colnames(object$probabilities)
  }
  if (returnData) attr(result, "data") <- newdata
  result
}

Oprob <- fitted(reg4, type = "probabilities")
Nprob <- result
apple <- rbind(old = apply(Oprob, 2, mean, na.rm = TRUE), new = apply(Nprob, 2, mean, na.rm = TRUE))
predictions <- as.data.table(t(apple), keep.rownames = "alt")[, "alt" := as.integer(alt)]
predictions <- merge(predictions, masterKey, by = "alt")
predictions[, .(old = sum(old) / sum(predictions$old),
                new = sum(new) / sum(predictions$new)), by = brand]

################################################################################
################### CONSUMER SURPLUS ###########################################
################################################################################
# Consumer surplus is simply the logsum value divided by the cost coefficient

# Sourcing the modified logsum() function that allows for different choice sets
source("/home/upenn/hossaine/Nielsen/logsum.R")

# Computing the price coefficient for each household
# 1. I get the price coefficients from the estimation
# 2. Get the demographics of each houeshold
# 3. Do matrix multiplication to get the household's price coefficient
# 4. Only take the price coefficient for their choice (doesn't matter, it's the
# same for all alternatives within a choice situation)
# 5. Logsum / -alpha is the surplus (up to a constant)
pCoefs <- coef(reg)[grepl("^p", names(coef(reg)))]
customP <- focusSet[, .(intercept = 1,
                        income = household_income_cts,
                        rate = rate,
                        multifam = (type_of_residence == "Multi-Family"),
                        mobile = (type_of_residence == "Mobile"),
                        college = college,
                        monthStart = monthStart,
                        white = white,
                        monthStartPoor = monthStart * household_income_cts)]
newP <- as.matrix(customP) %*% as.matrix(pCoefs, ncol = 1)
newP <- newP[focusSet$choice == 1]
surplus <- logsum(reg) / -newP

# Combining surplus with each trip and plotting the 2 groups
tripSurplus <- data.table(trip_code_uc = unique(focusSet$trip_code_uc),
                          surplus = surplus)
tripSurplus <- merge(tripSurplus, unique(focusSet[, .(trip_code_uc, household_income_cts)]),
                     by = "trip_code_uc")
plot(quantile(tripSurplus[household_income_cts < 30]$surplus, seq(0, 1, 0.01)),
     quantile(tripSurplus[household_income_cts > 80]$surplus, seq(0, 1, 0.01)))
abline(a = 0, b = 1)
