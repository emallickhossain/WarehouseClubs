# Running multinomial models
library(mlogit, lib.loc = "~/lib/R")
library(data.table)
library(stargazer)
library(car)
library(ggplot2)
library(ggthemes)
threads <- 8
fullChoice <- fread("/scratch/upenn/hossaine/TPMLogit.csv", nThread = threads)
fullChoice <- na.omit(fullChoice, cols = "household_code")
fullChoice[, c("p", "pCents", "unitCost") := .(pCents / 100, NULL, unitCost / 100)]

brandKey <- data.table(brand = c("Angel Soft", "Charmin", "Other", "Cottonelle",
                                 "Qltd Ntn", "Scott"),
                       topBrand = c(506045, 526996, 536746, 581898, 624459, 635074))
fullChoice <- merge(fullChoice, brandKey, by = "topBrand")
Purch <- fullChoice[, .(choice = sum(choice)), by = trip_code_uc][choice == 1]$trip_code_uc
fullChoice <- fullChoice[trip_code_uc %in% Purch]

# Collapsing products that are not uniquely defined by a brand-size
fullChoice <- fullChoice[, .(size = mean(size),
                             choice = sum(choice),
                             p = mean(p)),
                         by = .(alt, topBrand, brand, sizeCat, household_code,
                                trip_code_uc, week_end, projection_factor,
                                household_income, type_of_residence, market,
                                college, white, household_income_cts, rate)]
Purch <- fullChoice[, .(choice = sum(choice)), by = trip_code_uc][choice == 1]$trip_code_uc
fullChoice <- fullChoice[trip_code_uc %in% Purch]
fullChoice[, "small" := ifelse(sizeCat == "small", 1L, 0L)]
fullChoice[, "medium" := ifelse(sizeCat == "medium", 1L, 0L)]
fullChoice[, "large" := ifelse(sizeCat == "large", 1L, 0L)]
fullChoice[, "logP" := log(p)]
fullChoice[, "angelSoft" := ifelse(brand == "Angel Soft", 1L, 0L)]
fullChoice[, "charmin" := ifelse(brand == "Charmin", 1L, 0L)]
fullChoice[, "cotton" := ifelse(brand == "Cottonelle", 1L, 0L)]
fullChoice[, "other" := ifelse(brand == "Other", 1L, 0L)]
fullChoice[, "qn" := ifelse(brand == "Qltd Ntn", 1L, 0L)]
fullChoice[, "scott" := ifelse(brand == "Scott", 1L, 0L)]

# Restricting to trips with at least 10 options
fullChoice[, "nChoice" := .N, by = trip_code_uc]
fullChoice <- fullChoice[nChoice > 10]

# Adding in indicator for if it is the start or end of the month as a liquidity
# shifter as in Orhun and Palazzolo
fullChoice[, "day" := as.integer(substr(week_end, 7, 8))]
fullChoice[, "monthStart" := (day < 10)]

# Testing for multicollinearity
# Variance Inflation Factor test (<5 or 10 is good)
vif(lm(data = fullChoice, p ~ size + sizeCat + brand))
vif(lm(data = fullChoice, size ~ p + sizeCat + brand))

# Adding in demographic and product characteristic interactions for observed preference heterogeneity
fullChoice[, ':=' (pInc = household_income_cts * p,
                   pRate = rate * p,
                   pMultiFam = (type_of_residence == "Multi-Family") * p,
                   pMobile = (type_of_residence == "Mobile") * p,
                   pCollege = college * p,
                   pMonthStart = monthStart * p,
                   pWhite = white * p,
                   pMonthStartPoor = monthStart * (household_income_cts <= 35) * p,
                   sizeInc = household_income_cts * size,
                   sizeRate = rate * size,
                   sizeMultiFam = (type_of_residence == "Multi-Family") * size,
                   sizeMobile = (type_of_residence == "Mobile") * size,
                   sizeCollege = college * size,
                   sizeMonthStart = monthStart * size,
                   sizeWhite = white * size,
                   sizeMonthStartPoor = monthStart * (household_income_cts <= 35) * size,
                   smallInc = household_income_cts * small,
                   smallRate = rate * small,
                   smallMultiFam = (type_of_residence == "Multi-Family") * small,
                   smallMobile = (type_of_residence == "Mobile") * small,
                   smallCollege = college * small,
                   smallMonthStart = monthStart * small,
                   smallWhite = white * small,
                   smallMonthStartPoor = monthStart * (household_income_cts <= 35) * small,
                   largeInc = household_income_cts * large,
                   largeRate = rate * large,
                   largeMultiFam = (type_of_residence == "Multi-Family") * large,
                   largeMobile = (type_of_residence == "Mobile") * large,
                   largeCollege = college * large,
                   largeMonthStart = monthStart * large,
                   largeWhite = white * large,
                   largeMonthStartPoor = monthStart * (household_income_cts <= 35) * large,
                   charminInc = household_income_cts * charmin,
                   charminRate = rate * charmin,
                   charminMultiFam = (type_of_residence == "Multi-Family") * charmin,
                   charminMobile = (type_of_residence == "Mobile") * charmin,
                   charminCollege = college * charmin,
                   charminMonthStart = monthStart * charmin,
                   charminWhite = white * charmin,
                   charminMonthStartPoor = monthStart * (household_income_cts <= 35) * charmin,
                   cottonInc = household_income_cts * cotton,
                   cottonRate = rate * cotton,
                   cottonMultiFam = (type_of_residence == "Multi-Family") * cotton,
                   cottonMobile = (type_of_residence == "Mobile") * cotton,
                   cottonCollege = college * cotton,
                   cottonMonthStart = monthStart * cotton,
                   cottonWhite = white * cotton,
                   cottonMonthStartPoor = monthStart * (household_income_cts <= 35) * cotton,
                   otherInc = household_income_cts * other,
                   otherRate = rate * other,
                   otherMultiFam = (type_of_residence == "Multi-Family") * other,
                   otherMobile = (type_of_residence == "Mobile") * other,
                   otherCollege = college * other,
                   otherMonthStart = monthStart * other,
                   otherWhite = white * other,
                   otherMonthStartPoor = monthStart * (household_income_cts <= 35) * other,
                   qnInc = household_income_cts * qn,
                   qnRate = rate * qn,
                   qnMultiFam = (type_of_residence == "Multi-Family") * qn,
                   qnMobile = (type_of_residence == "Mobile") * qn,
                   qnCollege = college * qn,
                   qnMonthStart = monthStart * qn,
                   qnWhite = white * qn,
                   qnMonthStartPoor = monthStart * (household_income_cts <= 35) * qn,
                   scottInc = household_income_cts * scott,
                   scottRate = rate * scott,
                   scottMultiFam = (type_of_residence == "Multi-Family") * scott,
                   scottMobile = (type_of_residence == "Mobile") * scott,
                   scottCollege = college * scott,
                   scottMonthStart = monthStart * scott,
                   scottWhite = white * scott,
                   scottMonthStartPoor = monthStart * (household_income_cts <= 35) * scott)]

  # topShoppers <- fullChoice[, uniqueN(trip_code_uc), by = .(household_code, market)]
  # setorder(topShoppers, -V1)
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

  # Regular MNL
  reg1 <- mlogit(choice ~ p + as.factor(brand) + size + small + large | 0,
                 data = focusM, weights = focusSet$projection_factor)

  # Mixed Logit (no observed heterogeneity and fixing brand coefs and allowing correlation)
  reg2 <- mlogit(choice ~ p + size + small + large + as.factor(brand) | 0,
                 data = focusM, weights = focusSet$projection_factor,
                 rpar = c(p = "n"),
                 R = draws, halton = NA, panel = TRUE)
  reg3 <- mlogit(choice ~ p + size + small + large + as.factor(brand) | 0,
                 data = focusM, weights = focusSet$projection_factor,
                 rpar = c(p = "n", size = "n", small = "n", large = "n"),
                 R = draws, halton = NA, panel = TRUE, correlation = TRUE)

  # Mixed Logit (no observed heterogeneity and fixing price coef)
  reg4 <- mlogit(choice ~ p + size + small + large + charmin + cotton + other + qn + scott | 0,
                  data = focusM, weights = focusSet$projection_factor,
                  rpar = c(size = "n", small = "n", large = "n",
                           charmin = "n", cotton = "n", other = "n", qn = "n", scott = "n"),
                  R = draws, halton = NA, panel = TRUE, correlation = TRUE)

  # Mixed Logit (all random)
  reg5 <- mlogit(choice ~ p + size + small + large + charmin + cotton + other + qn + scott | 0,
                  data = focusM, weights = focusSet$projection_factor,
                  rpar = c(p = "n", size = "n", small = "n", large = "n",
                           charmin = "n", cotton = "n", other = "n", qn = "n", scott = "n"),
                  R = draws, halton = NA, panel = TRUE, correlation = TRUE)
  print(rpar(reg5))
  stargazer(reg1, reg2, reg3, reg4, reg5, type = "text")

  # Revelt and Train (1998) table
  stargazer(reg1, reg5, type = "text",
            title = paste0("Standard and Mixed Logit Estimation with Normal Coefficients: ", city),
            omit = c("charmin", "scott", "qn", "other", "cotton", "^as.factor*"),
            out.header = FALSE,
            notes.align = "l",
            column.labels = c("Standard Logit", "Mixed Logit"),
            dep.var.caption = "", dep.var.labels = "", model.numbers = FALSE,
            order = c("p$", "size$", "small$", "large$"),
            covariate.labels = c("Price", "Price (SD)",
                                 "Std. Rolls", "Std. Rolls (SD)",
                                 "Small Size", "Small Size (SD)",
                                 "Large Size", "Large Size (SD)"),
            no.space = TRUE, single.row = FALSE, digits = 2,
            label = paste0("tab:mnl", city, "Baseline"),
            notes = c("Standard rolls are defined as 225-sheet, 2-ply equivalents.",
                      "Actual rolls are the number of rolls marketed in the package."),
            add.lines = list(c("Brand FE", "Y", "Y")),
            out = paste0("/home/upenn/hossaine/tables/mnl", city, "Baseline.tex"))

  # Mixed Logit with Observed Heterogeneity (with correlation)
  reg6 <- mlogit(choice ~ p +
                    pInc + pRate + pMultiFam + pMobile + pCollege +
                    pMonthStart + pWhite + pMonthStartPoor +
                    size + small + large + charmin + cotton + other + qn + scott | 0,
                  data = focusM, weights = focusSet$projection_factor,
                  rpar = c(size = "n", small = "n", large = "n",
                           charmin = "n", cotton = "n", other = "n", qn = "n", scott = "n"),
                  R = draws, halton = NA, panel = TRUE, correlation = TRUE)
  reg7 <- mlogit(choice ~ p +
                    pInc + pRate + pMultiFam + pMobile + pCollege + pMonthStart +
                    pWhite + pMonthStartPoor +
                    size +
                    sizeInc + sizeRate + sizeMultiFam + sizeMobile + sizeCollege +
                    sizeMonthStart + sizeWhite + sizeMonthStartPoor +
                    small + large + charmin + cotton + other + qn + scott | 0,
                  data = focusM, weights = focusSet$projection_factor,
                  rpar = c(size = "n", small = "n", large = "n",
                           charmin = "n", cotton = "n", other = "n", qn = "n", scott = "n"),
                  R = draws, halton = NA, panel = TRUE, correlation = TRUE)
  reg8 <- mlogit(choice ~ p +
                    pInc + pRate + pMultiFam + pMobile + pCollege + pMonthStart +
                    pWhite + pMonthStartPoor +
                    size +
                    sizeInc + sizeRate + sizeMultiFam + sizeMobile +
                    sizeCollege + sizeMonthStart + sizeWhite + sizeMonthStartPoor +
                    small +
                    smallInc + smallRate + smallMultiFam + smallMobile +
                    smallCollege + smallMonthStart + smallWhite + smallMonthStartPoor +
                    large + charmin + cotton + other + qn + scott | 0,
                  data = focusM, weights = focusSet$projection_factor,
                  rpar = c(size = "n", small = "n", large = "n",
                           charmin = "n", cotton = "n", other = "n", qn = "n", scott = "n"),
                  R = draws, halton = NA, panel = TRUE, correlation = TRUE)
  reg9 <- mlogit(choice ~ p +
                    pInc + pRate + pMultiFam + pMobile + pCollege + pMonthStart +
                    pWhite + pMonthStartPoor +
                    size +
                    sizeInc + sizeRate + sizeMultiFam + sizeMobile +
                    sizeCollege + sizeMonthStart + sizeWhite + sizeMonthStartPoor +
                    small +
                    smallInc + smallRate + smallMultiFam + smallMobile +
                    smallCollege + smallMonthStart + smallWhite + smallMonthStartPoor +
                    large +
                   largeInc + largeRate + largeMultiFam + largeMobile +
                    largeCollege + largeMonthStart + largeWhite + largeMonthStartPoor +
                    charmin + cotton + other + qn + scott | 0,
                  data = focusM, weights = focusSet$projection_factor,
                  rpar = c(size = "n", small = "n", large = "n",
                           charmin = "n", cotton = "n", other = "n", qn = "n", scott = "n"),
                  R = draws, halton = NA, panel = TRUE, correlation = TRUE)
  print(rpar(reg9))
  stargazer(reg1, reg5, reg6, reg7, reg8, reg9, type = "text")

  # Mixed Logit (adding brands and then prices)
  reg10 <- mlogit(choice ~ p +
                    pInc + pRate + pMultiFam + pMobile + pCollege + pMonthStart +
                    pWhite + pMonthStartPoor +
                    size +
                    sizeInc + sizeRate + sizeMultiFam + sizeMobile +
                    sizeCollege + sizeMonthStart + sizeWhite + sizeMonthStartPoor +
                    small +
                    smallInc + smallRate + smallMultiFam + smallMobile +
                    smallCollege + smallMonthStart + smallWhite + smallMonthStartPoor +
                    large +
                    largeInc + largeRate + largeMultiFam + largeMobile +
                    largeCollege + largeMonthStart + largeWhite + largeMonthStartPoor +
                    charmin +
                    charminInc + charminRate + charminMultiFam + charminMobile +
                    charminCollege + charminMonthStart + charminWhite + charminMonthStartPoor +
                    cotton + other + qn + scott | 0,
                  data = focusM, weights = focusSet$projection_factor,
                  rpar = c(size = "n", small = "n", large = "n",
                           charmin = "n", cotton = "n", other = "n", qn = "n", scott = "n"),
                  R = draws, halton = NA, panel = TRUE, correlation = TRUE)

  reg11 <- mlogit(choice ~ p +
                    pInc + pRate + pMultiFam + pMobile + pCollege + pMonthStart +
                    pWhite + pMonthStartPoor +
                    size +
                    sizeInc + sizeRate + sizeMultiFam + sizeMobile +
                    sizeCollege + sizeMonthStart + sizeWhite + sizeMonthStartPoor +
                    small +
                    smallInc + smallRate + smallMultiFam + smallMobile +
                    smallCollege + smallMonthStart + smallWhite + smallMonthStartPoor +
                    large +
                    largeInc + largeRate + largeMultiFam + largeMobile +
                    largeCollege + largeMonthStart + largeWhite + largeMonthStartPoor +
                    charmin +
                    charminInc + charminRate + charminMultiFam + charminMobile +
                    charminCollege + charminMonthStart + charminWhite + charminMonthStartPoor +
                    cotton +
                    cottonInc + cottonRate + cottonMultiFam + cottonMobile +
                    cottonCollege + cottonMonthStart + cottonWhite + cottonMonthStartPoor +
                    other + qn + scott | 0,
                  data = focusM, weights = focusSet$projection_factor,
                  rpar = c(size = "n", small = "n", large = "n",
                           charmin = "n", cotton = "n", other = "n", qn = "n", scott = "n"),
                  R = draws, halton = NA, panel = TRUE, correlation = TRUE)

  reg12 <- mlogit(choice ~ p +
                    pInc + pRate + pMultiFam + pMobile + pCollege + pMonthStart +
                    pWhite + pMonthStartPoor +
                    size +
                    sizeInc + sizeRate + sizeMultiFam + sizeMobile +
                    sizeCollege + sizeMonthStart + sizeWhite + sizeMonthStartPoor +
                    small +
                    smallInc + smallRate + smallMultiFam + smallMobile +
                    smallCollege + smallMonthStart + smallWhite + smallMonthStartPoor +
                    large +
                    largeInc + largeRate + largeMultiFam + largeMobile +
                    largeCollege + largeMonthStart + largeWhite + largeMonthStartPoor +
                    charmin +
                    charminInc + charminRate + charminMultiFam + charminMobile +
                    charminCollege + charminMonthStart + charminWhite + charminMonthStartPoor +
                    cotton +
                    cottonInc + cottonRate + cottonMultiFam + cottonMobile +
                    cottonCollege + cottonMonthStart + cottonWhite + cottonMonthStartPoor +
                    other +
                    otherInc + otherRate + otherMultiFam + otherMobile +
                    otherCollege + otherMonthStart + otherWhite + otherMonthStartPoor +
                    qn + scott | 0,
                  data = focusM, weights = focusSet$projection_factor,
                  rpar = c(size = "n", small = "n", large = "n",
                           charmin = "n", cotton = "n", other = "n", qn = "n", scott = "n"),
                  R = draws, halton = NA, panel = TRUE, correlation = TRUE)

  reg13 <- mlogit(choice ~ p +
                    pInc + pRate + pMultiFam + pMobile + pCollege + pMonthStart +
                    pWhite + pMonthStartPoor +
                    size +
                    sizeInc + sizeRate + sizeMultiFam + sizeMobile +
                    sizeCollege + sizeMonthStart + sizeWhite + sizeMonthStartPoor +
                    small +
                    smallInc + smallRate + smallMultiFam + smallMobile +
                    smallCollege + smallMonthStart + smallWhite + smallMonthStartPoor +
                    large +
                    largeInc + largeRate + largeMultiFam + largeMobile +
                    largeCollege + largeMonthStart + largeWhite + largeMonthStartPoor +
                    charmin +
                    charminInc + charminRate + charminMultiFam + charminMobile +
                    charminCollege + charminMonthStart + charminWhite + charminMonthStartPoor +
                    cotton +
                    cottonInc + cottonRate + cottonMultiFam + cottonMobile +
                    cottonCollege + cottonMonthStart + cottonWhite + cottonMonthStartPoor +
                    other +
                    otherInc + otherRate + otherMultiFam + otherMobile +
                    otherCollege + otherMonthStart + otherWhite + otherMonthStartPoor +
                    qn +
                    qnInc + qnRate + qnMultiFam + qnMobile +
                    qnCollege + qnMonthStart + qnWhite + qnMonthStartPoor +
                    scott | 0,
                  data = focusM, weights = focusSet$projection_factor,
                  rpar = c(size = "n", small = "n", large = "n",
                           charmin = "n", cotton = "n", other = "n", qn = "n", scott = "n"),
                  R = draws, halton = NA, panel = TRUE, correlation = TRUE)

  reg14 <- mlogit(choice ~ p +
                    pInc + pRate + pMultiFam + pMobile + pCollege + pMonthStart +
                    pWhite + pMonthStartPoor +
                    size +
                    sizeInc + sizeRate + sizeMultiFam + sizeMobile +
                    sizeCollege + sizeMonthStart + sizeWhite + sizeMonthStartPoor +
                    small +
                    smallInc + smallRate + smallMultiFam + smallMobile +
                    smallCollege + smallMonthStart + smallWhite + smallMonthStartPoor +
                    large +
                    largeInc + largeRate + largeMultiFam + largeMobile +
                    largeCollege + largeMonthStart + largeWhite + largeMonthStartPoor +
                    charmin +
                    charminInc + charminRate + charminMultiFam + charminMobile +
                    charminCollege + charminMonthStart + charminWhite + charminMonthStartPoor +
                    cotton +
                    cottonInc + cottonRate + cottonMultiFam + cottonMobile +
                    cottonCollege + cottonMonthStart + cottonWhite + cottonMonthStartPoor +
                    other +
                    otherInc + otherRate + otherMultiFam + otherMobile +
                    otherCollege + otherMonthStart + otherWhite + otherMonthStartPoor +
                    qn +
                    qnInc + qnRate + qnMultiFam + qnMobile +
                    qnCollege + qnMonthStart + qnWhite + qnMonthStartPoor +
                    scott +
                    scottInc + scottRate + scottMultiFam + scottMobile +
                    scottCollege + scottMonthStart + scottWhite + scottMonthStartPoor | 0,
                  data = focusM, weights = focusSet$projection_factor,
                  rpar = c(size = "n", small = "n", large = "n",
                           charmin = "n", cotton = "n", other = "n", qn = "n", scott = "n"),
                  R = draws, halton = NA, panel = TRUE, correlation = TRUE)

  reg15 <- mlogit(choice ~ p +
                    pInc + pRate + pMultiFam + pMobile + pCollege + pMonthStart +
                    pWhite + pMonthStartPoor +
                    size +
                    sizeInc + sizeRate + sizeMultiFam + sizeMobile +
                    sizeCollege + sizeMonthStart + sizeWhite + sizeMonthStartPoor +
                    small +
                    smallInc + smallRate + smallMultiFam + smallMobile +
                    smallCollege + smallMonthStart + smallWhite + smallMonthStartPoor +
                    large +
                    largeInc + largeRate + largeMultiFam + largeMobile +
                    largeCollege + largeMonthStart + largeWhite + largeMonthStartPoor +
                    charmin +
                    charminInc + charminRate + charminMultiFam + charminMobile +
                    charminCollege + charminMonthStart + charminWhite + charminMonthStartPoor +
                    cotton +
                    cottonInc + cottonRate + cottonMultiFam + cottonMobile +
                    cottonCollege + cottonMonthStart + cottonWhite + cottonMonthStartPoor +
                    other +
                    otherInc + otherRate + otherMultiFam + otherMobile +
                    otherCollege + otherMonthStart + otherWhite + otherMonthStartPoor +
                    qn +
                    qnInc + qnRate + qnMultiFam + qnMobile +
                    qnCollege + qnMonthStart + qnWhite + qnMonthStartPoor +
                    scott +
                    scottInc + scottRate + scottMultiFam + scottMobile +
                    scottCollege + scottMonthStart + scottWhite + scottMonthStartPoor | 0,
                  data = focusM, weights = focusSet$projection_factor,
                  rpar = c(p = "n", size = "n", small = "n", large = "n",
                           charmin = "n", cotton = "n", other = "n", qn = "n", scott = "n"),
                  R = draws, halton = NA, panel = TRUE, correlation = TRUE)
  save(reg15, file = paste0("/home/upenn/hossaine/regs/", city, ".rda"))

  print(rpar(reg15))
  stargazer(reg5, reg9, reg10, reg11, reg12, reg13, reg14, reg15, type = "text")

  # # MNL Regression Table
  # stargazer(reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8, reg9, reg10, type = "text")
  # ,
  #           title = paste0("Multinomial Logit Estimation: ", city),
  #           omit = "^as.factor*",
  #           out.header = FALSE,
  #           notes.align = "l",
  #           covariate.labels = c("Price", "Std. Rolls", "Actual Rolls"),
  #           order = c("p$", "size", "pkgSize"),
  #           no.space = TRUE, single.row = FALSE, digits = 2,
  #           label = paste0("tab:mnl", city, "Baseline"),
  #           notes = c("Standard rolls are defined as 225-sheet, 2-ply equivalents.",
  #                     "Actual rolls are the number of rolls marketed in the package."),
  #           add.lines = list(c("Brand FE", "N", "N", "N", "Y")),
  #           out = paste0("/home/upenn/hossaine/tables/mnl", city, "Baseline.tex"))
}

getMNL("Boston")
getMNL("Dallas")
getMNL("Philadelphia")
getMNL("Los Angeles")
getMNL("Chicago")
getMNL("Phoenix")
getMNL("Minneapolis")
getMNL("Columbus")
getMNL("Charlotte")
getMNL("Denver")

############################### WTP ###########################################
# Plots distribution of WTP given a regression, demographics, and parameter
###############################################################################
getWTP <- function(coefP, sdP, coefPar, sdPar, dems, nDraws = 10000,
                   inputTitle = NULL, xrange = NULL, yrange = NULL) {
  monthStartPoor <- ifelse(dems["monthStart"] == 1 & dems["income"] < 35, 1L, 0L)
  fullDems <- c(1, dems, monthStartPoor)
  muP <- sum(fullDems * coefP)
  muPar <- sum(fullDems * coefPar)

  pDraw <- rnorm(nDraws, muP, sdP)
  parDraw <- rnorm(nDraws, muPar, sdPar)
  wtp <- data.table(wtpNum = - parDraw / pDraw)
  numLabel <- median(wtp$wtpNum)
  chart <- ggplot(data = wtp, aes(x = wtpNum)) +
    geom_density() +
    scale_x_continuous(limits = xrange) +
    scale_y_continuous(limits = yrange) +
    labs(title = inputTitle) +
    geom_text(aes(label = round(numLabel, 2), y = yrange[2] - 1, x = numLabel)) +
    theme_fivethirtyeight()
  return(chart)
}

coefP <- c(p = -0.576,
           pInc = 0.001,
           pRate = 0.141,
           pMultiFam = 0.122,
           pMobile = -0.345,
           pCollege = 0.039,
           pMonthStart = 0.057,
           pWhite = 0.168,
           pMonstStartPoor = 0)
sdP <- 0.139
coefSize <- c(coef = 0,
             coefInc = 0.0004,
             coefRate = 0.149,
             coefMultiFam = -0.058,
             coefMobile = 0.124,
             coefCollege = -0.019,
             coefMonthStart = -0.028,
             coefWhite = -0.016,
             coefMonthStartPoor = 0)
sdSize <- 0.039

coefSmall <- c(coef = -2.134,
              coefInc = -0.004,
              coefRate = 0.883,
              coefMultiFam = 0.149,
              coefMobile = 0.553,
              coefCollege = -0.079,
              coefMonthStart = 0.138,
              coefWhite = 1.161,
              coefMonthStartPoor = 0)
sdSmall <- 0.583

coefLarge <- c(coef = -1.110,
               coefInc = 0,
               coefRate = 0,
               coefMultiFam = 0.182,
               coefMobile = -1.264,
               coefCollege = 0,
               coefMonthStart = 0.334,
               coefWhite = 0.196,
               coefMonthStartPoor = 0)
sdLarge <- 0.46

dems <- matrix(c(income = c(25, 50, 75, 100),
                 rate = rep(0.25, 4),
                 multifam = rep(0, 4),
                 mobile = rep(0, 4),
                 college = rep(1, 4),
                 monthStart = rep(0, 4),
                 white = rep(1, 4)), ncol = 4, byrow = TRUE,
               dimnames = list(c("income", "rate", "multifam", "mobile", "college",
                            "monthStart", "white")))
titles <- c("25k", "50k", "75k", "100k")
for (i in 1:ncol(dems)) {
  set.seed(121)
  print(getWTP(coefP, sdP, coefPar = coefSize, sdPar = sdSize, dems[, i],
               nDraws = 10000, inputTitle = titles[i], xrange = c(-5, 5), yrange = c(0, 4)))
}


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
                        monthStartPoor = monthStart * (household_income_cts < 35))]
newP <- as.matrix(customP) %*% as.matrix(pCoefs, ncol = 1)
newP <- newP[focusSet$choice == 1]
surplus <- logsum(reg15) / -newP

# Combining surplus with each trip and plotting the 2 groups
tripSurplus <- data.table(trip_code_uc = unique(focusSet$trip_code_uc),
                          surplus = surplus)
tripSurplus <- merge(tripSurplus, unique(focusSet[, .(trip_code_uc, household_income_cts)]),
                     by = "trip_code_uc")
plot(quantile(tripSurplus[household_income_cts < 30]$surplus, seq(0, 1, 0.01)),
     quantile(tripSurplus[household_income_cts > 80]$surplus, seq(0, 1, 0.01)))
abline(a = 0, b = 1)
