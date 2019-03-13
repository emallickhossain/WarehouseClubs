# Running multinomial models
library(mlogit, lib.loc = "~/lib/R")
library(data.table)
library(stargazer)
library(ggplot2)
library(ggthemes)
library(fmsb)
threads <- 8

fullChoice <- fread("/scratch/upenn/hossaine/TPMLogit.csv", nThread = threads)

# # Looking at price variance
# fullChoice[, "meanPrice" := mean(p), by = .(topBrand, sizeCat, store_code_uc, week_end)]
# fullChoice[, "meanDiff" := (p - meanPrice) / meanPrice * 100]
# ggplot(data = fullChoice, aes(x = meanDiff, y = ..density..)) +
#   geom_histogram(binwidth = 1) +
#   scale_x_continuous(limits = c(-100, 100)) +
#   theme_fivethirtyeight()

# Collapsing products that are not uniquely defined by a brand-size
fullChoice <- na.omit(fullChoice, cols = "size")
fullChoice <- fullChoice[, .(size = mean(size),
                             unitCost = mean(unitCost),
                             choice = sum(choice),
                             p = mean(p)),
                         by = .(alt, topBrand, sizeCat,
                                household_code, panel_year, trip_code_uc,
                                store_code_uc, week_end,
                                projection_factor, household_income,
                                household_size, type_of_residence, marital_status,
                                hispanic_origin, market, age, college, white,
                                child, household_income_cts, rate)]
Purch <- fullChoice[, .(choice = sum(choice)), by = trip_code_uc][choice == 1]$trip_code_uc
fullChoice <- fullChoice[trip_code_uc %in% Purch]

brandKey <- data.table(brand = c("Angel Soft", "Charmin", "Other", "Cottonelle",
                                 "Qltd Ntn", "Scott"),
                       topBrand = c(506045, 526996, 536746, 581898, 624459, 635074))
fullChoice <- merge(fullChoice, brandKey, by = "topBrand")
fullChoice[, ':=' (small = (sizeCat == "small"),
                   medium = (sizeCat == "medium"),
                   charmin = (brand == "Charmin"),
                   other = (brand == "Other"),
                   cotton = (brand == "Cottonelle"),
                   qn = (brand == "Qltd Ntn"),
                   scott = (brand == "Scott"))]

altKey <- unique(fullChoice[, .(alt, brand, sizeCat)])
setorder(altKey, alt)
altKey[, "sizeCat" := toupper(substr(sizeCat, 1, 1))]
altKey[, "brandSize" := paste(brand, sizeCat, sep = "-")]

# Testing for multicollinearity
## Condition Number test (<30 is good)
reg1 <- lm(data = fullChoice, size ~ sizeCat + unitCost)
VIF(reg1)

# Adding in demographic and product characteristic interactions for observed preference heterogeneity
fullChoice[, ':=' (unitCostInc = household_income_cts * unitCost,
                   sizeInc = household_income_cts * size,
                   smallInc = household_income_cts * small,
                   medInc = household_income_cts * medium,
                   charInc = household_income_cts * charmin,
                   otherInc = household_income_cts * other,
                   cottonInc = household_income_cts * cotton,
                   qnInc = household_income_cts * qn,
                   scottInc = household_income_cts * scott)]

fullChoice[, ':=' (unitCostHHSize = household_size * unitCost,
                   sizeHHSize = household_size * size,
                   smallHHSize = household_size * small,
                   medHHSize = household_size * medium,
                   charHHSize = household_size * charmin,
                   otherHHSize = household_size * other,
                   cottonHHSize = household_size * cotton,
                   qnHHSize = household_size * qn,
                   scottHHSize = household_size * scott)]

fullChoice[, ':=' (unitCostRate = rate * unitCost,
                   sizeRate = rate * size,
                   smallRate = rate * small,
                   medRate = rate * medium,
                   charRate = rate * charmin,
                   otherRate = rate * other,
                   cottonRate = rate * cotton,
                   qnRate = rate * qn,
                   scottRate = rate * scott)]

fullChoice[, ':=' (unitCostMultiFam = (type_of_residence == "Multi-Family") * unitCost,
                   sizeMultiFam = (type_of_residence == "Multi-Family") * size,
                   smallMultiFam = (type_of_residence == "Multi-Family") * small,
                   medMultiFam = (type_of_residence == "Multi-Family") * medium,
                   charMultiFam = (type_of_residence == "Multi-Family") * charmin,
                   otherMultiFam = (type_of_residence == "Multi-Family") * other,
                   cottonMultiFam = (type_of_residence == "Multi-Family") * cotton,
                   qnMultiFam = (type_of_residence == "Multi-Family") * qn,
                   scottMultiFam = (type_of_residence == "Multi-Family") * scott)]

fullChoice[, ':=' (unitCostMobile = (type_of_residence == "Mobile") * unitCost,
                   sizeMobile = (type_of_residence == "Mobile") * size,
                   smallMobile = (type_of_residence == "Mobile") * small,
                   medMobile = (type_of_residence == "Mobile") * medium,
                   charMobile = (type_of_residence == "Mobile") * charmin,
                   otherMobile = (type_of_residence == "Mobile") * other,
                   cottonMobile = (type_of_residence == "Mobile") * cotton,
                   qnMobile = (type_of_residence == "Mobile") * qn,
                   scottMobile = (type_of_residence == "Mobile") * scott)]

############################ TESTING ###########################################
getMNL <- function(city) {
  if(city == "All") {
    focusSet <- fullChoice
  } else {
    focusSet <- fullChoice[market == city]
  }

  # Running MNL regression
  focusM <- mlogit.data(data = focusSet, choice = "choice", shape = "long",
                        alt.var = "alt", chid.var = "trip_code_uc",
                        id.var = "household_code", opposite = c("unitCost"))
  start <- proc.time()
  reg1 <- mlogit(choice ~ unitCost | 0, data = focusM)
  reg2 <- mlogit(choice ~ unitCost + charmin + other + cotton + qn + scott | 0, data = focusM)
  reg3 <- mlogit(choice ~ unitCost + charmin + other + cotton + qn + scott + size | 0, data = focusM)
  reg4 <- mlogit(choice ~ unitCost + charmin + other + cotton + qn + scott +
                   size + small + medium | 0, data = focusM)
  proc.time() - start

  # MNL Regression Table
  stargazer(reg1, reg2, reg3, reg4, type = "text",
            out.header = FALSE,
            notes.align = "l",
            covariate.labels = c("Unit Cost (-)",
                                 "Charmin", "Cottonelle", "Qltd Ntn", "Scott", "Other",
                                 "Std. Rolls", "Small Size", "Medium Size"),
            order = c(1:3, 5, 6, 4, 7, 9, 8),
            no.space = TRUE, single.row = TRUE, digits = 2,
            label = paste0("tab:mnl", city, "Baseline"),
            notes = c("Small size is less than 6 rolls. Medium size is 7-12 rolls. ",
                      "Days' supply is the number of standardized rolls in a package",
                      "divided by a household's average daily consumption."),
            out = paste0("./tables/mnl", city, "Baseline.tex"))

  # Marginal effects table
  effectDat <- focusSet[, .(unitCost = mean(unitCost),
                            size = mean(size)), by = .(alt, brand, sizeCat)]
  mnlEffects <- effects(object = reg4, covariate = "unitCost", type = "rr", data = effectDat)
  colnames(mnlEffects) <- altKey$brandSize
  rownames(mnlEffects) <- altKey$brandSize
  stargazer(-mnlEffects, type = "text", digits = 3,
            title = paste0("Unit Cost Elasticity (", city, ")"), summary = FALSE,
            colnames = TRUE, rownames = TRUE,
            label = paste0("tab:mnlEffects", city),
            notes = c("Entry (i, j) corresponds to percent change in share of product in column j in response to a 1 percent change in the unit price ",
                      "of the product in row i. Computed at the mean unit cost and standard rolls."),
            out = paste0("/home/upenn/hossaine/tables/mnlEffects", city, ".tex"))
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
getMNL("All")

getMNLObsHet <- function(city) {
  focusSet <- fullChoice[market == city]

  # Running MNL regression
  focusM <- mlogit.data(data = focusSet, choice = "choice", shape = "long",
                        alt.var = "alt", chid.var = "trip_code_uc",
                        id.var = "household_code",
                        opposite = c("unitCost", "unitCostInc", "unitCostHHSize",
                                     "unitCostRate", "unitCostMultiFam", "unitCostMobile"))
  start <- proc.time()
  reg0 <- mlogit(choice ~ unitCost + brand + size + sizeCat | 0, data = focusM)
  reg1 <- mlogit(choice ~ unitCost +
                   unitCostInc + unitCostHHSize + unitCostRate + unitCostMultiFam + unitCostMobile +
                   brand + size + sizeCat | 0, data = focusM)
  reg2 <- mlogit(choice ~ unitCost +
                   unitCostInc + unitCostHHSize + unitCostRate + unitCostMultiFam + unitCostMobile +
                   brand +
                   charInc + charHHSize + charRate + charMultiFam + charMobile +
                   otherInc + otherHHSize + otherRate + otherMultiFam + otherMobile +
                   cottonInc + cottonHHSize + cottonRate + cottonMultiFam + cottonMobile +
                   qnInc + qnHHSize + qnRate + qnMultiFam + qnMobile +
                   scottInc + scottHHSize + scottRate + scottMultiFam + scottMobile +
                   size + sizeCat | 0, data = focusM)
  reg3 <- mlogit(choice ~ unitCost +
                   unitCostInc + unitCostHHSize + unitCostRate + unitCostMultiFam + unitCostMobile +
                   brand +
                   charInc + charHHSize + charRate + charMultiFam + charMobile +
                   otherInc + otherHHSize + otherRate + otherMultiFam + otherMobile +
                   cottonInc + cottonHHSize + cottonRate + cottonMultiFam + cottonMobile +
                   qnInc + qnHHSize + qnRate + qnMultiFam + qnMobile +
                   scottInc + scottHHSize + scottRate + scottMultiFam + scottMobile +
                   size +
                   sizeCat +
                   smallInc + smallHHSize + smallRate + smallMultiFam + smallMobile +
                   medInc + medHHSize + medRate + medMultiFam + medMobile | 0, data = focusM)
  reg4 <- mlogit(choice ~ unitCost +
                   unitCostInc + unitCostHHSize + unitCostRate + unitCostMultiFam + unitCostMobile +
                   brand +
                   charInc + charHHSize + charRate + charMultiFam + charMobile +
                   otherInc + otherHHSize + otherRate + otherMultiFam + otherMobile +
                   cottonInc + cottonHHSize + cottonRate + cottonMultiFam + cottonMobile +
                   qnInc + qnHHSize + qnRate + qnMultiFam + qnMobile +
                   scottInc + scottHHSize + scottRate + scottMultiFam + scottMobile +
                   size +
                   sizeInc + sizeHHSize + sizeRate + sizeMultiFam + sizeMobile +
                   sizeCat +
                   smallInc + smallHHSize + smallRate + smallMultiFam + smallMobile +
                   medInc + medHHSize + medRate + medMultiFam + medMobile | 0, data = focusM)
  proc.time() - start

  stargazer(reg0, reg1, reg2, reg3, reg4, type = "text",
            out.header = FALSE,
            notes.align = "l",
            covariate.labels = c("Unit Cost (-)",
                                 "Unit Cost : Income", "Unit Cost : HH Size",
                                 "Unit Cost : Cons. Rate", "Unit Cost : MultiFam",
                                 "Unit Cost : Mobile",
                                 "Charmin",
                                 "Charmin : Income", "Charmin : HH Size",
                                 "Charmin : Cons. Rate", "Charmin : MultiFam",
                                 "Charmin : Mobile",
                                 "Cottonelle",
                                 "Cottonelle : Income", "Cottonelle : HH Size",
                                 "Cottonelle : Cons. Rate", "Cottonelle : MultiFam",
                                 "Cottonelle : Mobile",
                                 "Qltd Ntn",
                                 "Qltd Ntn : Income", "Qltd Ntn : HH Size",
                                 "Qltd Ntn : Cons. Rate", "Qltd Ntn : MultiFam",
                                 "Qltd Ntn : Mobile",
                                 "Scott",
                                 "Scott : Income", "Scott : HH Size",
                                 "Scott : Cons. Rate", "Scott : MultiFam",
                                 "Scott : Mobile",
                                 "Other",
                                 "Other : Income", "Other : HH Size",
                                 "Other : Cons. Rate", "Other : MultiFam",
                                 "Other : Mobile",
                                 "Small Size",
                                 "Small : Income", "Small : HH Size",
                                 "Small : Cons. Rate", "Small : MultiFam",
                                 "Small : Mobile",
                                 "Medium Size",
                                 "Medium : Income", "Medium : HH Size",
                                 "Medium : Cons. Rate", "Medium : MultiFam",
                                 "Medium : Mobile",
                                 "Std. Rolls",
                                 "Std. Rolls : Income", "Std. Rolls : HH Size",
                                 "Std. Rolls : Cons. Rate", "Std. Rolls : MultiFam",
                                 "Std. Rolls : Mobile"),
            order = c("^unitCost",
                      "brandCharmin", "^char*",
                      "brandCottonelle", "^cotton*",
                      "brandQltd Ntn", "^qn*",
                      "brandScott", "^scott*",
                      "brandOther", "^other*",
                      "sizeCatsmall", "^small*",
                      "sizeCatmedium", "^med*",
                      "^size"),
            no.space = TRUE,
            single.row = TRUE,
            digits = 2,
            label = paste0("tab:mnl", city, "ObsHet"),
            notes = c("Small size is less than 6 rolls. Medium size is 7-12 rolls. ",
                      "Days' supply is the number of standardized rolls in a package",
                      "divided by a household's average daily consumption."),
            out = paste0("./tables/mnl", city, "ObsHet.tex"))
}

getMNLObsHet("Boston")
getMNLObsHet("Dallas")
getMNLObsHet("Philadelphia")
getMNLObsHet("Los Angeles")
getMNLObsHet("Chicago")
getMNLObsHet("Phoenix")
getMNLObsHet("Minneapolis")
getMNLObsHet("Columbus")
getMNLObsHet("Charlotte")
getMNLObsHet("Denver")

# Random coefs
getRand <- function(city) {
  focusSet <- fullChoice[market == city & panel_year %in% 2016]

  # Running MNL regression
  focusM <- mlogit.data(data = focusSet, choice = "choice", shape = "long",
                        alt.var = "alt", chid.var = "trip_code_uc",
                        id.var = "household_code",
                        opposite = c("unitCost", "unitCostInc", "unitCostHHSize",
                                     "unitCostRate", "unitCostMultiFam", "unitCostMobile"))
  reg1 <- mlogit(choice ~ unitCost +
                   unitCostInc + unitCostHHSize + unitCostRate + unitCostMultiFam + unitCostMobile +
                   brand + size + sizeCat | 0, data = focusM,
                 rpar = c(unitCost = "ln"), R = 25, halton = NA, panel = TRUE)
  reg2 <- mlogit(choice ~ unitCost +
                   unitCostInc + unitCostHHSize + unitCostRate + unitCostMultiFam + unitCostMobile +
                   charmin + charInc + charHHSize + charRate + charMultiFam + charMobile +
                   other + otherInc + otherHHSize + otherRate + otherMultiFam + otherMobile +
                   cotton + cottonInc + cottonHHSize + cottonRate + cottonMultiFam + cottonMobile +
                   qn + qnInc + qnHHSize + qnRate + qnMultiFam + qnMobile +
                   scott + scottInc + scottHHSize + scottRate + scottMultiFam + scottMobile +
                   size + sizeCat | 0, data = focusM,
                 rpar = c(unitCost = "ln", charmin = "n", other = "n", cotton = "n",
                          qn = "n", scott = "n"), R = 25, halton = NA, panel = TRUE)
  reg3 <- mlogit(choice ~ unitCost +
                   unitCostInc + unitCostHHSize + unitCostRate + unitCostMultiFam + unitCostMobile +
                   charmin + charInc + charHHSize + charRate + charMultiFam + charMobile +
                   other + otherInc + otherHHSize + otherRate + otherMultiFam + otherMobile +
                   cotton + cottonInc + cottonHHSize + cottonRate + cottonMultiFam + cottonMobile +
                   qn + qnInc + qnHHSize + qnRate + qnMultiFam + qnMobile +
                   scott + scottInc + scottHHSize + scottRate + scottMultiFam + scottMobile +
                   size +
                   small + smallInc + smallHHSize + smallRate + smallMultiFam + smallMobile +
                   medium + medInc + medHHSize + medRate + medMultiFam + medMobile | 0, data = focusM,
                 rpar = c(unitCost = "ln", charmin = "n", other = "n", cotton = "n",
                          qn = "n", scott = "n", small = "n", medium = "n"),
                 R = 25, halton = NA, panel = TRUE)
  reg4 <- mlogit(choice ~ unitCost +
                   unitCostInc + unitCostHHSize + unitCostRate + unitCostMultiFam + unitCostMobile +
                   charmin + charInc + charHHSize + charRate + charMultiFam + charMobile +
                   other + otherInc + otherHHSize + otherRate + otherMultiFam + otherMobile +
                   cotton + cottonInc + cottonHHSize + cottonRate + cottonMultiFam + cottonMobile +
                   qn + qnInc + qnHHSize + qnRate + qnMultiFam + qnMobile +
                   scott + scottInc + scottHHSize + scottRate + scottMultiFam + scottMobile +
                   size + sizeInc + sizeHHSize + sizeRate + sizeMultiFam + sizeMobile +
                   small + smallInc + smallHHSize + smallRate + smallMultiFam + smallMobile +
                   medium + medInc + medHHSize + medRate + medMultiFam + medMobile | 0, data = focusM,
                 rpar = c(unitCost = "ln", charmin = "n", other = "n", cotton = "n",
                          qn = "n", scott = "n", small = "n", medium = "n", size = "n"),
                 R = 25, halton = NA, panel = TRUE)

  stargazer(reg0, reg1, reg2, reg3, reg4, type = "text",
            out.header = FALSE,
            notes.align = "l",
            covariate.labels = c("Unit Cost (-)",
                                 "Unit Cost : Income", "Unit Cost : HH Size",
                                 "Unit Cost : Cons. Rate", "Unit Cost : MultiFam",
                                 "Unit Cost : Mobile",
                                 "Charmin",
                                 "Charmin : Income", "Charmin : HH Size",
                                 "Charmin : Cons. Rate", "Charmin : MultiFam",
                                 "Charmin : Mobile",
                                 "Cottonelle",
                                 "Cottonelle : Income", "Cottonelle : HH Size",
                                 "Cottonelle : Cons. Rate", "Cottonelle : MultiFam",
                                 "Cottonelle : Mobile",
                                 "Qltd Ntn",
                                 "Qltd Ntn : Income", "Qltd Ntn : HH Size",
                                 "Qltd Ntn : Cons. Rate", "Qltd Ntn : MultiFam",
                                 "Qltd Ntn : Mobile",
                                 "Scott",
                                 "Scott : Income", "Scott : HH Size",
                                 "Scott : Cons. Rate", "Scott : MultiFam",
                                 "Scott : Mobile",
                                 "Other",
                                 "Other : Income", "Other : HH Size",
                                 "Other : Cons. Rate", "Other : MultiFam",
                                 "Other : Mobile",
                                 "Small Size",
                                 "Small : Income", "Small : HH Size",
                                 "Small : Cons. Rate", "Small : MultiFam",
                                 "Small : Mobile",
                                 "Medium Size",
                                 "Medium : Income", "Medium : HH Size",
                                 "Medium : Cons. Rate", "Medium : MultiFam",
                                 "Medium : Mobile",
                                 "Std. Rolls",
                                 "Std. Rolls : Income", "Std. Rolls : HH Size",
                                 "Std. Rolls : Cons. Rate", "Std. Rolls : MultiFam",
                                 "Std. Rolls : Mobile"),
            order = c("^unitCost",
                      "brandCharmin", "^char*",
                      "brandCottonelle", "^cotton*",
                      "brandQltd Ntn", "^qn*",
                      "brandScott", "^scott*",
                      "brandOther", "^other*",
                      "sizeCatsmall", "^small*",
                      "sizeCatmedium", "^med*",
                      "^size"),
            no.space = TRUE,
            single.row = TRUE,
            digits = 2,
            label = paste0("tab:mnl", city, "ObsHet"),
            notes = c("Small size is less than 6 rolls. Medium size is 7-12 rolls. ",
                      "Days' supply is the number of standardized rolls in a package",
                      "divided by a household's average daily consumption."),
            out = paste0("./tables/mnl", city, "ObsHet.tex"))
}

getRand("Boston")
getRand("Dallas")
getRand("Philadelphia")
getRand("Los Angeles")
getRand("Chicago")
getRand("Phoenix")
getRand("Minneapolis")
getRand("Columbus")
getRand("Charlotte")
getRand("Denver")
