# Running multinomial models
library(mlogit, lib.loc = "~/lib/R")
library(data.table)
library(stargazer)
library(ggplot2)
library(ggthemes)
threads <- 8

fullChoice <- fread("/scratch/upenn/hossaine/TPMLogit.csv", nThread = threads)

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
fullChoice[, ':=' (piRatio = p / household_income_cts,
                   daySupply = size / rate)]

############################ TESTING ###########################################
# Focusing only on Boston for testing
focusSet <- fullChoice[market == "Dallas"]

# Running MNL regression
focusM <- mlogit.data(data = focusSet, choice = "choice", shape = "long",
                      alt.var = "alt", chid.var = "trip_code_uc",
                      id.var = "household_code", opposite = c("unitCost", "piRatio"))
start <- proc.time()
reg1 <- mlogit(choice ~ unitCost + brand + sizeCat | 0, data = focusM)
reg2 <- mlogit(choice ~ unitCost + brand + sizeCat + piRatio | 0, data = focusM)
reg3 <- mlogit(choice ~ unitCost + brand + sizeCat + piRatio + daySupply | 0, data = focusM)
proc.time() - start

stargazer(reg1, reg2, reg3, type = "text",
          out.header = FALSE,
          notes.align = "l",
          covariate.labels = c("Unit Cost (-)",
                               "Charmin", "Cottonelle", "Qltd Ntn", "Scott", "Other",
                               "Small Size", "Medium Size",
                               "Price/Income Ratio", "Day Supply"),
          order = c(1:4, 6, 7, 5, 9, 8, 10, 11),
          no.space = TRUE,
          single.row = TRUE,
          digits = 2,
          label = "tab:mnlBostonBaseline",
          notes = c("Small size is less than 6 rolls. Medium size is 7-12 rolls. ",
                    "Days' supply is the number of standardized rolls in a package",
                    "divided by a household's average daily consumption."),
          out = "./tables/mnlBostonBaseline.tex")

# Running random coefficients specification
start <- proc.time()
reg4 <- mlogit(choice ~ p + unitCost + brand + sizeCat + piRatio + daySupply | 0, data = focusM,
               rpar = c(p = "ln"), R = 25, halton = NA)
reg5 <- mlogit(choice ~ p + unitCost + brand + sizeCat + piRatio + daySupply | 0, data = focusM,
               rpar = c(p = "ln", unitCost = "ln"), R = 25, halton = NA)
reg6 <- mlogit(choice ~ p + unitCost + brand + sizeCat + piRatio + daySupply | 0, data = focusM,
               rpar = c(p = "ln", unitCost = "ln", daySupply = "n"), R = 25, halton = NA)
proc.time() - start

stargazer(reg4, reg5, reg6, type = "text",
          out.header = FALSE,
          notes.align = "l",
          # covariate.labels = c("Unit Cost",
          #                      "Charmin", "Cottonelle", "Qltd Ntn", "Scott", "Other",
          #                      "Small Size", "Medium Size",
          #                      "Price/Income Ratio", "Day Supply"),
          # order = c(1:3, 5, 6, 4, 8, 7, 9, 10),
          digits = 2,
          no.space = TRUE,
          single.row = TRUE,
          label = "tab:mnlBostonRand",
          notes = c("Pack size is defined number of rolls in a package. Days' ",
                    "supply is the number of standardized rolls in a package",
                    "divided by a household's average daily consumption."),
          out = "./tables/mnlBostonRand.tex")
