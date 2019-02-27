# Running multinomial models
library(mlogit, lib.loc = "~/lib/R")
library(data.table)
library(stargazer)
library(gmnl)
threads <- 8

fullChoice <- fread("/scratch/upenn/hossaine/TPMLogit.csv", nThread = threads)

# Collapsing products that are not uniquely defined by a brand-size
fullChoice <- fullChoice[, .(ply = mean(ply),
                             size = mean(size),
                             unitCost = mean(unitCost),
                             choice = sum(choice),
                             p = mean(p)),
                         by = .(household_code, panel_year, trip_code_uc,
                                brand_code_uc, pkgSize, store_code_uc, week_end,
                                alt, projection_factor, household_income,
                                household_size, type_of_residence, marital_status,
                                hispanic_origin, market, age, college, white,
                                child, household_income_cts, rate)]

# Assigning choice id's
sizes <- c(4, 6, 12, 24)
brands <- c("Prv_Lbl", "Charmin", "Angel_Soft", "Qltd_Ntn", "Cottonelle", "Scott")
brandCode <- c(536746, 526996, 506045, 624459, 581898, 635074)
brandKey <- data.table(brands, brand_code_uc = brandCode)
fullSet <- expand.grid(brand = brands, pkgSize = sizes)
fullSet <- setDT(cbind(alt = 1:24, fullSet))
fullSet[, c("brandSize", "brand", "pkgSize") := .(paste0(brand, pkgSize), NULL, NULL)]
fullChoice <- merge(fullChoice, fullSet, by = "alt")
fullChoice <- merge(fullChoice, brandKey, by = "brand_code_uc")

# Adding in days' supply for each product based on HH consumption rate
fullChoice[, "daysSupply" := size / rate]

# Adding lagged choice
lagChoice <- unique(fullChoice[choice == 1, .(household_code, trip_code_uc, week_end, brandSize)])
setorder(lagChoice, household_code, trip_code_uc)
lagChoice[, "prevChoice" := shift(brandSize, 1, type = "lag"), by = household_code]
lagChoice[, "brandSize" := NULL]

fullChoice <- merge(fullChoice, lagChoice, by = c("household_code", "trip_code_uc", "week_end"))

############################ TESTING ###########################################
# Focusing only on Boston for testing
focusSet <- fullChoice[market == "Boston" & panel_year %in% 2014:2016]

# Running MNL regression
focusM <- mlogit.data(data = focusSet, choice = "choice", shape = "long",
                      alt.var = "brandSize", chid.var = "trip_code_uc",
                      id.var = "household_code", group.var = "brands")

reg1 <- mlogit(choice ~ p + brands | 0,
               data = focusM, weights = projection_factor)
reg2 <- mlogit(choice ~ p + brands + pkgSize | 0,
               data = focusM, weights = projection_factor)
reg3 <- mlogit(choice ~ p + brands + pkgSize + daysSupply | 0,
               data = focusM, weights = projection_factor)

stargazer(reg1, reg2, reg3, type = "text",
          out.header = FALSE,
          notes.align = "l",
          covariate.labels = c("Price", "Pack Size", "Day Supply", "Charmin",
                               "Cottonelle", "Private Label", "Qltd Ntn", "Scott"),
          order = c(1, 7:8, 2:6),
          digits = 2,
          label = "tab:mnlBostonBaseline",
          notes = c("Pack size is defined number of rolls in a package. Days' ",
                    "supply is the number of standardized rolls in a package",
                    "divided by a household's average daily consumption."),
          out = "./tables/mnlBostonBaseline.tex")

# Adding in demographics (conditional logit)
reg4 <- mlogit(choice ~ p + brands + pkgSize + daysSupply | rate + 0,
               data = focusM, weights = projection_factor)

stargazer(reg1, reg2, reg3, type = "text",
          out.header = FALSE,
          notes.align = "l",
          covariate.labels = c("Price", "Pack Size", "Day Supply", "Charmin",
                               "Cottonelle", "Private Label", "Qltd Ntn", "Scott"),
          order = c(1, 7:8, 2:6),
          digits = 2,
          label = "tab:mnlBostonBaseline",
          notes = c("Pack size is defined number of rolls in a package. Days' ",
                    "supply is the number of standardized rolls in a package",
                    "divided by a household's average daily consumption."),
          out = "./tables/mnlBostonBaseline.tex")

# Running random coefficients specification
focusM <- mlogit.data(data = focusSet, choice = "choice", shape = "long",
                      alt.var = "brandSize", chid.var = "trip_code_uc",
                      id.var = "household_code", group.var = "brands",
                      opposite = "p")

reg4 <- mlogit(choice ~ p | 0,
               data = focusM, weights = projection_factor,
               rpar = c(p = "ln"), R = 50, halton = NA)
reg5 <- mlogit(choice ~ p + brands + pkgSize | 0,
               data = focusM, weights = projection_factor,
               rpar = c(pkgSize = "n"), R = 50, halton = NA)
reg6 <- mlogit(choice ~ p + brands + pkgSize + daysSupply | 0,
               data = focusM, weights = projection_factor,
               rpar = c(daysSupply = "n"), R = 50, halton = NA)

stargazer(reg4, reg5, reg6, type = "text")
,
          out.header = FALSE,
          notes.align = "l",
          covariate.labels = c("Price", "Pack Size", "Day Supply", "Charmin",
                               "Cottonelle", "Private Label", "Qltd Ntn", "Scott"),
          order = c(1, 7:8, 2:6),
          digits = 2,
          label = "tab:mnlBostonRand",
          notes = c("Pack size is defined number of rolls in a package. Days' ",
                    "supply is the number of standardized rolls in a package",
                    "divided by a household's average daily consumption."),
          out = "./tables/mnlBostonRand.tex")
