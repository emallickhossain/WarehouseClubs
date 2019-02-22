# Running multinomial models on a panel of households that make between 10-20 shopping trips
library(mlogit)
library(gmnl)
library(data.table)
library(stargazer)

fullChoiceMlog <- fread("/scratch/upenn/hossaine/TPMLogit.csv")
trips <- fullChoiceMlog[, .(tripCount = sum(choice)), by = household_code]
tripID <- trips[tripCount >= 10 & tripCount <= 20]$household_code
focusSample <- fullChoiceMlog[household_code %in% tripID]

############################ TESTING ###########################################
focusSet <- fullChoiceMlog[panel_year == 2016 & market == "Boston"]

# Non-random estimations
focusMlog <- mlogit.data(data = focusSet,
                         choice = "choice", shape = "long",
                         alt.var = "id", chid.var = "trip_code_uc",
                         id.var = "household_code", opposite = "p")
reg1 <- gmnl(choice ~ p | 0,
             data = focusMlog,
             subset = 1:5000,
             model = "mixl",
             R = 10,
             ranp = c(p = "n"))

reg2 <- mlogit(choice ~ p | household_income_cts,
               data = focusMlog, print.level = 1)

reg3 <- mlogit(choice ~ p  | household_income_cts + household_size_cts,
               data = focusMlog, print.level = 1)
stargazer(reg1, reg2, reg3, type = "text")

# Random estimations (no panel techniques)
reg4 <- mlogit(choice ~ p | 0, data = focusMlog, print.level = 1,
               rpar = c(p = "ln"), R = 50, halton = NA, panel = FALSE)

reg5 <- mlogit(choice ~ p | household_income_cts + household_size_cts +
                 as.factor(age) + child,
               data = focusMlog, print.level = 1,
               rpar = c(p = "ln"), R = 50, halton = NA, panel = FALSE)
stargazer(reg4, reg5, type = "text")

################################################################################

# Non-random estimates
fullChoiceMlog <- mlogit.data(fullChoiceMlog, choice = "choice", shape = "long",
                              alt.var =  "id", chid.var = "trip_code_uc",
                              id.var = "household_code", opposite = "p")
reg1 <- mlogit(choice ~ p + pkgSize + brandBin | 0, print.level = 1,
               weights = projection_factor, data = fullChoiceMlog)
save(reg1, file = "/scratch/upenn/hossaine/reg1.rda", compress = TRUE)

reg2 <- mlogit(choice ~ 1 | household_income_cts + household_size_cts +
                 as.factor(age) + child, print.level = 1,
               weights = projection_factor, data = fullChoiceMlog)
save(reg2, file = "/scratch/upenn/hossaine/reg2.rda", compress = TRUE)

reg3 <- mlogit(choice ~ p + pkgSize + brandBin | household_income_cts +
                 household_size_cts + as.factor(age) + child, print.level = 1,
               weights = projection_factor, data = fullChoiceMlog)
save(reg3, file = "/scratch/upenn/hossaine/reg3.rda", compress = TRUE)

stargazer(reg1, reg2, reg3, type = "text")




# Random coefficients
reg2 <- mlogit(choice ~ p + pkgSize + brandBin | 0, print.level = 1,
               weights = projection_factor, data = fullChoiceMlog,
               rpar = c(p = "ln", pkgSize = "n"), R = 50, halton = NA, panel = TRUE)
save(reg2, file = "reg2.rda", compress = TRUE)
stargazer(reg1, reg2, type = "text")
