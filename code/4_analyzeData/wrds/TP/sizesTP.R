# Gets package size purchase differences between rich and poor households
# NOTE: I don't have any distances for warehouse clubs.
library(data.table)
library(purrr)
library(lfe)
library(stargazer)
library(ggplot2)
library(ggthemes)
threads <- 8
source("./Nielsen/runReg.R")
path <- "/scratch/upenn/hossaine/"

tpPurch <- fread(paste0(path, "7260Purch.csv"), nThread = threads)[drop %in% c(0, 2)][!is.na(size)][rate > 0 & rate < Inf]
tpPurch[, c("upc", "upc_ver_uc", "trip_code_uc", "quantity", "upc_descr") := NULL]
tpPurch[, "purchWeek" := week(purchase_date)]
tpPurch[, "month" := month(purchase_date)]
tpPurch[, "day" := mday(purchase_date)]
tpPurch[, "monthStart" := (day <= 7)]
tpPurch[method_of_payment_cd %in% c(1, 2, 8), "payment" := "Cash"]
tpPurch[method_of_payment_cd %in% 3:7, "payment" := "Credit"]
tpPurch[, "hasCredit" := NA_integer_]
tpPurch[panel_year >= 2013, "hasCredit" := ifelse(payment == "Credit", 1L, 0L)]
tpPurch[panel_year >= 2013, "hasCredit" := sum(hasCredit), by = .(household_code, panel_year)]
tpPurch[panel_year >= 2013, "hasCredit" := ifelse(hasCredit > 0, 1L, 0L)]
tpPurch[, "weekOfMonth" := ceiling((day - 1) / 7)]
tpPurch[weekOfMonth == 5, "weekOfMonth" := 4]
tpPurch[weekOfMonth == 0, "weekOfMonth" := 1]
tpPurch[, "brandStore" := paste0(brand_descr, store_code_uc)]
tpPurch[, "brandRetailer" := paste0(brand_descr, retailer_code)]
tpPurch[type_of_residence == "Mobile", "type_of_residence" := "Multi-Family"]
tpPurch[, "type_of_residence" := relevel(factor(type_of_residence), ref = "Single-Family")]
tpPurch[, "total_spent" := total_spent - total_price_paid]
tpPurch[, "household_income" := factor(household_income)]

# Main analysis ################################################################
y <- "log(size)"
x <- "household_income_coarse + rate"
controls <- paste0("panel_year + month + market + household_size + ",
                   "married + white + hispanic_origin + age + urban + college")
cluster <- "market"
weights <- tpPurch$projection_factor

# Just overview, no storage or liquidity yet
reg0 <- runReg(x, y, tpPurch, controls, cluster, weights)
reg1 <- runReg(x, y, tpPurch, paste0(controls, "+brand_code_uc"), cluster, weights)
reg2 <- runReg(x, y, tpPurch, paste0(controls, "+brand_code_uc + store_code_uc"), cluster, weights)
reg3 <- runReg(x, y, tpPurch, paste0(controls, "+brandStore"), cluster, weights)

stargazer(reg0, reg1, reg2, reg3, type = "text",
          add.lines = list(c("Time/MSA/Demog. FE", "Y", "Y", "Y", "Y"),
                           c("Brand FE", "N", "Y", "Y", "N"),
                           c("Store FE", "N", "N", "Y", "N"),
                           c("Store x Brand FE", "N", "N", "N", "Y")),
          covariate.labels = c("25-50k", "50-100k", ">100k"),
          order = c(2, 3, 1),
          omit = "rate",
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("Log(Size)"), column.separate = c(4),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          notes.align = "l",
          notes = c("Standard errors are clustered at the market level."),
          digits = 2,
          label = "tab:tpQOverview",
          title = "Toilet Paper Package Size Purchases Increase in Household Income",
          out = "./tables/tpQOverview.tex")

# Running with household fixed effect
y <- "log(size)"
x <- "household_income_coarse + type_of_residence"
controls <- paste0("panel_year + month + market + household_size + household_code +",
                   "married + age + urban + college")
cluster <- "market"
weights <- tpPurch$projection_factor

reg0 <- runReg(x, y, tpPurch, controls, cluster, weights)
reg1 <- runReg(x, y, tpPurch, paste0(controls, "+brand_code_uc"), cluster, weights)
reg2 <- runReg(x, y, tpPurch, paste0(controls, "+brand_code_uc + store_code_uc"), cluster, weights)
reg3 <- runReg(x, y, tpPurch, paste0(controls, "+brandStore"), cluster, weights)
stargazer(reg0, reg1, reg2, reg3, type = "text")

# Adding in storage and liquidity
y <- "log(size)"
x <- "household_income_coarse * type_of_residence + monthStart * household_income_coarse + rate"
controls <- paste0("panel_year + month + market + household_size + ",
                   "married + white + hispanic_origin + age + urban + college")
cluster <- "market"
weights <- tpPurch$projection_factor

reg1 <- runReg(x, y, tpPurch, controls, cluster, weights)
reg2 <- runReg(x, y, tpPurch, paste0(controls, "+brand_code_uc"), cluster, weights)
reg3 <- runReg(x, y, tpPurch, paste0(controls, "+brand_code_uc+store_code_uc"), cluster, weights)
reg4 <- runReg(x, y, tpPurch, paste0(controls, "+brandStore"), cluster, weights)

stargazer(reg1, reg2, reg3, reg4, type = "text",
          add.lines = list(c("Time/MSA/Demog. FE", "Y", "Y", "Y", "Y"),
                           c("Brand FE", "N", "Y", "Y", "N"),
                           c("Store FE", "N", "N", "Y", "N"),
                           c("Store x Brand FE", "N", "N", "N", "Y")),
          omit = "rate",
          single.row = TRUE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("Log(Size)"), column.separate = c(4),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          order = c(2, 3, 1, 4, 8, 9, 7, 5, 11, 12, 10),
          covariate.labels = c("25-50k", "50-100k", ">100k", "Apartment",
                               "25-50k : Apartment", "50-100k : Apartment", ">100k : Apartment",
                               "Month Start",
                               "25-50k : Month Start", "50-100k : Month Start", ">100k : Month Start"),
          notes.align = "l",
          notes = c("Standard errors are clustered at the market level. Fixed effects ",
                    "include indicators for year, month, week, MSA, retail chain, ",
                    "and brand. Demographics include household size, marital status, ",
                    "race, ethnicity, age group, urban/rural indicator, and education."),
          digits = 2,
          label = "tab:packageSizeFullTP",
          title = "Toilet Paper Package Size Purchases Increase in Household Income")

# Add dummy for each effect to get actual effects instead of playing with
# coefficients and interactions
tpPurch[, "fullEffects" := factor(paste(type_of_residence, household_income_coarse, monthStart, sep = "_"))]
tpPurch[, "fullEffects" := relevel(fullEffects, ref = "Multi-Family_<25k_FALSE")]

reg5 <- runReg("fullEffects + rate", y, tpPurch, paste0(controls, "+brandStore"), cluster, weights)
summary(reg5)

# Credit Access ################################################################
# Restrict to years 2013 and later
y <- "log(size)"
x <- "household_income_coarse + rate"
controls <- paste0("panel_year + month + market + household_size + ",
                   "married + white + hispanic_origin + ",
                   "age + urban + college + brandStore")
cluster <- "market"
weights <- tpPurch[panel_year >= 2013 & !is.na(hasCredit)]$projection_factor

reg1 <- runReg(x, y, tpPurch[panel_year >= 2013 & !is.na(hasCredit)],
               controls, cluster, weights)
reg2 <- runReg(paste0(x, "+hasCredit"), y, tpPurch[panel_year >= 2013 & !is.na(hasCredit)],
               controls, cluster, weights)
reg3 <- runReg("household_income_coarse * hasCredit + rate", y, tpPurch[panel_year >= 2013 & !is.na(hasCredit)],
               controls, cluster, weights)

stargazer(reg1, reg2, reg3, type = "text",
          add.lines = list(c("Time/MSA/Demog. FE", "Y", "Y", "Y"),
                           c("Brand x Store FE", "Y", "Y", "Y")),
          single.row = TRUE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("Log(Size)"),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          order = c(2, 3, 1, 8, 6, 7, 5),
          omit = "rate",
          covariate.labels = c("25-50k", "50-100k", ">100k", "Credit",
                               "Credit : 25-50k", "Credit : 50-100k", "Credit : >100k"),
          notes.align = "l",
          notes = c("Standard errors are clustered at the market level.",
                    "Fixed effects include indicators for year, month, ",
                    "week, MSA, and store x brand. Demographics ",
                    "include household size, housing type, marital status, ",
                    "race, ethnicity, age group, urban/rural indicator, ",
                    "and education. 'Credit' denotes whether or not the,",
                    "household used a credit card to pay for a purchase ",
                    "at any point during the year. This is only available ",
                    "for years 2013 and onward."),
          digits = 2,
          label = "tab:packageSizeFullTpLiq",
          title = "Credit Access Has Little Effect on Low-Income Households Ability to Buy In Bulk",
          out = "./tables/packageSizeFullTpLiq.tex")

# Car ownership and public transport ###########################################
y <- "log(size)"
x <- "household_income_coarse + rate"
controls <- paste0("panel_year + month + market + household_size + ",
                   "married + white + hispanic_origin + age + urban + college + brandStore")
cluster <- "market"
weights <- tpPurch$projection_factor

reg7 <- runReg(x, y, tpPurch, controls, cluster, weights)
reg8 <- runReg(paste0(x, "+carShare"), y, tpPurch, controls, cluster, weights)
reg9 <- runReg("household_income_coarse*carShare+rate", y, tpPurch, controls, cluster, weights)
reg10 <- runReg("household_income_coarse*carShare+rate+publicTransShare", y, tpPurch, controls, cluster, weights)

stargazer(reg7, reg8, reg9, reg10, type = "text",
          order = c(1, 3, 2, 4, 9, 6, 8, 7),
          add.lines = list(c("Time/MSA/Demog. FE", "Y", "Y", "Y", "Y"),
                           c("Store x Brand FE", "Y", "Y", "Y", "Y")),
          single.row = TRUE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("Log(Size)"), column.separate = c(4),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          covariate.labels = c(">100k", "50-100k", "25-50k", "Cons. Rate",
                               "Car Share", ">100k : Car Share", "50-100k : Car Share",
                               "25-50k : Car Share", "Transit Share"),
          notes.align = "l",
          notes = c("Standard errors are clustered at the market level.",
                    "Fixed effects include indicators for year, month, ",
                    "week, MSA, retail chain, and brand. Demographics ",
                    "include household size, housing type, marital status, ",
                    "race, ethnicity, age group, urban/rural indicator, ",
                    "and education. 'Car Share' denotes the share of ",
                    "households in the corresponding ZIP that have access to ",
                    "at least 1 vehicle. Transit share is the share of households ",
                    "that take public transport. Median rooms is the median ",
                    "number of rooms in a house at in that ZIP code."),
          digits = 3,
          label = "tab:packageSizeFullTpCar",
          title = "Access to Cars May Help Low-Income Households Buy In Bulk",
          out = "./tables/packageSizeFullTpCar.tex")

# Looking at unit cost laws ####################################################
x <- "household_income_coarse + rate + lawType"
controls <- paste0("panel_year + month + market + household_size + ",
                   "type_of_residence + married + white + hispanic_origin + ",
                   "age + urban + college + brandStore")
weights <- tpPurch$projection_factor

tpPurch[law == 1, "lawType" := "None"]
tpPurch[law >= 2, "lawType" := "Law/Guidelines"]
tpPurch[, "lawType" := relevel(factor(lawType), ref = "None")]

reg1 <- runReg(x, y, tpPurch, controls, cluster, weights)
reg2 <- runReg("household_income_coarse*lawType+rate", y, tpPurch, controls, cluster, weights)
stargazer(reg1, reg2, type = "text",
          order = c(1, 3, 2, 4, 8, 5, 7, 6),
          add.lines = list(c("Time/MSA/Demog. FE", "Y", "Y"),
                           c("Retailer-Brand FE", "Y", "Y")),
          single.row = TRUE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("Log(Size)"),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          covariate.labels = c(">100k", "50-100k", "25-50k", "Cons. Rate",
                               "Unit Price Law", "Unit Price Law : >100k",
                               "Unit Price Law : 50-100k", "Unit Price Law : 25-50k"),
          notes.align = "l",
          notes = c("Standard errors are clustered at the market level.",
                    "Unit Price Law indicates presence of law or guidelines."),
          digits = 2,
          label = "tab:unitPriceLawTp",
          title = "Unit Price Regulations May Help Consumers Make Higher Value Choices",
          out = "./tables/unitPriceLawTp.tex")

# Main analysis on switcher subsample ##########################################
y <- "log(size)"
x <- "switcher * as.factor(household_income) + rate"
controls <- paste0("household_code")
cluster <- "market"
weights <- tpPurch$projection_factor

reg1 <- runReg(x, y, tpPurch[switcher == 1], controls, cluster, weights)
reg2 <- runReg(x, y, tpPurch[switcher == 1], paste0(controls, "+retailer_code"), cluster, weights)
reg3 <- runReg(x, y, tpPurch[switcher == 1], paste0(controls, "+retailer_code+brand_code_uc"), cluster, weights)
reg4 <- runReg(x, y, tpPurch[switcher == 1], paste0(controls, "+brandRetailer"), cluster, weights)

stargazer(reg1, reg2, reg3, reg4, type = "text",
          add.lines = list(c("Time/MSA/Demog. FE", "Y", "Y", "Y", "Y"),
                           c("Retailer FE", "N", "Y", "Y", "N"),
                           c("Brand FE", "N", "N", "Y", "N"),
                           c("Brand-Retailer FE", "N", "N", "N", "Y")),
          single.row = TRUE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("Log(Size)"),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          covariate.labels = c(">100k", "50-100k", "25-50k", "Cons. Rate"),
          notes.align = "l",
          notes = c("Standard errors are clustered at the market level.",
                    "Fixed effects include indicators for year, month, ",
                    "week, MSA, retail chain, and brand. Demographics ",
                    "include household size, housing type, marital status, ",
                    "race, ethnicity, age group, urban/rural indicator, ",
                    "and education."),
          order = c(1, 3, 2, 4),
          digits = 2,
          label = "tab:packageSizeFullTP",
          title = "Toilet Paper Package Size Purchases Increase in Household Income",
          out = "./tables/packageSizeFullTp.tex")

################################################################################
#################### ROBUSTNESS ADDING ALL INCOME BRACKETS #####################
################################################################################
y <- "log(size)"
x <- "as.factor(household_income) + type_of_residence + rate + monthStart * as.factor(household_income)"
controls <- paste0("panel_year + month + market + household_size + ",
                   "marital_status + white + hispanic_origin + age + urban + college")
cluster <- "market"
weights <- tpPurch$projection_factor

# Just overview, no storage or liquidity yet
reg0 <- runReg("as.factor(household_income)", y, tpPurch, controls, cluster, weights)
reg1 <- runReg("as.factor(household_income)", y, tpPurch, paste0(controls, "+brand_code_uc"), cluster, weights)
reg2 <- runReg("as.factor(household_income)", y, tpPurch, paste0(controls, "+brand_code_uc + retailer_code"), cluster, weights)
reg3 <- runReg("as.factor(household_income)", y, tpPurch, paste0(controls, "+brandRetailer"), cluster, weights)
stargazer(reg0, reg1, reg2, reg3, type = "text",
add.lines = list(c("Time/MSA/Demog. FE", "Y", "Y", "Y", "Y"),
                 c("Brand FE", "N", "Y", "Y", "N"),
                 c("Retailer FE", "N", "N", "Y", "N"),
                 c("Retailer x Brand FE", "N", "N", "N", "Y")),
single.row = TRUE, no.space = TRUE, omit.stat = c("ser", "rsq"),
out.header = FALSE,
column.labels = c("Log(Size)"), column.separate = c(4),
dep.var.caption = "", dep.var.labels.include = FALSE,
notes.align = "l",
notes = c("Standard errors are clustered at the market level."),
digits = 2,
label = "tab:packageSizeFullTp",
title = "Toilet Paper Package Size Purchases Increase in Household Income")

# Unadjusted sizes
y <- "log(sizeUnadj)"
x <- "household_income + rate"
controls <- paste0("panel_year + month + market + household_size + ",
                   "married + white + hispanic_origin + age + urban + college")
cluster <- "market"
weights <- tpPurch$projection_factor
reg0 <- runReg("as.factor(household_income)", y, tpPurch, controls, cluster, weights)
reg1 <- runReg("as.factor(household_income)", y, tpPurch, paste0(controls, "+brand_code_uc"), cluster, weights)
reg2 <- runReg("as.factor(household_income)", y, tpPurch, paste0(controls, "+brand_code_uc + retailer_code"), cluster, weights)
reg3 <- runReg("as.factor(household_income)", y, tpPurch, paste0(controls, "+brandRetailer"), cluster, weights)
stargazer(reg0, reg1, reg2, reg3, type = "text",
          add.lines = list(c("Time/MSA/Demog. FE", "Y", "Y", "Y", "Y"),
                           c("Brand FE", "N", "Y", "Y", "N"),
                           c("Retailer FE", "N", "N", "Y", "N"),
                           c("Retailer x Brand FE", "N", "N", "N", "Y")),
          single.row = TRUE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("Log(Size)"), column.separate = c(4),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          notes.align = "l",
          notes = c("Standard errors are clustered at the market level."),
          digits = 2,
          label = "tab:packageSizeFullTp",
          title = "Toilet Paper Package Size Purchases Increase in Household Income")

# Running with household fixed effect
y <- "log(size)"
x <- "household_income + type_of_residence"
controls <- paste0("panel_year + month + market + household_size + household_code +",
                   "married + age + urban + college")
cluster <- "market"
weights <- tpPurch$projection_factor

reg0 <- runReg(x, y, tpPurch, controls, cluster, weights)
reg1 <- runReg(x, y, tpPurch, paste0(controls, "+brand_code_uc"), cluster, weights)
reg2 <- runReg(x, y, tpPurch, paste0(controls, "+brand_code_uc + store_code_uc"), cluster, weights)
reg3 <- runReg(x, y, tpPurch, paste0(controls, "+brandStore"), cluster, weights)
stargazer(reg0, reg1, reg2, reg3, type = "text",
          out = "./tables/packageSizeFullTpPanel.tex")

# Allowing for storage and liquidity
reg1 <- runReg(x, y, tpPurch, controls, cluster, weights)
reg2 <- runReg(x, y, tpPurch, paste0(controls, "+brand_code_uc"), cluster, weights)
reg3 <- runReg(x, y, tpPurch, paste0(controls, "+brand_code_uc + retailer_code"), cluster, weights)
reg4 <- runReg(x, y, tpPurch, paste0(controls, "+brandRetailer"), cluster, weights)

# Using full sample (not excluding "wrong" Retailers)
tpPurch <- fread(paste0(path, "7260Purch.csv"), nThread = threads)[drop %in% c(0, 2)][!is.na(size)][rate > 0 & rate < Inf]
tpPurch[, "purchWeek" := week(purchase_date)]
tpPurch[, "month" := month(purchase_date)]
weights <- tpPurch$projection_factor
reg5 <- runReg(x, y, tpPurch, controls, cluster, weights)
reg6 <- runReg(x, y, tpPurch, paste0(controls, "+brand_code_uc"), cluster, weights)
reg7 <- runReg(x, y, tpPurch, paste0(controls, "+brand_code_uc + retailer_code"), cluster, weights)
reg8 <- runReg(x, y, tpPurch, paste0(controls, "+brandRetailer"), cluster, weights)

stargazer(reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8, type = "text")
,
          add.lines = list(c("Time/MSA/Demog. FE", "Y", "Y", "Y"),
                           c("Retailer FE", "N", "Y", "Y"),
                           c("Brand FE", "N", "N", "Y")),
          single.row = TRUE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("Log(Size)"), column.separate = c(3),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          notes.align = "l",
          notes = c("Standard errors are clustered at the market level."),
          digits = 2,
          label = "tab:packageSizeFullTp",
          title = "Toilet Paper Package Size Purchases Increase in Household Income")

x <- "as.factor(household_income) * hasCredit"
reg4 <- runReg(x, y, tpPurch, controls, cluster, weights)
reg5 <- runReg(x, y, tpPurch, paste0(controls, "+retailer_code"), cluster, weights)
reg6 <- runReg(x, y, tpPurch, paste0(controls, "+retailer_code+brand_code_uc"),
               cluster, weights)
stargazer(reg4, reg5, reg6, type = "text",
          add.lines = list(c("Time/MSA/Demog. FE", "Y", "Y", "Y"),
                           c("Retailer FE", "N", "Y", "Y"),
                           c("Brand FE", "N", "N", "Y")),
          single.row = TRUE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("Log(Size)"), column.separate = c(3),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          notes.align = "l",
          notes = c("Standard errors are clustered at the market level."),
          digits = 2,
          label = "tab:packageSizeFullTpLiq",
          title = "Credit Access May Help Lower Middle-Income Households")

incomes <- c(11, 13.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 55, 65, 85, 100)
betas <- c(0, reg1$coefficients[1:12])
se <- c(0, reg1$cse[1:12])
graphData <- data.table(income = incomes, beta = betas, se = se)
ggplot(graphData, aes(x = incomes, y = betas)) +
  geom_errorbar(aes(ymin = betas - 1.96 * se, ymax = betas + 1.96 * se), width = 1) +
  geom_line() +
  geom_hline(yintercept = 0) +
  labs(title = "Package Size Purchased Increases in Income", x = "Household Income",
       y = "Log TP Rolls Per Package",
       caption = paste0("Source: Author calulations using Nielsen Consumer Panel.\n",
                   "Note: TP rolls are standardized to 225-sheet, 2-ply rolls.\n",
                   "Midpoints of household income bins are plotted above.")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text())
ggsave(filename = "./figures/tpCoefficients.png")

# Running full interaction with week of month for liquidity test
# Week of month to test for liquidity constraints
y <- "log(size)"
x <- "household_income * as.factor(weekOfMonth)"
controls <- paste0("panel_year + month + purchWeek + market + household_size + ",
                   "type_of_residence + marital_status + white + hispanic_origin + ",
                   "age + urban + college")
cluster <- "market"
weights <- tpPurch$projection_factor

reg1 <- runReg(x, y, tpPurch, controls, cluster, weights)
reg2 <- runReg(x, y, tpPurch, paste0(controls, "+retailer_code"), cluster, weights)
reg3 <- runReg(x, y, tpPurch, paste0(controls, "+retailer_code+brand_code_uc"),
               cluster, weights)

stargazer(reg1, reg2, reg3, type = "text")


#### Graphing average size purchased
tpPurch <- fread(paste0(path, "7260Purch.csv"), nThread = threads)[drop %in% c(0, 2)][!is.na(size)][rate > 0 & rate < Inf]
tpPurch[, "household_income" := factor(household_income)]
incomes <- c(13.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 55, 65, 85, 100)

# Demographic adjustment
reg0 <- felm(data = tpPurch, log(size) ~ household_income + household_size + hispanic_origin +
             age + white + child + married + rate)
reg1 <- felm(data = tpPurch, log(size) ~ household_income + household_size + hispanic_origin +
             age + white + child + married + rate | retailer_code)
reg2 <- lm(data = tpPurch[store_code_uc != 0], log(size) ~ household_income + household_size + hispanic_origin +
             age + white + child + married + rate)
reg3 <- felm(data = tpPurch[store_code_uc != 0], log(size) ~ household_income + household_size + hispanic_origin +
             age + white + child + married + rate | store_code_uc)
stargazer(reg0, reg1, reg2, reg3, type = "text")
betas0 <- summary(reg0)$coefficients[2:13, 1]
se0 <- summary(reg0)$coefficients[2:13, 2]
graphData1 <- data.table(income = incomes, beta = betas0, se = se0, type = "No Store Adjustment")

betas1 <- summary(reg1)$coefficients[1:12, 1]
se1 <- summary(reg1)$coefficients[1:12, 2]
graphData2 <- data.table(income = incomes, beta = betas1, se = se1, type = "Store Adjustment")

graphData <- rbindlist(list(graphData1, graphData2), use.names = TRUE)

ggplot(data = graphData2, aes(x = income, y = beta)) +
  geom_errorbar(aes(ymin = beta - 1.96 * se, ymax = beta + 1.96 * se), width = 1) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  labs(title = "Quantity Purchased Increases in Income", x = "Household Income",
       y = "Log Quantity",
       caption = paste0("Source: Author calulations. \n ",
                        "Note: Demographic adjustments control for household size, \n",
                        "age, race, marital status, and children. Midpoints of \n ",
                        "household income bins are plotted above.")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text())
ggsave("./figures/averageSizePurchased.png")


# Check coefficients by year
fullCoef <- NULL
for (i in 2006:2014) {
  weights <- tpPurch[panel_year == i]$projection_factor
  reg0 <- felm(data = tpPurch[panel_year == i], log(size) ~ household_income_coarse |
                 household_size + married + white + hispanic_origin + age + urban + college,
               weights = weights)
  coefs <- data.table(summary(reg0)$coefficients, keep.rownames = TRUE)[, c("t value", "Pr(>|t|)") := NULL][rn != "rate"]
  coefs[, "rn" := gsub("household_income_coarse", "", rn)]
  coefs[, "panel_year" := i]
  fullCoef <- rbindlist(list(fullCoef, coefs))
}

fullCoef[, c("min", "max") := .(Estimate - 1.96 * `Std. Error`, Estimate + 1.96 * `Std. Error`)]
fwrite(fullCoef, "/home/upenn/hossaine/Nielsen/Data/coefs.csv")

# Download using scp and plot manually since ggplot2 isn't working on WRDS
library(data.table)
library(ggplot2)
library(ggthemes)
fullCoef <- fread("./code/0_data/coefs.csv")
fullCoef[, "rn" := factor(rn, levels = c("25-50k", "50-100k", ">100k"), ordered = TRUE)]
ggplot(data = fullCoef, aes(x = panel_year, y = Estimate, color = rn)) +
  geom_errorbar(aes(ymin = min, ymax = max), width = 0.2) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_fiv
+
  labs(title = "Quantity Purchased Increases in Income", x = "Household Income",
       y = "Log Quantity",
       caption = paste0("Source: Author calulations. \n ",
                        "Note: Demographic adjustments control for household size, \n",
                        "age, race, marital status, and children. Midpoints of \n ",
                        "household income bins are plotted above.")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text())
