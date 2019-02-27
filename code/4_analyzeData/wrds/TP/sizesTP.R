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

tpPurch <- fread(paste0(path, "7260Purch.csv"), nThread = threads)[drop == 0]
tpPurch[, c("upc", "upc_ver_uc", "trip_code_uc", "quantity", "upc_descr") := NULL]
tpPurch[, "purchWeek" := week(purchase_date)]
tpPurch[, "day" := mday(purchase_date)]
tpPurch[method_of_payment_cd %in% c(1, 2, 8), "payment" := "Cash"]
tpPurch[method_of_payment_cd %in% 3:7, "payment" := "Credit"]
tpPurch[, "hasCredit" := NA_integer_]
tpPurch[panel_year >= 2013, "hasCredit" := ifelse(payment == "Credit", 1L, 0L)]
tpPurch[panel_year >= 2013, "hasCredit" := sum(hasCredit), by = .(household_code, panel_year)]
tpPurch[panel_year >= 2013, "hasCredit" := ifelse(hasCredit > 0, 1L, 0L)]
tpPurch[, "weekOfMonth" := ceiling((day - 1) / 7)]
tpPurch[weekOfMonth == 5, "weekOfMonth" := 4]
tpPurch[weekOfMonth == 0, "weekOfMonth" := 1]

# Main analysis ################################################################
y <- "log(size)"
x <- "household_income_coarse"
controls <- paste0("panel_year + month + purchWeek + market + household_size + ",
                   "type_of_residence + marital_status + white + hispanic_origin + ",
                   "age + urban + college")
cluster <- "market"
weights <- tpPurch$projection_factor

reg1 <- runReg(x, y, tpPurch, controls, cluster, weights)
reg2 <- runReg(paste0(x, "+rate"), y, tpPurch, controls, cluster, weights)
reg3 <- runReg(paste0(x, "+rate"), y, tpPurch, paste0(controls, "+retailer_code"), cluster, weights)
reg4 <- runReg(paste0(x, "+rate"), y, tpPurch, paste0(controls, "+retailer_code+brand_code_uc"),
               cluster, weights)

stargazer(reg1, reg2, reg3, reg4, type = "text",
          add.lines = list(c("Time/MSA/Demog. FE", "Y", "Y", "Y", "Y"),
                           c("Retailer FE", "N", "N", "Y", "Y"),
                           c("Brand FE", "N", "N", "N", "Y")),
          single.row = TRUE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("Log(Size)"), column.separate = c(4),
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

# Credit Access ################################################################
# Restrict to years 2013 and later
# # No interaction
# x <- "household_income_coarse + hasCredit"
# weights <- tpPurch[panel_year >= 2013]$projection_factor
#
# reg4 <- runReg(x, y, tpPurch[panel_year >= 2013], controls, cluster, weights)
# reg5 <- runReg(paste0(x, "+rate"), y, tpPurch[panel_year >= 2013], controls, cluster, weights)
# reg6 <- runReg(paste0(x, "+rate"), y, tpPurch[panel_year >= 2013], paste0(controls, "+retailer_code"), cluster, weights)
# reg7 <- runReg(paste0(x, "+rate"), y, tpPurch[panel_year >= 2013], paste0(controls, "+retailer_code+brand_code_uc"),
#                cluster, weights)

# With interaction
x <- "household_income_coarse * hasCredit"
weights <- tpPurch[panel_year >= 2013]$projection_factor

reg8 <- runReg(x, y, tpPurch[panel_year >= 2013], controls, cluster, weights)
reg9 <- runReg(paste0(x, "+rate"), y, tpPurch[panel_year >= 2013], controls, cluster, weights)
reg10 <- runReg(paste0(x, "+rate"), y, tpPurch[panel_year >= 2013], paste0(controls, "+retailer_code"), cluster, weights)
reg11 <- runReg(paste0(x, "+rate"), y, tpPurch[panel_year >= 2013], paste0(controls, "+retailer_code+brand_code_uc"),
               cluster, weights)

# # Rerun original analysis on this subsample for robustness
# x <- "household_income_coarse"
# weights <- tpPurch[panel_year >= 2013]$projection_factor
# reg4 <- runReg(x, y, tpPurch[panel_year >= 2013], controls, cluster, weights)
# reg5 <- runReg(x, y, tpPurch[panel_year >= 2013], paste0(controls, "+retailer_code"), cluster, weights)
# reg6 <- runReg(x, y, tpPurch[panel_year >= 2013], paste0(controls, "+retailer_code+brand_code_uc"),
#                cluster, weights)

stargazer(reg8, reg9, reg10, reg11, type = "text",
          add.lines = list(c("Time/MSA/Demog. FE", "Y", "Y", "Y", "Y"),
                           c("Retailer FE", "N", "N", "Y", "Y"),
                           c("Brand FE", "N", "N", "N", "Y")),
          single.row = TRUE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("Log(Size)"), column.separate = c(4),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          covariate.labels = c(">100k", "50-100k", "25-50k", "Cons. Rate",
                               "Credit:>100k", "Credit:50-100k", "Credit:25-50k", "Credit"),
          notes.align = "l",
          notes = c("Standard errors are clustered at the market level.",
                    "Fixed effects include indicators for year, month, ",
                    "week, MSA, retail chain, and brand. Demographics ",
                    "include household size, housing type, marital status, ",
                    "race, ethnicity, age group, urban/rural indicator, ",
                    "and education. 'Credit' denotes whether or not the,",
                    "household used a credit card to pay for a purchase ",
                    "at any point during the year. This is only available ",
                    "for years 2013 and onward."),
          order = c(1, 3, 2, 5, 6, 8, 7, 4),
          digits = 2,
          label = "tab:packageSizeFullTpLiq",
          title = "Credit Access May Help Low-Income Households Buy In Bulk",
          out = "./tables/packageSizeFullTpLiq.tex")

# Getting channel FEs ##########################################################
regData <- tpPurch[channel_type %in% unique(tpPurch$channel_type)]
regData[, "channel_type" := relevel(as.factor(channel_type), ref = "Grocery")]

x <- "household_income_coarse + total + channel_type"
regFE <- runReg(x, y, tpPurch, paste0(controls, "+brand_code_uc+household_code"), weights)

stargazer(regFE, type = "text",
          add.lines = list(c("Time/MSA/Demog. FE", "Y"),
                           c("Channel FE", "Y"),
                           c("Brand FE", "Y"),
                           c("Household FE", "Y")),
          keep = c("household_income*", "*Dollar", "*Discount", "*Warehouse"),
          single.row = TRUE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("Log(Size)"), column.separate = c(1),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          covariate.labels = c(">100k", "50-100k", "25-50k",
                               "Discount", "Dollar", "Dist (km)"),
          notes.align = "l",
          notes = c("Standard errors are clustered at the market level."),
          order = c(1, 3, 2, 4, 5, 6),
          digits = 2,
          label = "tab:tpFEs",
          title = "Channel FEs",
          out = "./tables/tpFEs.tex")

# Car ownership and travel costs ##############################################
y <- "log(size)"
x <- "household_income_coarse + ownership"
controls <- paste0("panel_year + month + purchWeek + market + household_size + ",
                   "type_of_residence + marital_status + white + hispanic_origin + ",
                   "age + urban + college")
cluster <- "market"
weights <- tpPurch$projection_factor

reg7 <- runReg(x, y, tpPurch, controls, cluster, weights)
reg8 <- runReg(paste0(x, "+rate"), y, tpPurch, controls, cluster, weights)
reg9 <- runReg(paste0(x, "+rate"), y, tpPurch, paste0(controls, "+retailer_code"), cluster, weights)
reg10 <- runReg(paste0(x, "+rate"), y, tpPurch, paste0(controls, "+retailer_code+brand_code_uc"), cluster, weights)

stargazer(reg7, reg8, reg9, reg10, type = "text",
          add.lines = list(c("Time/MSA/Demog. FE", "Y", "Y", "Y"),
                           c("Retailer FE", "N", "Y", "Y"),
                           c("Brand FE", "N", "N", "Y")),
          single.row = TRUE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("Log(Size)"), column.separate = c(3),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          covariate.labels = c(">100k", "50-100k", "25-50k", "Car Share"),
          notes.align = "l",
          notes = c("Standard errors are clustered at the market level.",
                    "Fixed effects include indicators for year, month, ",
                    "week, MSA, retail chain, and brand. Demographics ",
                    "include household size, housing type, marital status, ",
                    "race, ethnicity, age group, urban/rural indicator, ",
                    "and education. 'Car Share' denotes the share of ",
                    "households in the corresponding PUMA that own at ",
                    "least 1 vehicle."),
          order = c(1, 3, 2, 4, 5, 7, 6),
          digits = 2,
          label = "tab:packageSizeFullTpCar",
          title = "Car Ownership May Help Low-Income Households Buy In Bulk",
          out = "./tables/packageSizeFullTpCar.tex")

# Restricting to subset of PUMAs with more than 99% car ownership
y <- "log(size)"
x <- "household_income_coarse + ownership"
controls <- paste0("panel_year + month + purchWeek + market + household_size + ",
                   "type_of_residence + marital_status + white + hispanic_origin + ",
                   "age + urban + college")
cluster <- "market"
weights <- tpPurch[ownership >= 0.99]$projection_factor

reg7 <- runReg(x, y, tpPurch[ownership >= 0.99], controls, cluster, weights)
reg8 <- runReg(x, y, tpPurch[ownership >= 0.99], paste0(controls, "+retailer_code"), cluster, weights)
reg9 <- runReg(x, y, tpPurch[ownership >= 0.99], paste0(controls, "+retailer_code+brand_code_uc"), cluster, weights)

stargazer(reg7, reg8, reg9, type = "text",
          add.lines = list(c("Time/MSA/Demog. FE", "Y", "Y", "Y"),
                           c("Retailer FE", "N", "Y", "Y"),
                           c("Brand FE", "N", "N", "Y")),
          single.row = TRUE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("Log(Size)"), column.separate = c(3),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          covariate.labels = c(">100k", "50-100k", "25-50k", "Car Share"),
          notes.align = "l",
          notes = c("Standard errors are clustered at the market level.",
                    "Fixed effects include indicators for year, month, ",
                    "week, MSA, retail chain, and brand. Demographics ",
                    "include household size, housing type, marital status, ",
                    "race, ethnicity, age group, urban/rural indicator, ",
                    "and education. 'Car Share' denotes the share of ",
                    "households in the corresponding PUMA that own at ",
                    "least 1 vehicle."),
          order = c(1, 3, 2, 4, 5, 7, 6),
          digits = 2,
          label = "tab:packageSizeFullTpCarAll",
          title = "Car Ownership May Help Low-Income Households Buy In Bulk",
          out = "./tables/packageSizeFullTpCarAll.tex")

# Looking at unit cost laws ####################################################
x <- "household_income_coarse + lawType"

tpPurch[law == 1, "lawType" := "None"]
tpPurch[law == 2, "lawType" := "Guidelines"]
tpPurch[law >= 3, "lawType" := "Law"]
tpPurch[, "lawType" := relevel(factor(lawType), ref = "None")]

reg1 <- runReg(x, y, tpPurch, controls, cluster, weights)
reg2 <- runReg(x, y, tpPurch, paste0(controls, "+retailer_code"), cluster, weights)
reg3 <- runReg(x, y, tpPurch, paste0(controls, "+retailer_code+brand_code_uc"),
               cluster, weights)

stargazer(reg1, reg2, reg3, type = "text",
          add.lines = list(c("Time/MSA/Demog. FE", "Y", "Y", "Y"),
                           c("Retailer FE", "N", "Y", "Y"),
                           c("Brand FE", "N", "N", "Y")),
          single.row = TRUE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("Log(Size)"), column.separate = c(3),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          covariate.labels = c(">100k", "50-100k", "25-50k", "Guidelines", "Law"),
          notes.align = "l",
          notes = c("Standard errors are clustered at the market level."),
          order = c(1, 3, 2, 4, 5),
          digits = 2,
          label = "tab:unitPriceLawTp",
          title = "Unit Price Regulations May Help Consumers Make Higher Value Choices",
          out = "./tables/unitPriceLawTp.tex")

# Week of month to test for liquidity constraints
y <- "log(size)"
x <- "household_income_coarse * as.factor(weekOfMonth)"
controls <- paste0("panel_year + month + purchWeek + market + household_size + ",
                   "type_of_residence + marital_status + white + hispanic_origin + ",
                   "age + urban + college")
cluster <- "market"
weights <- tpPurch$projection_factor

reg1 <- runReg(x, y, tpPurch, controls, cluster, weights)
reg2 <- runReg(x, y, tpPurch, paste0(controls, "+retailer_code"), cluster, weights)
reg3 <- runReg(x, y, tpPurch, paste0(controls, "+retailer_code+brand_code_uc"),
               cluster, weights)

stargazer(reg1, reg2, reg3, type = "text",
          add.lines = list(c("Time/MSA/Demog. FE", "Y", "Y", "Y"),
                           c("Retailer FE", "N", "Y", "Y"),
                           c("Brand FE", "N", "N", "Y")),
          single.row = TRUE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("Log(Size)"), column.separate = c(3),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          covariate.labels = c(">100k", "50-100k", "25-50k",
                               "Week 2", "Week 3", "Week 4",
                               "Week 2 : >100k", "Week 2 : 50-100k", "Week 2 : 25-50k",
                               "Week 3 : >100k", "Week 3 : 50-100k", "Week 3 : 25-50k",
                               "Week 4 : >100k", "Week 4 : 50-100k", "Week 4 : 25-50k"),
          notes.align = "l",
          notes = c("Standard errors are clustered at the market level.",
                    "Fixed effects include indicators for year, month, ",
                    "week, MSA, retail chain, and brand. Demographics ",
                    "include household size, housing type, marital status, ",
                    "race, ethnicity, age group, urban/rural indicator, ",
                    "and education."),
          order = c(1, 3, 2),
          digits = 2,
          label = "tab:packageSizeFullWeekLiquidity",
          title = "Toilet Paper Package Size Purchases Increase in Household Income",
          out = "./tables/packageSizeFullWeekLiquidity.tex")

################################################################################
#################### ROBUSTNESS ADDING ALL INCOME BRACKETS #####################
################################################################################
y <- "log(size)"
x <- "as.factor(household_income)"
controls <- paste0("panel_year + month + purchWeek + market + household_size + ",
                   "type_of_residence + marital_status + white + hispanic_origin + ",
                   "age + urban + college")
cluster <- "market"
weights <- tpPurch$projection_factor
reg1 <- runReg(x, y, tpPurch, controls, cluster, weights)
reg2 <- runReg(x, y, tpPurch, paste0(controls, "+retailer_code"), cluster, weights)
reg3 <- runReg(x, y, tpPurch, paste0(controls, "+retailer_code+brand_code_uc"),
               cluster, weights)

stargazer(reg1, reg2, reg3, type = "text",
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
betas <- c(0, reg1$coefficients)
se <- c(0, reg1$cse)
graphData <- data.table(income = incomes, beta = betas, se = se)
ggplot(graphData, aes(x = incomes, y = betas)) +
  geom_errorbar(aes(ymin = betas - 1.96 * se, ymax = betas + 1.96 * se), width = 1) +
  geom_line() +
  labs(title = "Package Size Purchased Increases in Income", x = "Household Income",
       y = "Log TP Rolls Per Package",
       caption = paste0("Source: Author calulations using Nielsen Consumer Panel.\n",
                   "Note: TP rolls are standardized to 275-sheet, 2-ply rolls.\n",
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
