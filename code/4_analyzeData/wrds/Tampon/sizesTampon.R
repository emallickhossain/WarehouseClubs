# Gets package size purchase differences between rich and poor households
# NOTE: I don't have any distances for warehouse clubs.
library(data.table)
library(purrr)
library(lfe)
library(stargazer)
library(ggplot2)
library(ggthemes)
source("./Nielsen/runReg.R")
path <- "/scratch/upenn/hossaine/"

tamponPurch <- fread(paste0(path, "7270Purch.csv"))
tamponPurch[, c("upc", "upc_ver_uc", "trip_code_uc", "quantity",
                "product_module_code", "upc_descr", "size1_units") := NULL]
tamponPurch[, "purchWeek" := week(purchase_date)]
tamponPurch[method_of_payment_cd %in% c(1, 2, 8), "payment" := "Cash"]
tamponPurch[method_of_payment_cd %in% 3:7, "payment" := "Credit"]
tamponPurch[, "hasCredit" := ifelse(payment == "Credit", 1L, 0L)]
tamponPurch[, "hasCredit" := sum(hasCredit, na.rm = TRUE), by = .(household_code, panel_year)]
tamponPurch[, "hasCredit" := ifelse(hasCredit > 0, 1L, 0L)]

y <- "log(size)"
x <- "household_income_coarse"
controls <- paste0("panel_year + month + purchWeek + market + household_size + ",
                   "type_of_residence + marital_status + white + hispanic_origin + ",
                   "age + urban + college")
cluster <- "market"
weights <- tamponPurch$projection_factor

reg1 <- runReg(x, y, tamponPurch, controls, cluster, weights)
reg2 <- runReg(x, y, tamponPurch, paste0(controls, "+retailer_code"), cluster, weights)
reg3 <- runReg(x, y, tamponPurch, paste0(controls, "+retailer_code+brand_code_uc"),
               cluster, weights)

stargazer(reg1, reg2, reg3, type = "text",
          add.lines = list(c("Time/MSA/Demog. FE", "Y", "Y", "Y"),
                           c("Retailer FE", "N", "Y", "Y"),
                           c("Brand FE", "N", "N", "Y")),
          single.row = TRUE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("Log(Size)"), column.separate = c(3),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          covariate.labels = c(">100k", "50-100k", "25-50k"),
          notes.align = "l",
          notes = c("Standard errors are clustered at the market level."),
          order = c(1, 3, 2),
          digits = 2,
          label = "tab:packageSizeFullTampon",
          title = "Tampon Package Size Purchases Increase in Household Income",
          out = "./tables/packageSizeFullTampon.tex")

x <- "household_income_coarse * hasCredit"
reg4 <- runReg(x, y, tamponPurch, controls, cluster, weights)
reg5 <- runReg(x, y, tamponPurch, paste0(controls, "+retailer_code"), cluster, weights)
reg6 <- runReg(x, y, tamponPurch, paste0(controls, "+retailer_code+brand_code_uc"),
               cluster, weights)
stargazer(reg4, reg5, reg6, type = "text",
          add.lines = list(c("Time/MSA/Demog. FE", "Y", "Y", "Y"),
                           c("Retailer FE", "N", "Y", "Y"),
                           c("Brand FE", "N", "N", "Y")),
          single.row = TRUE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("Log(Size)"), column.separate = c(3),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          covariate.labels = c(">100k", "50-100k", "25-50k", "Credit",
                               "Credit:>100k", "Credit:50-100k", "Credit:25-50k"),
          notes.align = "l",
          notes = c("Standard errors are clustered at the market level."),
          order = c(1, 3, 2, 4, 5, 7, 6),
          digits = 2,
          label = "tab:packageSizeFullTamponLiq",
          title = "Credit Access May Help Lower Middle-Income Households",
          out = "./tables/packageSizeFullTamponLiq.tex")

# Getting channel FEs
regData <- tamponPurch[channel_type %in% unique(tamponPurch$channel_type)]
regData[, "channel_type" := relevel(as.factor(channel_type), ref = "Grocery")]

x <- "household_income_coarse + total + channel_type"
regFE <- runReg(x, y, tamponPurch, paste0(controls, "+brand_code_uc+household_code"), weights)

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
          label = "tab:tamponFEs",
          title = "Channel FEs",
          out = "./tables/tamponFEs.tex")

# Looking at unit cost laws
x <- "household_income_coarse + as.factor(law)"

reg1 <- runReg(x, y, tamponPurch, controls, cluster, weights)
reg2 <- runReg(x, y, tamponPurch, paste0(controls, "+retailer_code"), cluster, weights)
reg3 <- runReg(x, y, tamponPurch, paste0(controls, "+retailer_code+brand_code_uc"),
               cluster, weights)

stargazer(reg1, reg2, reg3, type = "text")

################################################################################
#################### ROBUSTNESS ADDING ALL INCOME BRACKETS #####################
################################################################################
x <- "as.factor(household_income)"

reg1 <- runReg(x, y, tamponPurch, controls, cluster, weights)
reg2 <- runReg(x, y, tamponPurch, paste0(controls, "+retailer_code"), cluster, weights)
reg3 <- runReg(x, y, tamponPurch, paste0(controls, "+retailer_code+brand_code_uc"),
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
          label = "tab:packageSizeFullTampon",
          title = "Tampon Package Size Purchases Increase in Household Income")

x <- "as.factor(household_income) * hasCredit"
reg4 <- runReg(x, y, tamponPurch, controls, cluster, weights)
reg5 <- runReg(x, y, tamponPurch, paste0(controls, "+retailer_code"), cluster, weights)
reg6 <- runReg(x, y, tamponPurch, paste0(controls, "+retailer_code+brand_code_uc"),
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
          label = "tab:packageSizeFullTamponLiq",
          title = "Credit Access May Help Lower Middle-Income Households")

incomes <- c(11, 13.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 55, 65, 85, 100)
betas <- c(0, reg1$coefficients)
se <- c(0, reg1$cse)
graphData <- data.table(income = incomes, beta = betas, se = se)
ggplot(graphData, aes(x = incomes, y = betas)) +
  geom_errorbar(aes(ymin = betas - 1.96 * se, ymax = betas + 1.96 * se), width = 1) +
  geom_line() +
  theme_fivethirtyeight()
