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

tpPurch <- fread(paste0(path, "3625Purch.csv"))
tpPurch[, c("upc", "upc_ver_uc", "trip_code_uc", "quantity",
            "product_module_code", "upc_descr", "size1_units") := NULL]
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

# Plot purchase sizes for various households
trips <- tpPurch[trip <= 100, .(size = weighted.mean(sizeUnadj, w = projection_factor)),
                 by = .(trip, household_income_coarse)]
tpPurch[, "trip" := 1:.N, by = household_code]
ggplot(data = tpPurch[type_of_residence == "Mobile" & trip <= 100 & household_income_coarse == ">100k"],
       aes(x = trip, y = sizeUnadj, color = as.character(household_code))) +
  geom_line() +
  theme_fivethirtyeight() +
  theme(legend.position = "none")

# Tallying deviations from the mode purchase
tpPurch[, "mode" := as.numeric(names(table(sizeUnadj))[which.max(table(sizeUnadj))])]
tpPurch[, "dev" := ifelse(mode == sizeUnadj, 0L, 1L)]
totalDevs <- tpPurch[, .(totalDevs = sum(dev) / .N * 100,
                         mode = mode),
                     by = .(household_code, panel_year, projection_factor,
                            household_income, household_size, type_of_residence,
                            marital_status, white, hispanic_origin, market,
                            household_composition, household_income_coarse,
                            age, college, urban)]
reg <- felm(data = totalDevs, totalDevs ~ factor(household_income_coarse, ordered = FALSE) |
              panel_year + market + household_size + type_of_residence +
              marital_status + white + hispanic_origin + age + urban +
              college + household_composition | 0 | market,
            weights = totalDevs$projection_factor)
