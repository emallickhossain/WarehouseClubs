# Calculates trip frequency differences between rich and poor households
library(data.table)
library(purrr)
library(furrr)
library(stringr)
library(lfe)
library(stargazer)
plan(multiprocess)
path <- "/scratch/upenn/hossaine/"

tpPurch <- fread(paste0(path, "tpPurch.csv"))
panel <- fread(paste0(path, "fullPanel.csv"))

# Getting annual trips for each households
annualTrips <- tpPurch[, .(trips = uniqueN(trip_code_uc),
                           annualTP = mean(annualTP),
                           annualSpend = sum(total_price_paid_real),
                           avgSpend = mean(total_price_paid_real),
                           size = mean(size),
                           sizeUnadj = mean(sizeUnadj)),
                        by = .(panel_year, household_code)]
annualTrips <- merge(annualTrips, panel, by = c("household_code", "panel_year"))

# Regression for monthly trips (logs)
reg1 <- felm(data = annualTrips, log(trips) ~ household_income | 0 | 0 | market,
             weights = annualTrips$projection_factor)
reg2 <- felm(data = annualTrips, log(trips) ~ household_income |
               as.factor(panel_year) + market + as.factor(urban) +
               household_size + as.factor(marital_status) +
               age + as.factor(race) +
               as.factor(hispanic_origin) + as.factor(college) | 0 | market,
             weights = annualTrips$projection_factor)

# Regression for monthly trips (levels)
reg3 <- felm(data = annualTrips, trips ~ household_income | 0 | 0 | market,
             weights = annualTrips$projection_factor)
reg4 <- felm(data = annualTrips, trips ~ household_income |
               as.factor(panel_year) + market + as.factor(urban) +
               household_size + as.factor(marital_status) +
               age + as.factor(race) +
               as.factor(hispanic_origin) + as.factor(college) | 0 | market,
             weights = annualTrips$projection_factor)

# Getting annual TP purchases
reg5 <- felm(data = annualTrips, log(annualTP) ~ household_income |
               as.factor(panel_year) + market + as.factor(urban) +
               household_size + as.factor(marital_status) +
               age + as.factor(race) +
               as.factor(hispanic_origin) + as.factor(college) | 0 | market,
             weights = annualTrips$projection_factor)

# Making regression table
stargazer(reg1, reg2, reg3, reg4, reg5,
          add.lines = list(c("Year FE", "N", "Y", "N", "Y", "Y"),
                           c("MSA FE", "N", "Y", "N", "Y", "Y"),
                           c("Demographics", "N", "Y", "N", "Y", "Y")),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          column.labels = c("Logs", "Levels", "Ann. Purch"), column.separate = c(2, 2, 1),
          dep.var.caption = "Annual Trips", dep.var.labels.include = FALSE,
          covariate.labels = c(">100k", "50-100k", "25-50k", "Constant"),
          notes.align = "l",
          notes = "Standard errors are clustered at the market level.",
          order = c(1, 3, 2, 4),
          digits = 2,
          label = "tab:tripFrequency",
          out = "tables/tripFrequency.tex")
