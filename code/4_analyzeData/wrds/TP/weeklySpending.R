# Looks at weekly spending in Nielsen
library(data.table)
library(purrr)
library(furrr)
library(stringr)
library(lfe)
library(stargazer)
plan(multiprocess)
yr <- 2004:2016
path <- "/scratch/upenn/hossaine/"

panel <- fread(paste0(path, "fullPanel.csv"))

getPurch <- function(yr) {
  purch <- fread(paste0(path, "Purchases/purchase", yr, ".csv"))
  purch[, "purchase_date" := as.Date(purchase_date, "%Y-%m-%d")]
  purch[, "week" := week(purchase_date)]
  weeklySpend <- purch[, .(spend = sum(total_price_paid)), by = .(household_code, panel_year, week)]
  return(weeklySpend)
}

weeklySpend <- rbindlist(future_map(yr, getPurch))
weeklySpend <- merge(weeklySpend, panel, by = c("household_code", "panel_year"))
reg1 <- felm(data = weeklySpend, spend ~ as.factor(week) * household_income |
               panel_year + market +
               household_size + marital_status + race + hispanic_origin +
               age + college + urban | 0 | market,
             weights = weeklySpend$projection_factor)
summary(reg1)
