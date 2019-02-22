# Looks at sales, coupon usage, and generics by demographics
library(data.table)
library(lfe)
library(purrr)
library(stargazer)
source("./Nielsen/runReg.R")
path <- "/scratch/upenn/hossaine/"
yr <- 2004:2016
panel <- fread(paste0(path, "fullPanel.csv"),
               select = c("household_code", "panel_year", "projection_factor",
                          "household_income", "household_size", "type_of_residence",
                          "marital_status", "hispanic_origin", "market", "age",
                          "college", "urban", "white", "child"))
prod <- fread(paste0(path, "prod.csv"), select = c("upc", "upc_ver_uc", "brand_code_uc"))

getPurch <- function(yr) {
  purch <- fread(paste0(path, "Purchases/purchase", yr, ".csv"))
  purch[deal_flag_uc == 0 & coupon_value == 0, "deal_type_ind" := 1L]    # No deal
  purch[deal_flag_uc == 1 & coupon_value == 0, "deal_type_ind" := 10L]  # Sale only
  purch[deal_flag_uc == 1 & coupon_value > 0, "deal_type_ind" := 100L] # Sale and/or coupon

  purch <- purch[, .(quantity = sum(quantity),
                     total_price_paid = sum(total_price_paid - coupon_value),
                     deal_type_ind = sum(deal_type_ind)),
                 by = .(upc, upc_ver_uc, trip_code_uc)]
  purch[, "deal_type" := cut(deal_type_ind, c(0, 9, 99, 999),
                             c("No Deal", "Sale Only", "Sale and/or Coupon"))]
  purch[, "deal_type_ind" := NULL]

  trips <- fread(paste0(path, "Trips/trips", yr, ".csv"))
  fullData <- merge(purch, trips, by = "trip_code_uc")
  fullData <- merge(fullData, prod, by = c("upc", "upc_ver_uc"))
  fullData <- fullData[, .(deal_type, household_code, panel_year, brand_code_uc)]
  return(fullData)
}

fullData <- rbindlist(map(yr, getPurch))
fullData[, "generic" := ifelse(brand_code_uc == 536746, 1L, 0L)]
fullData[, "brand_code_uc" := NULL]
fullData <- merge(fullData, panel, by = c("household_code", "panel_year"))
rm(prod, panel)

# Running regressions
x <- "as.factor(household_income)"
controls <- paste0("panel_year + market + household_size + ",
                   "type_of_residence + marital_status + white + hispanic_origin + ",
                   "age + urban + college")
cluster <- "market"

# Sales
fullData[, "sale" := ifelse(deal_type == "Sale Only", 1L, 0L)]
summary(runReg(x, y = "sale", fullData, controls, cluster, weights = fullData$projection_factor))
fullData[, "sale" := NULL]

# Coupons
fullData[, "coupon" := ifelse(deal_type == "Sale and/or Coupon", 1L, 0L)]
summary(runReg(x, y = "coupon", fullData, controls, cluster, weights = fullData$projection_factor))
fullData[, "coupon" := NULL]

# Any Deal
fullData[, "anyDeal" := ifelse(deal_type != "No Deal", 1L, 0L)]
summary(runReg(x, y = "anyDeal", fullData, controls, cluster, weights = fullData$projection_factor))
fullData[, "anyDeal" := NULL]

#Generics
summary(runReg(x, y = "generic", fullData, controls, cluster, weights = fullData$projection_factor))
fullData[, "generic" := NULL]
#stargazer(reg1, reg2, reg3, reg4, type = "text")
