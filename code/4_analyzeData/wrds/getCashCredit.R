# Computes share of cash and credit usage across households
library(data.table)
library(forcats)
library(purrr)
library(lfe)
path <- "/scratch/upenn/hossaine/"
yr <- 2013:2016
panel <- fread(paste0(path, "fullPanel.csv"))

getData <- function(yr) {
  # Getting Nielsen purchases by trip
  purch <- fread(paste0(path, "Purchases/purchase", yr, ".csv"),
                 select = c("trip_code_uc", "total_price_paid"))
  purch <- purch[, .(spend = sum(total_price_paid)), by = .(trip_code_uc)]

  # Merging trips
  trips <- fread(paste0(path, "Trips/trips", yr, ".csv"),
                 select = c("trip_code_uc", "household_code", "panel_year", "method_of_payment_cd"))
  trips[, "method_of_payment_cd" := fct_collapse(as.factor(method_of_payment_cd),
                                                 none = paste(0),
                                                 cash = paste(c(1, 2, 8)),
                                                 credit = paste(3:7),
                                                 other = paste(9))]
  purch <- merge(trips, purch, by = "trip_code_uc")
  purch[, "trip_code_uc" := NULL]

  finalData <- purch[, .(spend = sum(spend)),
                        by = .(household_code, panel_year, method_of_payment_cd)]
  return(finalData)
}

fullData <- rbindlist(map(yr, getData))
fullDataWide <- dcast(fullData, household_code + panel_year ~ method_of_payment_cd, value.var = "spend")
fullDataWide[is.na(none), "none" := 0]
fullDataWide[is.na(cash), "cash" := 0]
fullDataWide[is.na(credit), "credit" := 0]
fullDataWide[is.na(other), "other" := 0]
fullDataWide[, "total" := none + cash + credit + other]
fullDataWide[, ':=' (creditShare = credit / total * 100,
                     cashShare = cash / total * 100)]
fullDataWide <- merge(fullDataWide, panel, by = c("household_code", "panel_year"))

reg1 <- felm(data = fullDataWide, creditShare ~ household_income |
               panel_year + market | 0 | market,
             weights = fullDataWide$projection_factor)

reg2 <- felm(data = fullDataWide, creditShare ~ household_income |
               panel_year + market + household_size + marital_status + race +
               hispanic_origin + age + urban + college | 0 | market,
             weights = fullDataWide$projection_factor)

# Plotting annual averages
graphData <- fullDataWide[, .(credit = weighted.mean(credit, w = projection_factor),
                              cash = weighted.mean(cash, w = projection_factor),
                              total = weighted.mean(total, w = projection_factor)),
                          by = .(panel_year, household_income)]
graphData[, ':=' (cashShare = cash / total * 100,
                  creditShare = credit / total * 100)]
