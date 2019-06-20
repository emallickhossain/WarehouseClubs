# Looking at coverage of spending by retail scanner by income group
# Method is to look at share of spending captured at stores with a store_code_uc
# and see if that differs by income group
# Important note is that a store_code_uc is assigned to any store that currently
# or previously has shared data with Nielsen
# Overall, matching seems to be okay with some problems in the upper and lower tail,
# but that's not too surprising.
# The main caveat is that this is only based on whether or not the store_code_uc
# field is populated and not whether it can actually be matched with Scanner data
library(data.table)
library(ggplot2)
threads <- 8
yrs <- 2006:2016

# Getting trips
trips <- fread("/scratch/upenn/hossaine/fullTrips.csv", nThread = threads,
               select = c("store_code_uc", "household_code",
                          "panel_year", "trip_code_uc"))[panel_year >= 2006]

# Getting annual purchases by household
fullPurch <- NULL
for (yr in yrs) {
  print(yr)
  # Getting purchase data and computing unit price paid
  purch <- fread(paste0("/scratch/upenn/hossaine/fullPurch", yr, ".csv"),
                 nThread = threads,
                 select = c("trip_code_uc", "packagePrice", "quantity", "product_module_code"),
                 key = "trip_code_uc")
  purch[, "spend" := packagePrice * quantity]
  purch[, c("packagePrice", "quantity") := NULL]
  purch <- merge(purch, trips, by = "trip_code_uc")
  purch[, "matched" := ifelse(store_code_uc == 0, "Not Matched", "Matched")]

  storeSpending <- purch[, .(spend = sum(spend)),
                         keyby = .(household_code, panel_year, matched)]
  storeSpending[, "total" := sum(spend), by = .(household_code, panel_year)]
  storeSpending[, "share" := spend / total]
  storeSpending[, c("spend", "total") := NULL]

  fullPurch <- rbindlist(list(fullPurch, storeSpending), use.names = TRUE)
}

fullPurchWide <- dcast(fullPurch, household_code + panel_year ~ matched, value.var = "share")
fullPurchWide[is.na(Matched), "Matched" := 0]
fullPurchWide[is.na(`Not Matched`), "Not Matched" := 0]

panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("panel_year", "household_code", "projection_factor",
                          "household_income"),
               key = c("household_code", "panel_year"))

shares <- merge(fullPurchWide, panel, by = c("household_code", "panel_year"))

apple <- shares[, .(Matched = weighted.mean(Matched, w = projection_factor),
                    NotMatched = weighted.mean(`Not Matched`, w = projection_factor)),
                keyby = .(panel_year, household_income)]
ggplot(data = apple, aes(x = household_income, y = Matched, color = as.factor(panel_year))) +
  geom_line()
reg <- lm(Matched ~ as.factor(household_income),
          data = shares, weights = projection_factor)
summary(reg)


######### Specifically for toilet paper
library(data.table)
library(lubridate)
yrs <- 2006:2016
threads <- 8

# Getting trips and adding in week_end date
trips <- fread("/scratch/upenn/hossaine/fullTrips.csv", nThread = threads,
               select = c("store_code_uc", "trip_code_uc", "purchase_date",
                          "household_code", "panel_year"))[store_code_uc != 0]

fullPurch <- NULL
for (yr in yrs) {
  print(yr)
  # Getting purchase data and computing unit price paid
  purch <- fread(paste0("/scratch/upenn/hossaine/fullPurch", yr, ".csv"),
                 nThread = threads,
                 select = c("trip_code_uc", "brand_code_uc", "product_module_code",
                            "packagePrice", "quantity", "totalAmount"),
                 key = "trip_code_uc")[product_module_code == 7260]
  purch <- merge(purch, trips, by = "trip_code_uc")
  purch[, ':=' (unitPriceChosen = packagePrice / totalAmount,
                totalExp = quantity * packagePrice)]
  purch[, c("packagePrice", "quantity", "product_module_code") := NULL]
  purch[, "week_end" := ceiling_date(as.Date(purchase_date), "week", week_start = 6)]
  purch[, c("purchase_date", "trip_code_uc") := NULL]
  purch[, "week_end" := as.integer(gsub("-", "", week_end))]
  fullPurch <- rbindlist(list(fullPurch, purch), use.names = TRUE)
}

storeSpend <- fullPurch[, .(spend = sum(totalExp)),
                        by = .(household_code, panel_year, store_code_uc)]

# Combining with Homescan data
fullTP <- fread("/scratch/upenn/hossaine/fullTP.csv", nThread = threads)
fullTP[, "panel_year" := as.integer(substr(week_end, 1, 4))]
storeMatches <- unique(fullTP[, .(store_code_uc, panel_year, match = 1)])
choices <- merge(storeMatches, fullPurch, by = c("store_code_uc", "panel_year"), all.y = TRUE)
apple <- choices[, .(exp = sum(totalExp)), keyby = .(household_code, panel_year, match)]
apple[is.na(match), "match" := 0]

appleWide <- dcast(apple, household_code + panel_year ~ match, value.var = "exp")
setnames(appleWide, c("0", "1"), c("NotMatched", "Matched"))
appleWide[is.na(NotMatched), "NotMatched" := 0]
appleWide[is.na(Matched), "Matched" := 0]
appleWide[, "share" := Matched / (Matched + NotMatched)]

# Combining with household characteristics
panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("panel_year", "household_code", "projection_factor",
                          "household_income"),
               key = c("household_code", "panel_year"))
hhSavings <- merge(appleWide, panel, by = c("household_code", "panel_year"))

pear <- hhSavings[, .(share = weighted.mean(share, w = projection_factor)),
                  keyby = .(panel_year, household_income)]
ggplot(data = pear, aes(x = household_income, y = share, color = as.factor(panel_year))) +
  geom_line()
reg <- lm(share ~ as.factor(household_income), data = hhSavings, weights = projection_factor)
summary(reg)
