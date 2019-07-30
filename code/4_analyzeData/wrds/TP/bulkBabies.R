# Computing TP purchase changes for new parents compared to non-parents
library(data.table)
library(zoo)
library(stargazer)
threads <- 8

panel <- fread("/scratch/upenn/hossaine/fullPanel.csv")
trips <- fread("/scratch/upenn/hossaine/fullTrips.csv", nThread = threads,
               select = c("trip_code_uc", "household_code", "panel_year",
                          "retailer_code", "purchase_date"))

################## EVENT STUDY #################################################
# Event study only on households that experienced a birth during the panel
birthID <- unique(panel[birth == 1]$household_code)
birthData <- panel[household_code %in% birthID]

# Getting birth year
birthYear <- birthData[birth == 1, .(household_code, birthYear = panel_year)]
birthData <- merge(birthData, birthYear, by = "household_code")

# Restricting trips
trips <- trips[household_code %in% birthID]

# Getting bulk purchasing by household-quarter
quarterShares <- NULL
for (i in 2004:2017) {
  print(i)
  purch <- fread(paste0("/scratch/upenn/hossaine/fullPurch", i, ".csv"), nThread = threads,
                 select = c("trip_code_uc", "quantity", "packagePrice", "quintile"))

  # Merging with trips
  purch <- merge(purch, trips, by = "trip_code_uc")
  purch[, "purchase_date" := as.Date(purchase_date)]
  purch[, c("year", "quarter") := .(year(purchase_date), quarter(purchase_date))]
  purch[, "bulk" := ifelse(quintile >= 4, 1L, 0L)]
  purch[, "totalExp" := packagePrice * quantity]

  # Getting expenditure-weighted generic and bulk shares by month
  purch <- purch[, .(bulk = weighted.mean(bulk, w = totalExp)),
                 by = .(household_code, panel_year, year, quarter)]
  purch <- purch[panel_year == year]
  purch[, "year" := NULL]
  quarterShares <- rbindlist(list(quarterShares, purch), use.names = TRUE)
}

quarterShares <- merge(quarterShares, birthData, by = c("household_code", "panel_year"))
quarterShares[, "household_income" := factor(household_income)]
quarterShares[, "YQ" := as.yearqtr(paste(panel_year, quarter, sep = "-"))]
quarterShares[, "birthYQ" := as.yearqtr(paste(birthYear, "1", sep = "-"))]
quarterShares[, "postBirth" := (YQ - birthYQ) * 4]
quarterShares[, "postBirth" := relevel(factor(postBirth), ref = "-1")]

reg <- lm(bulk ~ postBirth, data = quarterShares)
summary(reg)




# Diff in diff (Getting all 2 person married households between 18-45)
analysisGroup <- panel[age <= 45 & age >= 18 & household_size == 2 & married == 1]

# Treated group is households that had a birth and have 3 people
firstBirthID <- panel[birth == 1 & household_size == 3]$household_code
analysisGroup[, "treatedGroup" := (household_code %in% firstBirthID)]

# Control group is other 2 person households with members 18-45
# This helps exclude any separate, but still married households
analysisGroup[, "controlGroup" := (treatedGroup == FALSE &
                                     (member_1_age >= 18 | is.na(member_1_age)))]
analysisGroup <- analysisGroup[treatedGroup == TRUE | controlGroup == TRUE]
