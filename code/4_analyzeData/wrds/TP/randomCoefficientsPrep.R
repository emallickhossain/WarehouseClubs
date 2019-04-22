# Estimates mixed logit model for TP choice. Restricting to HHs that only purchase 1 package
# of a major brand (CTL BR, Charmin, Qltd Ntn, Angel Soft, Kleenex, or Scott) [~86% of sales]
# Merging with store assortments to get full choice set for households. Only
# focusing on sizes of 4, 6, 12, or 24 rolls.
library(data.table)
library(bit64)
library(lubridate)
library(knitr)
library(stargazer)
path <- "/scratch/upenn/hossaine/"
threads <- 8
yr <- 2006:2014
diffTol <- 0 # Tolerance for generic price mismatch. 0 is the strictest and minDiff is the loosest
top5 <- c("CHARMIN", "ANGEL SOFT", "QUILTED NORTHERN", "KLEENEX COTTONELLE", "SCOTT 1000")
brandCode <- c(526996, 506045, 624459, 581898, 635074)

# Quickly summarize the data and where observations get dropped
tpPurch <- fread(paste0(path, "7260Purch.csv"), nThread = threads)[drop %in% c(0, 2) & panel_year %in% yr]
tpPurch[, "brand_code_uc" := as.integer(substr(brand_code_uc, 1, 6))]
row1 <- c("Starting", uniqueN(tpPurch$trip_code_uc), uniqueN(tpPurch$household_code))

# Dropping products that cannot be standardized
tpPurch <- na.omit(tpPurch, cols = "size")
row2 <- c("Cannot be standardized", uniqueN(tpPurch$trip_code_uc), uniqueN(tpPurch$household_code))

# Only single purchase trips
multiUPC <- tpPurch[, .(count = .N), by = trip_code_uc][count > 1]$trip_code_uc
tpPurch <- tpPurch[!trip_code_uc %in% multiUPC]
tpPurch <- tpPurch[quantity == 1]
row3 <- c("Single purchases only", uniqueN(tpPurch$trip_code_uc), uniqueN(tpPurch$household_code))

# Adding variables to enable merging with the retail scanner data
tpPurch[, "purchase_date" := as.Date(purchase_date, format = "%Y-%m-%d")]
tpPurch[, "week_end" := ceiling_date(purchase_date, "week",
                                     week_start = getOption("lubridate.week.start", 6))]
tpPurch[, "week_end" := as.character(week_end)]
tpPurch[, "week_end" := as.integer(gsub("-", "", week_end))]
tpPurch[, "pCents" := as.integer(round(total_price_paid * 100))]
tpPurch[, "unitCost" := as.integer(round(pCents / totVol))]

setnames(tpPurch, "dma_cd", "dma_code")

fullChoice <- NULL
fullTrips <- NULL
fullPanel <- NULL

#############################################################################
################### MATCHING EXACTLY ON STORE ID ############################
#############################################################################
for (i in yr) {
  print(i)
  # Getting master lists
  trips <- unique(tpPurch[panel_year == i, .(trip_code_uc, household_code, panel_year,
                                             total_spent, cumSpend)])
  panel <- unique(tpPurch[panel_year == i, .(household_code, panel_year, projection_factor,
                                             household_income, type_of_residence,
                                             market, college, white, rate)])
  purch <- unique(tpPurch[panel_year == i, .(trip_code_uc, week_end, store_code_uc,
                                             choice_upc = upc,
                                             choice_upc_ver_uc = upc_ver_uc,
                                             choice_brand = brand_code_uc,
                                             choice_size = sizeUnadj,
                                             choice_ply = ply,
                                             choice_pCents = pCents,
                                             choice_unitCost = unitCost)])
  setkey(purch, store_code_uc, week_end)

  # Getting store assortment and remove NA unit costs
  assort <- fread(paste0(path, "Assortment/", i, ".csv"), nThread = threads,
                  key = c("store_code_uc", "week_end"))
  assort[, ':=' (unitCost = round(pCents / size))]
  assort <- na.omit(assort, cols = "unitCost")

  # Matching purchases with store assortments and getting exact matches (group 1)
  choiceLong <- merge(purch, assort, by = c("store_code_uc", "week_end"))
  choiceLong[, c("retailer_code", "dma_code", "fips_state_code") := NULL]
  choiceLong[, "choice" := ifelse(choice_upc == upc, TRUE, FALSE)]
  choiceLong[, "match" := sum(choice), by = trip_code_uc]
  exactMatch <- choiceLong[match == 1][, "matchQual" := "Exact"]
  exactMatch[, c("choice_upc", "choice_upc_ver_uc", "choice_brand", "choice_size",
                 "choice_ply", "choice_pCents", "choice_unitCost", "units") := NULL]

  # Approximating the match for other purchases based on brand, ply, and size.
  # To ensure a close match, I set a cutoff for how large the price difference
  # can be. Most of these cases are small differences in the assigned UPC, but
  # not the underlying product characteristics. In cases where I have multiple
  # exact matches, I collapse these to a single choice because these are the
  # dimensions I care about and a small UPC difference is not detectable in
  # my model (group 2)
  approxMatch <- choiceLong[match == 0][, "match" := NULL]
  approxMatch[, "diffPrice" := abs(choice_pCents - pCents)]
  approxMatch[, "diffUnitCost" := abs(choice_unitCost - unitCost)]
  approxMatch[, "possible" := ifelse(choice_brand == brand_code_uc &
                                     choice_ply == ply &
                                     choice_size == pkgSize, TRUE, FALSE)]
  approxMatch[possible == TRUE, "minDiffPrice" := min(diffPrice), by = trip_code_uc]
  approxMatch[possible == TRUE, "minDiffUnitCost" := min(diffUnitCost, na.rm = TRUE), by = trip_code_uc]
  approxMatch[, "choice" := ifelse(possible == TRUE &
                                   diffPrice == minDiffPrice &
                                   diffUnitCost == minDiffUnitCost &
                                   diffPrice <= diffTol, TRUE, FALSE)]
  approxMatch[, "match" := sum(choice), by = trip_code_uc]
  choiceUPC <- approxMatch[match > 1 & choice == TRUE, .(upc = head(upc, 1)), by = trip_code_uc]$upc
  approxMatch[match > 1 & choice == TRUE & !upc %in% choiceUPC, "choice" := FALSE]
  approxMatch[, "match" := sum(choice), by = trip_code_uc]
  approxMatch1 <- approxMatch[match == 1][, "matchQual" := "Approx"]
  approxMatch1[, c("choice_brand", "choice_size", "choice_ply", "choice_pCents",
                   "choice_unitCost", "diffPrice", "diffUnitCost", "possible",
                   "minDiffPrice", "minDiffUnitCost","choice_upc",
                   "choice_upc_ver_uc", "units") := NULL]

  # For some reason, there are instances where the product purchased is not
  # recorded in the scanner data. In these cases, I input the data from the
  # purchase occasion.
  noMatch <- approxMatch[match == 0, .(week_end, trip_code_uc, upc, upc_ver_uc,
                                       pCents, brand_code_uc, pkgSize, ply,
                                       size, unitCost, choice, store_code_uc)]
  noMatchID <- unique(noMatch$trip_code_uc)
  newDat <- tpPurch[trip_code_uc %in% noMatchID,
                    .(week_end, trip_code_uc, upc, upc_ver_uc, pCents, store_code_uc,
                      brand_code_uc, pkgSize = sizeUnadj, ply, size, unitCost,
                      choice = TRUE)]
  noMatch <- rbindlist(list(noMatch, newDat), use.names = TRUE)
  noMatch[, "match" := sum(choice), by = trip_code_uc]
  noMatch[, "matchQual" := "Append"]

  # Combining exact store id matches
  combineData <- rbindlist(list(exactMatch, approxMatch1, noMatch), use.names = TRUE)
  print(prop.table(table(unique(combineData[, .(trip_code_uc, matchQual)])$matchQual)))

  fullChoice <- rbindlist(list(fullChoice, combineData), use.names = TRUE)

  # Combining choice sets and expanding to full possibility set
  fullTrips <- rbindlist(list(fullTrips, trips), use.names = TRUE)
  fullPanel <- rbindlist(list(fullPanel, panel), use.names = TRUE)
}

##############################################################################
############################ MATCHING BY RETAILER-WEEK #######################
##############################################################################
matchedTrips <- unique(fullChoice$trip_code_uc)
for (i in yr) {
  print(i)
  # Getting master lists
  purch <- unique(tpPurch[panel_year == i & !trip_code_uc %in% matchedTrips,
                          .(trip_code_uc, week_end, retailer_code,
                            choice_upc = upc,
                            choice_upc_ver_uc = upc_ver_uc,
                            choice_brand = brand_code_uc,
                            choice_size = sizeUnadj,
                            choice_ply = ply,
                            choice_pCents = pCents,
                            choice_unitCost = unitCost)])

  # Getting full assortment for a retailer and remove NA unit costs
  assort <- fread(paste0(path, "Assortment/", i, ".csv"), nThread = threads)
  assort[, ':=' (unitCost = round(pCents / size))]
  assort <- na.omit(assort, cols = c("unitCost", "retailer_code"))
  retAssort <- assort[, .(pCents = weighted.mean(pCents, w = units),
                          unitCost = weighted.mean(unitCost, w = units)),
                      by = .(retailer_code, upc, upc_ver_uc, week_end,
                             brand_code_uc, pkgSize, ply, size)]

  # Matching purchases with store assortments and getting exact matches (group 1)
  choiceLong <- merge(purch, retAssort, by = c("retailer_code", "week_end"), allow.cartesian = TRUE)
  choiceLong[, "choice" := ifelse(choice_upc == upc, TRUE, FALSE)]
  choiceLong[, "match" := sum(choice), by = trip_code_uc]
  exactMatch <- choiceLong[match == 1][, "matchQual" := "RetExact"]
  exactMatch[, c("choice_upc", "choice_upc_ver_uc", "choice_brand", "choice_size",
                 "choice_ply", "choice_pCents", "choice_unitCost", "retailer_code") := NULL]

  # Approximating the match for other purchases based on brand, ply, and size.
  # To ensure a close match, I set a cutoff for how large the price difference
  # can be. Most of these cases are small differences in the assigned UPC, but
  # not the underlying product characteristics. In cases where I have multiple
  # exact matches, I collapse these to a single choice because these are the
  # dimensions I care about and a small UPC difference is not detectable in
  # my model (group 2)
  approxMatch <- choiceLong[match == 0][, "match" := NULL]
  approxMatch[, "diffPrice" := abs(choice_pCents - pCents)]
  approxMatch[, "diffUnitCost" := abs(choice_unitCost - unitCost)]
  approxMatch[, "possible" := ifelse(choice_brand == brand_code_uc &
                                       choice_ply == ply &
                                       choice_size == pkgSize, TRUE, FALSE)]
  approxMatch[possible == TRUE, "minDiffPrice" := min(diffPrice), by = trip_code_uc]
  approxMatch[possible == TRUE, "minDiffUnitCost" := min(diffUnitCost, na.rm = TRUE), by = trip_code_uc]
  approxMatch[, "choice" := ifelse(possible == TRUE &
                                     diffPrice == minDiffPrice &
                                     diffUnitCost == minDiffUnitCost &
                                     diffPrice <= diffTol, TRUE, FALSE)]
  approxMatch[, "match" := sum(choice), by = trip_code_uc]

  if (nrow(approxMatch[match > 1]) > 0) {
    choiceUPC <- approxMatch[match > 1 & choice == TRUE, .(upc = head(upc, 1)), by = trip_code_uc]$upc
    approxMatch[match > 1 & choice == TRUE & !upc %in% choiceUPC, "choice" := FALSE]
    approxMatch[, "match" := sum(choice), by = trip_code_uc]
  }

  approxMatch1 <- approxMatch[match == 1][, "matchQual" := "RetApprox"]
  approxMatch1[, c("choice_brand", "choice_size", "choice_ply", "choice_pCents",
                   "choice_unitCost", "diffPrice", "diffUnitCost", "possible",
                   "minDiffPrice", "minDiffUnitCost", "choice_upc",
                   "choice_upc_ver_uc", "retailer_code") := NULL]

  # For some reason, there are instances where the product purchased is not
  # recorded in the scanner data. In these cases, I input the data from the
  # purchase occasion.
  noMatch <- approxMatch[match == 0, .(week_end, trip_code_uc, upc, upc_ver_uc,
                                       pCents, brand_code_uc, pkgSize, ply,
                                       size, unitCost, choice)]
  noMatchID <- unique(noMatch$trip_code_uc)
  newDat <- tpPurch[trip_code_uc %in% noMatchID,
                    .(week_end, trip_code_uc, upc, upc_ver_uc, pCents,
                      brand_code_uc, pkgSize = sizeUnadj, ply, size, unitCost,
                      choice = TRUE)]
  noMatch <- rbindlist(list(noMatch, newDat), use.names = TRUE)
  noMatch[, "match" := sum(choice), by = trip_code_uc]
  noMatch[, "matchQual" := "RetAppend"]

  # Combining exact store id matches
  fullChoice <- rbindlist(list(fullChoice, exactMatch, approxMatch1, noMatch), use.names = TRUE)
}

masterChoice <- fullChoice
fullTrips <- unique(fullTrips)
masterChoice <- merge(masterChoice, fullTrips, by = c("trip_code_uc"))

# Matched to Retail Scanner Data
row4 <- c("Matched to Scanner", uniqueN(masterChoice$trip_code_uc), uniqueN(masterChoice$household_code))

# Identifying purchases of the top 5 brands
masterChoice[, "topBrand" := ifelse(brand_code_uc %in% brandCode, brand_code_uc, 536746)]
madePurch <- masterChoice[, .(madePurch = sum(choice)), by = trip_code_uc][madePurch > 0]$trip_code_uc
masterChoice <- masterChoice[trip_code_uc %in% madePurch]

# Dropping packages that are outside the interval [0.75, 30] (<1% of options)
masterChoice <- masterChoice[pCents >= 75 & pCents <= 3000]
row5 <- c("Excess Prices", uniqueN(masterChoice$trip_code_uc), uniqueN(masterChoice$household_code))

cleanTable <- as.data.table(rbind(row1, row2, row3, row4, row5))
setnames(cleanTable, c("Step", "Obs", "HH"))
cleanTable[, c("Obs", "HH") := .(as.integer(Obs), as.integer(HH))]
stargazer(cleanTable, type = "text", summary = FALSE,
          title = "Scanner Cleaning and Merging Steps",
          label = "tab:scannerClean", digits = 2, rownames = FALSE,
          out = "tables/scannerClean.tex")

# Assigning choice id's
tripCodes <- unique(masterChoice$trip_code_uc)
fullSet <- expand.grid(topBrand = c(brandCode, 536746),
                       pkgSize = c(4, 6, 12, 24),
                       trip_code_uc = tripCodes)
fullSet <- cbind(alt = 1:24, fullSet)

# Getting selection
masterChoice <- merge(masterChoice, fullSet,
                      by = c("trip_code_uc", "topBrand", "pkgSize"), all.y = TRUE)

# Adding in demographics and making income and household size continuous
fullPanel[, "household_income_cts" :=
            factor(household_income, levels = c(3, 4, 6, 8, 10, 11, 13, 15:19, 21, 23, 26, 27),
                    labels = c("2.5", "6.5", "9", "11", "13.5", "17.5", "22.5",
                               "27.5", "32.5", "37.5", "42.5", "47.5", "55",
                               "65", "85", "100"))]
fullPanel[, "household_income_cts" := as.numeric(as.character(household_income_cts))]
masterChoice <- merge(masterChoice, fullPanel, by = c("household_code", "panel_year"))

# Adding size categories
masterChoice[, "sizeCat" := cut(pkgSize, c(0, 6, 12, 100), c("small", "medium", "large"))]

# Table of available product assortments
stores <- c("Grocery", "Discount Store")
fullChoice <- na.omit(masterChoice, cols = "household_code")
tripAssort <- fullChoice[, .(sizeAssort = uniqueN(sizeCat),
                             brandAssort = uniqueN(topBrand),
                             altAssort = uniqueN(alt)),
                         by = trip_code_uc]
trips <- fread("/scratch/upenn/hossaine/trips.csv", nThread = threads,
               select = c("trip_code_uc", "store_code_uc", "retailer_code"))
tripAssort <- merge(tripAssort, trips, by = "trip_code_uc")
retailers <- fread("/scratch/upenn/hossaine/retailers.csv")
tripAssort <- merge(tripAssort, retailers, by = "retailer_code")
tripAssort <- tripAssort[channel_type %in% stores]
for (i in stores) {
  print(i)
  print(quantile(tripAssort[channel_type == i]$altAssort, seq(0, 1, 0.01)))
}

masterChoice[, c("ply", "match", "matchQual", "brand_code_uc", "panel_year", "upc_ver_uc") := NULL]
masterChoice[, "topBrand" := as.integer(topBrand)]
masterChoice[, "pkgSize" := as.integer(pkgSize)]

# Saving data
fwrite(masterChoice, "/scratch/upenn/hossaine/TPMLogit.csv", nThread = threads)
# cd /scratch/upenn/hossaine
# tar -cvzf TPMLogit.tgz TPMLogit.csv
# cp TPMLogit.tgz /home/upenn/hossaine/TPMLogit.tgz
