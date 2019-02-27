# Estimates mixed logit model for TP choice. Restricting to HHs that only purchase 1 package
# of a major brand (CTL BR, Charmin, Qltd Ntn, Angel Soft, Kleenex, or Scott) [~86% of sales]
# Merging with store assortments to get full choice set for households. Only
# focusing on sizes of 4, 6, 12, or 24 rolls.
library(data.table)
library(bit64)
library(lubridate)
library(knitr)
path <- "/scratch/upenn/hossaine/"
threads <- 8
yr <- 2006:2016
diffTol <- 0 # Tolerance for generic price mismatch. 0 is the strictest and minDiff is the loosest
brands <- c("CTL BR", "CHARMIN", "ANGEL SOFT", "QUILTED NORTHERN", "KLEENEX COTTONELLE", "SCOTT 1000")
brandCode <- c(536746, 526996, 506045, 624459, 581898, 635074)

# Quickly summarize the data and where observations get dropped
tpPurch <- fread(paste0(path, "7260Purch.csv"), nThread = threads)[drop == 0]
tpPurch[, "brand_code_uc" := as.integer(substr(brand_code_uc, 1, 6))]
row1 <- c("Starting", uniqueN(tpPurch$trip_code_uc), uniqueN(tpPurch$household_code))

# Dropping products that cannot be standardized
tpPurch <- na.omit(tpPurch, cols = "size")
row2 <- c("Cannot be standardized", uniqueN(tpPurch$trip_code_uc), uniqueN(tpPurch$household_code))

# Dropping stores without a store_code_uc
tpPurch <- tpPurch[store_code_uc != 0]
row3 <- c("Dropping stores without ID", uniqueN(tpPurch$trip_code_uc), uniqueN(tpPurch$household_code))

# Adding variables to enable merging with the retail scanner data
tpPurch[, "generic" := ifelse(brand_descr == "CTL BR", 1L, 0L)]
tpPurch[, "purchase_date" := as.Date(purchase_date, format = "%Y-%m-%d")]
tpPurch[, "week_end" := ceiling_date(purchase_date, "week",
                                     week_start = getOption("lubridate.week.start", 6))]
tpPurch[, "purchase_date" := NULL]
tpPurch[, "week_end" := as.character(week_end)]
tpPurch[, "week_end" := as.integer(gsub("-", "", week_end))]
tpPurch[, "pCents" := as.integer(round(total_price_paid * 100))]
tpPurch[, "unitCost" := as.integer(round(pCents / totVol))]

fullChoice <- NULL
fullTrips <- NULL
fullPanel <- NULL
for (i in yr) {
  print(i)
  # Getting master lists
  trips <- unique(tpPurch[panel_year == i, .(trip_code_uc, household_code, panel_year)])
  panel <- unique(tpPurch[panel_year == i, .(household_code, panel_year, projection_factor,
                                             household_income, household_size,
                                             type_of_residence, marital_status,
                                             hispanic_origin, market, age, college,
                                             white, child, rate)])
  purch <- unique(tpPurch[panel_year == i, .(trip_code_uc,
                                             choice_upc = upc,
                                             choice_upc_ver_uc = upc_ver_uc,
                                             store_code_uc, week_end, generic)])

  # Getting store assortment
  assort <- fread(paste0(path, "Assortment/", i, ".csv"), nThread = threads)
  assort[, "unitCost" := round(pCents / size)]

  # Remove NA unit costs
  assort <- na.omit(assort, cols = "unitCost")

  # Combining with non-generic purchases. Can only match about 50% of stores and
  # only about 35% of shopping trips in 2010 for non-generics
  nonGeneric <- purch[generic != 1]
  choiceLong <- merge(nonGeneric, assort, by = c("store_code_uc", "week_end"),
                      allow.cartesian = TRUE)
  choiceLong[, "choice" := ifelse(choice_upc == upc, TRUE, FALSE)]
  choiceLong[, "match" := sum(choice), by = trip_code_uc]

  # For some reason, there are instances where the product purchased is not
  # recorded in the scanner data. In these cases, I input the data from the
  # purchase occasion.
  noMatch <- unique(choiceLong[match == 0]$trip_code_uc)
  newDat <- tpPurch[trip_code_uc %in% noMatch,
                    .(store_code_uc, week_end, trip_code_uc, choice_upc = upc,
                      choice_upc_ver_uc = upc_ver_uc, generic, upc, upc_ver_uc,
                      pCents, brand_code_uc, pkgSize = sizeUnadj, ply, size,
                      unitCost, choice = TRUE, match = 1)]
  choiceLong <- rbind(choiceLong, newDat, use.names = TRUE)
  choiceLong[, "match" := NULL]
  choiceLong[, "match" := sum(choice), by = trip_code_uc]
  choiceLong[, c("choice_upc", "choice_upc_ver_uc", "generic", "match") := NULL]

  # Adding generic purchases
  # As with non-generics, I can only match about 50% of stores and about 50% of trips in 2010
  # I match primary based on matching the ply, package size, and ply of the generic product
  # For unmatched trips, these are primarily due to mismatches in price (difference
  # between avg weekly price and price reported by household), I assign the chosen
  # product to the one with the minimum difference between price reported and Scanner price
  # About 25% of generic purchases don't match exactly. However, given a positive
  # discrepancy, the median discrepancy is 12 cents with 75% of discrepancies being
  # below 50 cents, and 90% being below 1 dollar.
  # In cases where there are multiple possibilities, I choose the option with the
  # closes unit cost.
  genericTrips <- purch[generic == 1]$trip_code_uc
  generic <- tpPurch[trip_code_uc %in% genericTrips,
                     .(choice_ply = ply, pCents_choice = pCents,
                       choice_size = sizeUnadj, choice_unitCost = unitCost,
                       store_code_uc, trip_code_uc, week_end, coupon)]
  genericChoice <- merge(generic, assort, by = c("store_code_uc", "week_end"),
                         allow.cartesian = TRUE)
  genericChoice[, "diff" := abs(pCents_choice - pCents)]
  genericChoice[, "possible" := ifelse(brand_code_uc == 536746 &
                                         choice_ply == ply &
                                         choice_size == pkgSize, TRUE, FALSE)]
  genericChoice[possible == TRUE, "minDiff" := min(diff), by = trip_code_uc]
  genericChoice[, "choice" := ifelse(possible == TRUE & diff <= diffTol, TRUE, FALSE)]
  genericChoice[, c("diff", "possible", "minDiff") := NULL]
  multiples <- genericChoice[, .(choice = sum(choice)), by = trip_code_uc][choice > 1]$trip_code_uc
  genericChoice[, "diff" := abs(choice_unitCost - unitCost)]
  genericChoice[, "possible" := ifelse(brand_code_uc == 536746 &
                                         choice_ply == ply &
                                         choice_size == pkgSize, TRUE, FALSE)]
  genericChoice[possible == TRUE, "minDiff" := min(diff), by = trip_code_uc]
  genericChoice[trip_code_uc %in% multiples & possible == TRUE, "choice" := ifelse(diff <= minDiff + 0.01, TRUE, FALSE)]
  genericChoice[, "match" := sum(choice), by = trip_code_uc]
  genericChoice[, c("choice_ply", "pCents_choice", "choice_size", "choice_unitCost",
                    "coupon", "diff", "possible", "minDiff") := NULL]

  # For some reason, there are instances where the product purchased is not
  # recorded in the scanner data. For generics, there are a variety of explanations
  # To fix this, I just add in the record from the consumer panel
  noMatch <- unique(genericChoice[match == 0]$trip_code_uc)
  newDat <- tpPurch[trip_code_uc %in% noMatch,
                    .(store_code_uc, week_end, trip_code_uc, upc, upc_ver_uc,
                      pCents, brand_code_uc, pkgSize = sizeUnadj, ply, size,
                      unitCost, choice = TRUE, match = 1)]
  genericChoice <- rbind(genericChoice, newDat, use.names = TRUE)
  genericChoice[, "match" := NULL]
  genericChoice[, "match" := sum(choice), by = trip_code_uc]
  genericChoice[, c("match") := NULL]

  # Combining choice sets and expanding to full possibility set
  fullChoice <- rbindlist(list(fullChoice, choiceLong, genericChoice), use.names = TRUE)
  fullTrips <- rbindlist(list(fullTrips, trips), use.names = TRUE)
  fullPanel <- rbindlist(list(fullPanel, panel), use.names = TRUE)
}

fullChoice <- merge(fullChoice, fullTrips, by = "trip_code_uc")

# Matched to Retail Scanner Data
row4 <- c("Matched to Scanner", uniqueN(fullChoice$trip_code_uc), uniqueN(fullChoice$household_code))

# Identifying purchases of the top 5 brands and in the 4 popular sizes
fullChoice[, "rightBrand" := ifelse(brand_code_uc %in% brandCode, 1L, 0L)]
fullChoice[, "rightSize" := ifelse(pkgSize %in% c(4, 6, 12, 24), 1L, 0L)]
fullChoice[, "focusSet" := rightBrand * rightSize]
fullChoice <- fullChoice[focusSet == 1]
fullChoice[, c("rightBrand", "rightSize", "focusSet") := NULL]
madePurch <- fullChoice[, .(madePurch = sum(choice)), by = trip_code_uc][madePurch > 0]$trip_code_uc
fullChoice <- fullChoice[trip_code_uc %in% madePurch]
row5 <- c("Restrict to top brand/size combos", uniqueN(fullChoice$trip_code_uc),
          uniqueN(fullChoice$household_code))

# Only single-unit purchases
unitCount <- fullChoice[, .(quantity = sum(choice)), by = trip_code_uc][quantity == 1]$trip_code_uc
fullChoice <- fullChoice[trip_code_uc %in% unitCount]
row6 <- c("Single packages", uniqueN(fullChoice$trip_code_uc), uniqueN(fullChoice$household_code))

cleanTable <- as.data.table(rbind(row1, row2, row3, row4, row5, row6))
setnames(cleanTable, c("Step", "Obs", "HH"))
cleanTable[, c("Obs", "HH") := .(as.integer(Obs), as.integer(HH))]
kable(cleanTable, format = "markdown", format.args = list(big.mark = ","))

# Assigning choice id's
sizes <- c(4, 6, 12, 24)
tripCodes <- unique(fullChoice$trip_code_uc)
fullSet <- expand.grid(brand_code_uc = brandCode, pkgSize = sizes, trip_code_uc = tripCodes)
fullSet <- cbind(alt = 1:24, fullSet)

# Getting selection
fullChoice <- merge(fullChoice, fullSet, by = c("trip_code_uc", "brand_code_uc", "pkgSize"))

# Adding in demographics and making income and household size continuous
fullChoice[, c("p", "pCents") := .(pCents / 100, NULL)]
fullChoice[, "unitCost" := unitCost / 100]
fullPanel[, "household_income_cts" :=
            factor(household_income, levels = c(8, 10, 11, 13, 15:19, 21, 23, 26, 27),
                    labels = c("11", "13.5", "17.5", "22.5", "27.5", "32.5",
                               "37.5", "42.5", "47.5", "55", "65", "85", "100"))]
fullPanel[, "household_income_cts" := as.numeric(as.character(household_income_cts))]

fullChoice <- merge(fullChoice, fullPanel, by = c("household_code", "panel_year"))

# Saving data
fwrite(fullChoice, "/scratch/upenn/hossaine/TPMLogit.csv", nThread = threads)
