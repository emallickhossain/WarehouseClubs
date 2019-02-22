# Estimates mixed logit model for TP choice (ONLY 2010)
library(data.table)
library(bit64)
library(lubridate)
library(purrr)
library(mlogit)
path <- "/scratch/upenn/hossaine/"
yr <- 2006:2016
diffTol <- 0 # Tolerance for generic price mismatch. 0 is the strictest and minDiff is the loosest

# Getting product master list
tpPurch <- fread(paste0(path, "7260Purch.csv"))
tpPurch[, "generic" := ifelse(brand_descr == "CTL BR", 1L, 0L)]
tpPurch[, "purchase_date" := as.Date(purchase_date, format = "%Y-%m-%d")]
tpPurch[, "week_end" := ceiling_date(purchase_date, "week",
                                     week_start = getOption("lubridate.week.start", 6))]
tpPurch[, "purchase_date" := NULL]
tpPurch[, "week_end" := as.character(week_end)]
tpPurch[, "week_end" := as.integer(gsub("-", "", week_end))]
tpPurch[, "pCents" := as.integer(round(total_price_paid * 100))]
tpPurch[, "unitCost" := as.integer(round(unitCost * 100))]

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
                                             white, child)])
  purch <- unique(tpPurch[panel_year == i, .(trip_code_uc,
                                             choice_upc = upc,
                                             choice_upc_ver_uc = upc_ver_uc,
                                             store_code_uc, week_end, generic)])

  # Getting store assortment
  assort <- fread(paste0(path, "Assortment/", i, ".csv"))

  # Remove NA unit costs
  assort <- na.omit(assort, cols = "unitCost")

  # Categorizing brands to top and other to reduce choice set
  assort[, "pkgSizeBin" := cut(pkgSize, c(0, 4, 6, 9, 12, 20, 24, 100),
                               c("1-4", "5-6", "7-9", "10-12", "13-20", "21-24", "25+"))]

  assort[brand_descr %in% c("CHARMIN", "CHARIN BASIC", "CHARMIN ESSENTIALS",
                            "CHARMIN PLUS", "CHARMIN SCENTS", "CHARMIN SENSITIVE",
                            "CHARMIN ULTRA SCENTS"), "brandBin" := "CHARMIN"]
  assort[brand_descr %in% c("COTTONELLE", "KLEENEX COTTONELLE"), "brandBin" := "COTTONELLE"]
  assort[brand_descr %in% c("QUILTED NORTHERN"), "brandBin" := "QLTD NTN"]
  assort[brand_descr %in% c("ANGEL SOFT"), "brandBin" := "ANGEL SOFT"]
  assort[brand_descr %in% c("SCOTT", "SCOTT 1000", "SCOTT EXTRA SOFT",
                            "SCOTT NATURALS", "SCOTT RAPID DISSOLVING"), "brandBin" := "SCOTT"]
  assort[is.na(brandBin), "brandBin" := "OTHER"]

  # Combining with non-generic purchases. Can only match about 50% of stores and
  # only about 35% of shopping trips in 2010 for non-generics
  nonGeneric <- purch[generic != 1]
  choiceLong <- merge(nonGeneric, assort, by = c("store_code_uc", "week_end"),
                      allow.cartesian = TRUE)
  choiceLong[, "choice" := ifelse(choice_upc == upc, TRUE, FALSE)]
  choiceLong[, "match" := sum(choice), by = trip_code_uc]
  choiceLong <- choiceLong[match == 1]
  choiceLong <- choiceLong[, .(unitCost = weighted.mean(unitCost, w = pkgSize),
                               pCents = weighted.mean(pCents, w = pkgSize),
                               choice = sum(choice),
                               ply = weighted.mean(ply, w = pkgSize),
                               pkgSize = weighted.mean(pkgSize, w = pkgSize)),
                           by = .(trip_code_uc, pkgSizeBin, brandBin)]

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
  genericChoice <- genericChoice[match == 1]

  genericChoice <- genericChoice[, .(unitCost = weighted.mean(unitCost, w = pkgSize),
                                     pCents = weighted.mean(pCents, w = pkgSize),
                                     choice = sum(choice),
                                     ply = weighted.mean(ply, w = pkgSize),
                                     pkgSize = weighted.mean(pkgSize, w = pkgSize)),
                                 by = .(trip_code_uc, pkgSizeBin, brandBin)]

  # Combining choice sets and expanding to full possibility set
  fullChoice <- rbindlist(list(fullChoice, choiceLong, genericChoice), use.names = TRUE)
  fullTrips <- rbindlist(list(fullTrips, trips), use.names = TRUE)
  fullPanel <- rbindlist(list(fullPanel, panel), use.names = TRUE)
}

# Generating full choice set and making unavailable products really expensive
brands <- c("CHARMIN", "COTTONELLE", "QLTD NTN", "ANGEL SOFT", "SCOTT", "OTHER")
sizes <- c("1-4", "5-6", "7-9", "10-12", "13-20", "21-24", "25+")
tripCodes <- unique(fullChoice$trip_code_uc)
fullSet <- expand.grid(brandBin = brands, pkgSizeBin = sizes, trip_code_uc = tripCodes)
fullSet <- cbind(id = 1:42, fullSet)

# Getting selection
fullChoice <- merge(fullChoice, fullSet, by = c("trip_code_uc", "brandBin", "pkgSizeBin"))

# # If selection has to be balanced
# fullChoice <- merge(fullChoice, fullSet,
#                     by = c("trip_code_uc", "brandBin", "pkgSizeBin"), all.y = TRUE)
# fullChoice[, c("lower", "upper") := tstrsplit(pkgSizeBin, "-")]
# fullChoice[is.na(choice), ':=' (unitCost = 999.0,
#                                 pCents = 99999.0,
#                                 choice = 0L,
#                                 ply = 2.0,
#                                 pkgSize = as.numeric(upper))]
# fullChoice[is.na(pkgSize), "pkgSize" := 30]
# fullChoice[, c("lower", "upper") := NULL]

# Adding in demographics and making income and household size continuous
fullChoice <- merge(fullChoice, fullTrips, by = "trip_code_uc")
fullChoice[, "p" := pCents / 100]
fullChoice[, "pCents" := NULL]
fullChoice[, "household_income_cts" :=
             factor(household_income, levels = c(8, 10, 11, 13, 15:19, 21, 23, 26, 27),
                    labels = c("11", "13.5", "17.5", "22.5", "27.5", "32.5",
                               "37.5", "42.5", "47.5", "55", "65", "85", "100"))]
fullChoice[, "household_income_cts" := as.numeric(as.character(household_income_cts))]
fullChoice[, "household_size_cts" := factor(household_size,
                                            levels = c("1", "2", "3", "4", "5+"),
                                            labels = c("1", "2", "3", "4", "5"))]
fullChoice[, "household_size_cts" := as.numeric(as.character(household_size_cts))]
fullChoice <- merge(fullChoice, fullPanel, by = c("household_code", "panel_year"))

# Saving data
fwrite(fullChoice, "/scratch/upenn/hossaine/TPMLogit.csv")
