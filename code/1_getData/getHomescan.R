# All transferring, unzipping and cleaning of Homescan data
library(data.table)
library(purrr)
library(stringr)
library(readxl)
library(stargazer)
library(geosphere)
yrs <- 2004:2017
path <- "/scratch/upenn/hossaine/nielsen_extracts/HMS/"
threads <- 8

######################## STEP 1: TRANSFER ######################################
# Transfer homescan data from Globus to WRDS scratch
################################################################################

######################## STEP 2: UNZIP #########################################
# cd /scratch/upenn/hossaine
# tar -xvzf Consumer_Panel_Data_2004_2017.tgz
################################################################################

######################## STEP 3: CLEAN PANELIST FILE ###########################
# Final sample removes any students and military as well as various missing
# entries based on other data combined with the panel data
getPanel <- function(yr) {
  panel <- fread(paste0(path, yr, "/Annual_Files/panelists_", yr, ".tsv"),
                 nThread = threads,
                 select = c("Household_Cd", "Panel_Year", "Projection_Factor",
                            "Household_Income", "Household_Size", "Type_Of_Residence",
                            "Age_And_Presence_Of_Children",
                            "Female_Head_Education", "Male_Head_Education",
                            "Male_Head_Occupation", "Female_Head_Occupation",
                            "Male_Head_Birth", "Female_Head_Birth",
                            "Marital_Status", "Race", "Hispanic_Origin",
                            "Panelist_ZipCd", "Fips_State_Cd", "Fips_County_Cd",
                            "Scantrack_Market_Identifier_Desc", "DMA_Cd", "DMA_Name",
                            "Member_1_Birth", "Member_2_Birth", "Member_3_Birth",
                            "Member_4_Birth", "Member_5_Birth", "Member_6_Birth",
                            "Member_7_Birth"))
  setnames(panel, tolower(names(panel)))
  panel[, "fips" := paste0(str_pad(fips_state_cd, 2, "left", "0"),
                           str_pad(fips_county_cd, 3, "left", "0"))]
  panel[, c("fips_state_cd", "fips_county_cd") := NULL]
  setnames(panel, c("household_cd", "scantrack_market_identifier_desc", "panelist_zipcd"),
           c("household_code", "market", "zip_code"))
  return(panel)
}

# Initial sample
panel <- rbindlist(map(yrs, getPanel))
row1 <- c("Starting HH:", uniqueN(panel$household_code))

# Removing military and students
panel <- panel[!male_head_occupation %in% c(7, 10)]
panel <- panel[!female_head_occupation %in% c(7, 10)]
row2 <- c("Exclude military and students:", uniqueN(panel$household_code))

# Removing households making under $5k
panel <- panel[household_income != 3]
row3 <- c("Exclude Households under 5k:", uniqueN(panel$household_code))

# Adding income factors
panel[, "household_income_coarse" := cut(household_income, c(0, 13, 19, 26, 30),
                                         labels = c("<25k", "25-50k", "50-100k", ">100k"),
                                         ordered_result = TRUE)]
panel[, "household_income" := ifelse(household_income >= 27, 27, household_income)]

# Adding age
panel[, "age" := panel_year - (female_head_birth + male_head_birth) / 2]
panel[is.na(age), "age" := as.numeric(panel_year - female_head_birth)]
panel[is.na(age), "age" := as.numeric(panel_year - male_head_birth)]

# Adding Age of other household members
panel[, ':=' (member_1_age = panel_year - member_1_birth,
              member_2_age = panel_year - member_2_birth,
              member_3_age = panel_year - member_3_birth,
              member_4_age = panel_year - member_4_birth,
              member_5_age = panel_year - member_5_birth,
              member_6_age = panel_year - member_6_birth,
              member_7_age = panel_year - member_7_birth)]
panel[, c("member_1_birth", "member_2_birth", "member_3_birth", "member_4_birth",
          "member_5_birth", "member_6_birth", "member_7_birth") := NULL]

# Identifying newborns
panel[member_1_age <= 1, "birth" := 1]
panel[member_2_age <= 1, "birth" := 1]
panel[member_3_age <= 1, "birth" := 1]
panel[member_4_age <= 1, "birth" := 1]
panel[member_5_age <= 1, "birth" := 1]
panel[member_6_age <= 1, "birth" := 1]
panel[member_7_age <= 1, "birth" := 1]

# Adding college indicator if at least 1 HoH has graduated college
panel[, "college" := 0]
panel[female_head_education >= 5 | male_head_education >= 5, "college" := 1]

# Making race binary
panel[, "white" := ifelse(race == 1, 1L, 0L)]

# Collapsing type of residence to single-family home, mobile home, and other (likely apt)
panel[, "type_of_residence" := cut(type_of_residence, c(0, 2, 6, 10),
                                   labels = c("Single-Family", "Multi-Family", "Mobile"))]

# Adding child indicator
panel[, "child" := ifelse(age_and_presence_of_children == 9, 0L, 1L)]

# Adding marriage indicator
panel[, "married" := ifelse(marital_status == 1, 1L, 0L)]

# Adding in lat and lon
# scp Desktop/Research/OnlineShopping/WarehouseClubs/code/0_data/zipLatLon.csv
# hossaine@wrds-cloud.wharton.upenn.edu:/home/upenn/hossaine/Nielsen/Data
zipLatLon <- fread("/home/upenn/hossaine/Nielsen/Data/zipLatLon.csv")
panel <- merge(panel, zipLatLon, by = "zip_code")
panel[, "zip_code" := str_pad(zip_code, 5, "left", "0")]
panel[, "state" := substr(fips, 1, 2)]
row4 <- c("Drop ZIPs Not Geocoded:", uniqueN(panel$household_code))

# Adding car ownership and median home size
# scp Desktop/Research/OnlineShopping/WarehouseClubs/code/0_data/carOwnership.csv
# hossaine@wrds-cloud.wharton.upenn.edu:/home/upenn/hossaine/Nielsen/Data
own <- fread("/home/upenn/hossaine/Nielsen/Data/carAndHome.csv")
own[, "zip_code" := str_pad(zip_code, 5, "left", "0")]
panel <- merge(panel, own, by = "zip_code")
row5 <- c("Cannot Be Matched to Car Access:", uniqueN(panel$household_code))

# Add Unit pricing indicators
# scp Desktop/Research/OnlineShopping/WarehouseClubs/code/0_data/nist130.csv
# hossaine@wrds-cloud.wharton.upenn.edu:/home/upenn/hossaine/Nielsen/Data
nist <- fread("/home/upenn/hossaine/Nielsen/Data/nist130.csv")
fips <- unique(as.data.table(maps::state.fips)[, .(fips, abb)])
setnames(fips, c("fips", "state"))
nist <- merge(nist, fips, by = "state")
nist[, "fips" := str_pad(fips, 2, "left", "0")]
nist[, "state" := NULL]
setnames(nist, "fips", "state")
nistLong <- melt(nist, measure.vars = patterns("^law"), value.name = "law",
                 variable.name = "panel_year")
nistLong[, "panel_year" := as.integer(gsub("law", "", panel_year))]
panel <- merge(panel, nistLong, by = c("state", "panel_year"))

# Housekeeping
panel[, c("state", "female_head_education", "male_head_education",
          "male_head_occupation", "female_head_occupation",
          "male_head_birth", "female_head_birth", "race", "marital_status") := NULL]
intCols <- c("zip_code", "fips", "household_income", "college", "married")
panel[, (intCols) := lapply(.SD, as.integer), .SDcols = intCols]
fwrite(panel, "/home/upenn/hossaine/Nielsen/Data/fullPanel.csv", nThread = threads)

cleanTable <- as.data.table(rbind(row1, row2, row3, row4, row5))
setnames(cleanTable, c("Step", "HH"))
cleanTable[, "HH" := as.integer(HH)]
stargazer(cleanTable, type = "text", summary = FALSE,
          title = "Homescan Sample",
          label = "tab:homeScanClean", digits = 2, rownames = FALSE,
          out = "/home/upenn/hossaine/tables/homeScanClean.tex")

######################## STEP 4: CLEAN PRODUCTS FILE ###########################
# Final sample removes any deferred modules, magnet categories, alcohol, and
# any other modules that have fewer than 3 unique sizes ore fewer than 100
# recorded purchases in a given year.
prod <- na.omit(fread(paste0(path, "Master_Files/Latest/products.tsv"), quote = "",
                      nThread = threads, key = c("upc", "upc_ver_uc")))
storable <- fread(paste0("/scratch/upenn/hossaine/storableClassification.csv"))
prod <- merge(prod, storable, by = c("product_group_code", "product_group_descr"))
row0 <- c("Starting Categories", uniqueN(prod$product_module_code))

# Excluding "deferred" modules per Nielsen's suggestion (while there are 164 of
# these, only 60 are actually in the products file)
deferred <- setDT(read_xlsx("/home/upenn/hossaine/Nielsen/Data/Product_Hierarchy.xlsx"))
deferred <- unique(deferred[, .(product_module_code, `Deferred (Please see documentation for explanation and notes)`)])
setnames(deferred, c("product_module_code", "deferred"))
prod <- merge(prod, deferred, by = "product_module_code")
prod <- prod[is.na(deferred)]
prod[, "deferred" := NULL]
row1 <- c("Excluding Deferred Categories", uniqueN(prod$product_module_code))

# Excluding all alcohol, tobacco, pet food and pet care, health and beauty
# except for female hygiene products general merchandise, and magnet purchases
prod <- prod[!department_code %in% 8:9] #alcohol and gen merch
prod <- prod[!product_group_code %in% c(4510, 6001:6014, 6016:6018, 508, 4509)] #tobacco, health, pet food and care
prod <- prod[!product_module_code %in% c(445:468, 750)]
row2 <- c("Excluding Alcohol, Tobacco, Cosmetics, Pets, Gen. Merch. and Magnet Purchases",
          uniqueN(prod$product_module_code))

# Fixing toilet paper products
prod[upc == 1115032045 & upc_ver_uc %in% 1:2, "multi" := 6]
prod[upc == 2898513456, "upc_descr" := gsub("350", "350S", upc_descr)]
prod[upc == 3003402434 & upc_ver_uc == 2, "multi" := 6]
prod[upc == 3040000070 & upc_ver_uc == 1, "size1_amount" := 4]
prod[upc == 3040076541 & upc_ver_uc == 2, "size1_amount" := 1]
prod[upc == 3600010669 & upc_ver_uc == 1, "multi" := 3]
prod[upc == 3600011640 & upc_ver_uc == 2, "multi" := 5]
prod[upc == 3600011643 & upc_ver_uc == 3, "multi" := 6]
prod[upc == 3600041605 & upc_ver_uc == 1, "multi" := 5]
prod[upc == 3600064119 & upc_ver_uc %in% 1:2, "multi" := 6]
prod[upc == 3600067652 & upc_ver_uc == 1, "multi" := 6]
prod[upc == 3600067667 & upc_ver_uc == 1, "multi" := 5]
prod[upc == 3600067794 & upc_ver_uc == 1, "multi" := 2]
prod[upc == 3700002045 & upc_ver_uc == 1, "multi" := 2]
prod[upc == 3700002072 & upc_ver_uc == 2, "multi" := 6]
prod[upc == 3700006470 & upc_ver_uc %in% 1:2, "multi" := 6]
prod[upc == 3700006474 & upc_ver_uc %in% 1:2, "multi" := 9]
prod[upc == 3700012387 & upc_ver_uc %in% 1:3, c("multi", "size1_amount") := .(6, 4)]
prod[upc == 3700024064 & upc_ver_uc == 1, "multi" := 5]
prod[upc == 3700029315 & upc_ver_uc == 2, "size1_amount" := 24]
prod[upc == 3700029319 & upc_ver_uc == 3, "multi" := 4]
prod[upc == 3700032666 & upc_ver_uc == 2, "multi" := 4]
prod[upc == 3700034021 & upc_ver_uc == 2, "size1_amount" := 6]
prod[upc == 3700034030 & upc_ver_uc == 2, c("multi", "size1_amount") := .(6, 6)]
prod[upc == 3700044975 & upc_ver_uc == 1, "multi" := 2]
prod[upc == 3700046162 & upc_ver_uc == 1, "multi" := 4]
prod[upc == 3700046821 & upc_ver_uc == 1, "multi" := 6]
prod[upc == 3700047936 & upc_ver_uc == 2, "multi" := 9]
prod[upc == 3700083747 & upc_ver_uc == 1, "multi" := 4]
prod[upc == 3700083752 & upc_ver_uc == 2, "size1_amount" := 4]
prod[upc == 4116344619 & upc_ver_uc %in% 1:2, "multi" := 6]
prod[upc == 4116344624 & upc_ver_uc %in% 1:2, "multi" := 6]
prod[upc == 4116344628 & upc_ver_uc == 1, "multi" := 1]
prod[upc == 4127079116 & upc_ver_uc == 2, "upc_descr" := "CTL BR DR W 2P 264S TT"]
prod[upc == 4127079116 & upc_ver_uc == 1, "size1_amount" := 12]
prod[upc == 4200070066 & upc_ver_uc == 1, "multi" := 6]
prod[upc == 4200086237 & upc_ver_uc == 1, "multi" := 5]
prod[upc == 4200086510 & upc_ver_uc %in% 1:2, "multi" := 6]
prod[upc == 4200087146 & upc_ver_uc == 2, "size1_amount" := 9]
prod[upc == 4200096516 & upc_ver_uc == 1, "multi" := 4]
prod[upc == 4200096517 & upc_ver_uc == 1, "multi" := 4]
prod[upc == 4218739989 & upc_ver_uc == 1, "size1_amount" := 6]
prod[upc == 4218739989 & upc_ver_uc == 2, c("multi", "size1_amount") := .(6, 4)]
prod[upc == 4303202208 & upc_ver_uc == 2, "multi" := 1]
prod[upc == 4303202208 & upc_ver_uc == 3, "size1_amount" := 20]
prod[upc == 4303203408 & upc_ver_uc == 1, "size1_amount" := 1]
prod[upc == 5400000005 & upc_ver_uc == 1, "size1_amount" := 2]
prod[upc == 5400000009 & upc_ver_uc == 1, "multi" := 6]
prod[upc == 5400000010 & upc_ver_uc %in% 1:2, c("multi", "size1_amount") := .(1, 16)]
prod[upc == 5400041210 & upc_ver_uc == 1, "multi" := 20]
prod[upc == 5400042320 & upc_ver_uc %in% 1:2, "multi" := 20]
prod[upc == 5400042330 & upc_ver_uc == 2, "multi" := 30]
prod[upc == 6132835112 & upc_ver_uc == 1, "size1_amount" := 12]
prod[upc == 61429940221 & upc_ver_uc == 2, "size1_amount" := 4]
prod[upc == 7200015561 & upc_ver_uc == 1, "multi" := 1]
prod[upc == 7417505815 & upc_ver_uc == 1, "size1_amount" := 6]
prod[upc == 7417505831 & upc_ver_uc == 1, "size1_amount" := 24]
prod[upc == 7545007963 & upc_ver_uc == 2, "multi" := 1]
prod[upc == 7789022903 & upc_ver_uc == 1, "multi" := 1]
prod[upc == 7874201206 & upc_ver_uc == 2, "size1_amount" := 4]
prod[upc == 88867001012 & upc_ver_uc == 2, "multi" := 1]
prod[upc == 9661915988 & upc_ver_uc == 1, "multi" := 6]
prod[upc == 9661915988 & upc_ver_uc == 2, "size1_amount" := 6]
prod[upc == 18368900019 & upc_ver_uc == 2, "size1_amount" := 24]
prod[upc == 68113172120 & upc_ver_uc == 2, "multi" := 4]
prod[upc == 68826712833 & upc_ver_uc == 1, "size1_amount" := 4]
prod[upc == 71754411316 & upc_ver_uc == 2, "multi" := 1]
prod[upc == 72645900024 & upc_ver_uc == 2, "size1_amount" := 24]
prod[upc == 72796901557 & upc_ver_uc == 1, "size1_amount" := 12]

# Keeping most common size categories for each module
# Some products are categorized in 2 different units (typically count and ounces)
# so comparisons are difficult across products. I exclude all of these instances.
# Notable modules: 7226 (air fresheners), 1463 (coffee, pods vs grounds)
# This doesn't completely eliminate categories, but only keeps one subset of the
# products that are in the most common units. Mainly it excludes anything with "count" units
prod[, "count" := .N, by = .(product_module_code, size1_units)]
prod[, "max" := max(count), by = product_module_code]
prod <- prod[count == max]
prod[, c("count", "max") := NULL]
prod[, "totalAmount" := multi * size1_amount]
prod <- prod[, .(upc, upc_ver_uc, upc_descr, product_module_code, brand_code_uc,
                 brand_descr, multi, size1_amount, totalAmount, size1_units, food)]

# Getting unique sizes
# Modules that have 5 or fewer sizes: sandwiches, personal care accessories,
# sour cream mix, corned beef hash, canned ham patties, gift sets,
# frozen OJ, frozen grapefruit juice, thermometers, brushes, and frozen grape juice
prod[, "uniqueSizes" := uniqueN(totalAmount), by = product_module_code]
prod <- prod[uniqueSizes > 5][, "uniqueSizes" := NULL]
row3 <- c("More than 5 Sizes", uniqueN(prod$product_module_code))

# Classifying bulk sizes in product file
quintiles <- prod[, .(cutoff = quantile(totalAmount, c(0.2, 0.4, 0.6, 0.8, 1))),
                  by = .(product_module_code)]
quintiles[, "quintile" := 1:5]
quarWide <- dcast(data = quintiles, product_module_code ~ quintile, value.var = "cutoff")
setnames(quarWide, c("product_module_code", "q1", "q2", "q3", "q4", "q5"))
prod <- merge(prod, quarWide, by = "product_module_code")
rm(quintiles, quarWide)
prod[totalAmount > q4 & totalAmount <= q5, "quintile" := 5L]
prod[totalAmount > q3 & totalAmount <= q4, "quintile" := 4L]
prod[totalAmount > q2 & totalAmount <= q3, "quintile" := 3L]
prod[totalAmount > q1 & totalAmount <= q2, "quintile" := 2L]
prod[totalAmount > 0 & totalAmount <= q1, "quintile" := 1L]
prod[, c("q1", "q2", "q3", "q4", "q5") := NULL]
fwrite(prod, "/scratch/upenn/hossaine/fullProd.csv", nThread = threads)

cleanTable <- as.data.table(rbind(row0, row1, row2, row3))
setnames(cleanTable, c("Step", "Categories"))
cleanTable[, "Categories" := as.integer(Categories)]
stargazer(cleanTable, type = "text", summary = FALSE,
          title = "Nielsen Product Categories",
          label = "tab:prodClean", digits = 2, rownames = FALSE,
          out = "/home/upenn/hossaine/tables/prodClean.tex")

######################## STEP 5: CLEAN PURCHASES FILE ##########################
# Keeping purchases with positive prices and computing the unit cost based on
# the individual package price.
# Note: Excluding the product categories above leaves out about 20-35% of
# all Nielsen spending depending on the year. I'm not too worried because
# These categories make sense to exclude anyway.
for (yr in yrs) {
  print(yr)
  # Getting purchases and combining the same products purchased on the same trip
  purch <- fread(paste0(path, yr, "/Annual_Files/purchases_", yr, ".tsv"),
                 nThread = threads, key = c("upc", "upc_ver_uc"))

  purch <- purch[, .(quantity = sum(quantity),
                     total_price_paid = sum(total_price_paid),
                     coupon_value = sum(coupon_value)),
                 by = .(trip_code_uc, upc, upc_ver_uc)]
  totalPaid <- sum(purch$total_price_paid)

  # Keeping those with positive prices and computing unit costs for a single
  # package (e.g if they purchased 2 10oz packages for $10, then each was $5,
  # so the unit cost is $0.50/oz and the pack size is 10oz)
  # I also distribute the coupon value across all units purchased
  purch[, "packagePrice" := total_price_paid / quantity]
  purch[, "coupon_value" := coupon_value / quantity]
  purch[, c("total_price_paid") := NULL]
  purch <- merge(purch, prod, by = c("upc", "upc_ver_uc"))[packagePrice > 0]
  purch[, "size1_units" := NULL]
  setkey(purch, trip_code_uc)
  totalRemaining <- sum(purch$packagePrice * purch$quantity)
  print(totalRemaining / totalPaid)

  # Saving each year
  fwrite(purch, paste0("/scratch/upenn/hossaine/fullPurch", yr, ".csv"), nThread = threads)
  rm(purch)
}

######################## STEP 6: CLEAN TRIPS FILE ##############################
# Combining trips files and computing monthly cumulative spending and whether
# purchase is at the start of the month.
fiveFri <- fread("/scratch/upenn/hossaine/FiveFri.csv")

fullTrips <- NULL
for (yr in yrs) {
  print(yr)
  trips <- fread(paste0(path, yr, "/Annual_Files/trips_", yr, ".tsv"),
                 nThread = threads, key = "trip_code_uc")
  trips[, c("store_zip3") := NULL]
  trips[, c("year", "month", "day") := tstrsplit(purchase_date, "-")]
  trips[, ':=' (year = as.integer(year),
                month = as.integer(month),
                day = as.integer(day))]
  trips <- merge(trips, fiveFri, by = c("year", "month"))
  setorder(trips, household_code, year, month, day)
  trips[, "monthlyCum" := cumsum(total_spent), by = .(household_code, year, month)]
  trips[, "monthStart" := ifelse(day <= 10, 1L, 0L)]
  trips[, c("year", "month", "day") := NULL]

  # Getting credit access for 2013 onward
  if (yr >= 2013) {
    trips[method_of_payment_cd %in% c(1, 2, 8), "payment" := "Cash"]
    trips[method_of_payment_cd %in% 3:7, "payment" := "Credit"]
    trips[method_of_payment_cd == 9, "payment" := "Other"]
    trips[method_of_payment_cd == 0, "payment" := NA]
    trips[, "hasCredit" := (sum(payment == "Credit", na.rm = TRUE) > 0),
          by = .(household_code, panel_year)]
    trips[, "notMissing" := (sum(!is.na(payment)) > 0), by = household_code]
    trips[notMissing == FALSE, "hasCredit" := NA]
    trips[, "hasCredit" := (hasCredit > 0)]
    trips[, c("payment", "notMissing", "method_of_payment_cd") := NULL]
  } else {
    trips[, "hasCredit" := NA]
  }

  fullTrips <- rbindlist(list(fullTrips, trips), use.names = TRUE)
}

# Imputing store zip
path <- "/scratch/upenn/hossaine/"
panel <- fread(paste0(path, "fullPanel.csv"),
               select = c("household_code", "panel_year", "zip_code"),
               key = c("household_code", "panel_year"))

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

fullTrips <- merge(fullTrips, panel, by = c("household_code", "panel_year"))

zipImpute <- fullTrips[, .(zipImpute = Mode(zip_code)),
                       by = .(retailer_code, store_code_uc)]
zipImpute[store_code_uc == 0, "zipImpute" := NA]

fullTrips <- merge(fullTrips, zipImpute, by = c("retailer_code", "store_code_uc"))

# Getting store distance
zipLatLon <- fread("/scratch/upenn/hossaine/zipLatLon.csv")
fullTrips <- merge(fullTrips, zipLatLon, by.x = "zip_code", by.y = "zip_code")
setnames(fullTrips, c("lat", "lon"), c("hhLat", "hhLon"))
fullTrips <- merge(fullTrips, zipLatLon, by.x = "zipImpute", by.y = "zip_code", all.x = TRUE)
setnames(fullTrips, c("lat", "lon"), c("storeLat", "storeLon"))
hh <- as.matrix(fullTrips[, .(hhLon, hhLat)])
store <- as.matrix(fullTrips[, .(storeLon, storeLat)])
dists <- distGeo(hh, store)
fullTrips[, "dist" := dists]
fullTrips[, "zip_code" := NULL]
fwrite(fullTrips, "/scratch/upenn/hossaine/fullTrips.csv", nThread = threads)
rm(fullTrips, trips)

######################## STEP 7: RETAILERS FILE ################################
path <- "/scratch/upenn/hossaine/nielsen_extracts/HMS/"
retailers <- fread(paste0(path, "Master_Files/Latest/retailers.tsv"))
fwrite(retailers, "/scratch/upenn/hossaine/fullRetailers.csv", nThread = threads)
