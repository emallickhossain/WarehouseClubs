# All transferring, unzipping and cleaning of Homescan data
library(data.table)
library(purrr)
library(stringr)
library(readxl)
library(furrr)
library(stargazer)
plan(multiprocess)
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
                            "Scantrack_Market_Identifier_Desc", "DMA_Cd", "DMA_Name"))
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

# Adding income factors
panel[, "household_income_coarse" := cut(household_income, c(0, 13, 19, 26, 30),
                                         labels = c("<25k", "25-50k", "50-100k", ">100k"),
                                         ordered_result = TRUE)]
panel[, "household_income" := ifelse(household_income >= 27, 27, household_income)]

# Adding age
panel[, "age" := panel_year - (female_head_birth + male_head_birth) / 2]
panel[is.na(age), "age" := as.numeric(panel_year - female_head_birth)]
panel[is.na(age), "age" := as.numeric(panel_year - male_head_birth)]

# Adding college indicator if at least 1 HoH has graduated college
panel[, "college" := 0]
panel[female_head_education >= 5 | male_head_education >= 5, "college" := 1]

# Making race binary
panel[, "white" := ifelse(race == 1, 1L, 0L)]

# Collapsing type of residence to single-family home, mobile home, and other (likely apt)
# Drop any NA's
panel <- panel[!is.na(type_of_residence)]
panel[, "type_of_residence" := cut(type_of_residence, c(0, 2, 6, 10),
                                   labels = c("Single-Family", "Multi-Family", "Mobile"))]
row3 <- c("Drop Missing Housing:", uniqueN(panel$household_code))

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
panel[, c("state", "age_and_presence_of_children", "female_head_education",
          "male_head_education", "male_head_occupation", "female_head_occupation",
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
          out = "tables/homeScanClean.tex")

######################## STEP 4: CLEAN PRODUCTS FILE ###########################
# Final sample removes any deferred modules, magnet categories, alcohol, and
# any other modules that have fewer than 3 unique sizes ore fewer than 100
# recorded purchases in a given year.
prod <- na.omit(fread(paste0(path, "Master_Files/Latest/products.tsv"), quote = "",
                      nThread = threads, key = c("upc", "upc_ver_uc")))
storable <- fread(paste0("/scratch/upenn/hossaine/storableClassification.csv"))
prod <- merge(prod, storable, by = c("product_group_code", "product_group_descr"))

# Excluding "deferred" modules per Nielsen's suggestion (while there are 164 of
# these, only 60 are actually in the products file)
deferred <- setDT(read_xlsx("/home/upenn/hossaine/Nielsen/Data/Product_Hierarchy.xlsx"))
deferred <- unique(deferred[, .(product_module_code, `Deferred (Please see documentation for explanation and notes)`)])
setnames(deferred, c("product_module_code", "deferred"))
prod <- merge(prod, deferred, by = "product_module_code")
prod <- prod[is.na(deferred)]
prod[, "deferred" := NULL]

# Excluding all alcohol purchases and magnet purchases
prod <- prod[department_code != 8]
prod <- prod[!product_module_code %in% c(445:468, 750)]

# Keeping most common size categories for each module
# Some products are categorized in 2 different units (typically count and ounces)
# so comparisons are difficult across products. I exclude all of these instances.
# Notable modules: 7741 (candles), 8397 (nutritional supplements),
# 7226 (air fresheners), 1463 (coffee, pods vs grounds)
# This doesn't completely eliminate categories, but only keeps one subset of the
# products that are in the most common units
prod[, "count" := .N, by = .(product_module_code, size1_units)]
prod[, "max" := max(count), by = product_module_code]
prod <- prod[count == max]
prod[, c("count", "max") := NULL]
prod[, "totalAmount" := multi * size1_amount]
prod <- prod[, .(upc, upc_ver_uc, product_module_code, brand_code_uc, totalAmount,
                 size1_units, storable)]

# Classifying bulk sizes in product file
quartiles <- prod[, .(cutoff = quantile(totalAmount, c(0.25, 0.5, 0.75, 1))),
                  by = .(product_module_code)]
quartiles[, "quartile" := 1:4]
quarWide <- dcast(data = quartiles, product_module_code ~ quartile, value.var = "cutoff")
setnames(quarWide, c("product_module_code", "q1", "q2", "q3", "q4"))
prod <- merge(prod, quarWide, by = "product_module_code")
rm(quartiles, quarWide)
prod[totalAmount > q3 & totalAmount <= q4, "quartile" := 4L]
prod[totalAmount > q2 & totalAmount <= q3, "quartile" := 3L]
prod[totalAmount > q1 & totalAmount <= q2, "quartile" := 2L]
prod[totalAmount > 0 & totalAmount <= q1, "quartile" := 1L]
prod[, c("q1", "q2", "q3", "q4") := NULL]
fwrite(prod, "/scratch/upenn/hossaine/fullProd.csv", nThread = threads)

######################## STEP 5: CLEAN PURCHASES FILE ##########################
# Keeping purchases with positive prices and computing the unit cost based on
# the individual package price
for (yr in yrs) {
  print(yr)
  # Getting purchases and combining the same products purchased on the same trip
  purch <- fread(paste0(path, yr, "/Annual_Files/purchases_", yr, ".tsv"),
                 nThread = threads, key = c("upc", "upc_ver_uc"))
  purch <- purch[, .(quantity = sum(quantity),
                     total_price_paid = sum(total_price_paid),
                     coupon_value = sum(coupon_value),
                     deal_flag_uc = sum(deal_flag_uc)),
                 by = .(trip_code_uc, upc, upc_ver_uc)]

  # Keeping those with positive prices and computing unit costs for a single
  # package (e.g if they purchased 2 10oz packages for $10, then each was $5,
  # so the unit cost is $0.50/oz and the pack size is 10oz)
  # I also distribute the coupon value across all units purchased
  purch[, "packagePrice" := total_price_paid / quantity]
  purch[, "coupon_value" := coupon_value / quantity]
  purch[, c("total_price_paid") := NULL]
  purch <- merge(purch, prod, by = c("upc", "upc_ver_uc"))[packagePrice > 0]
  purch <- purch[, .(trip_code_uc, coupon_value, deal_flag_uc, packagePrice, quartile,
                     product_module_code, brand_code_uc, totalAmount, storable, quantity)]
  setkey(purch, trip_code_uc)

  # Only keeping modules with more than 100 recorded purchases
  purch[, "modCount" := .N, by = product_module_code]
  purch <- purch[modCount > 100][, "modCount" := NULL]

  # Only keeping modules with more than 3 unique sizes purchased
  purch[, "uniqueSizes" := uniqueN(totalAmount), by = product_module_code]
  purch <- purch[uniqueSizes > 3][, "uniqueSizes" := NULL]

  # Saving each year
  fwrite(purch, paste0("/scratch/upenn/hossaine/fullPurch", yr, ".csv"), nThread = threads)
  rm(purch)
}

######################## STEP 6: CLEAN TRIPS FILE ##############################
# Combining trips files and computing monthly cumulative spending and whether
# purchase is at the start of the month.
fullTrips <- NULL
for (yr in yrs) {
  print(yr)
  trips <- fread(paste0(path, yr, "/Annual_Files/trips_", yr, ".tsv"),
                 nThread = threads, key = "trip_code_uc")
  trips[, c("store_zip3", "method_of_payment_cd") := NULL]
  trips[, c("year", "month", "day") := tstrsplit(purchase_date, "-")]
  trips[, ':=' (year = as.integer(year),
                month = as.integer(month),
                day = as.integer(day))]
  setorder(trips, household_code, year, month, day)
  trips[, "monthlyCum" := cumsum(total_spent), by = .(household_code, year, month)]
  trips[, "monthStart" := ifelse(day <= 10, 1L, 0L)]
  trips[, c("year", "month", "day") := NULL]

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
fullTrips[, "zip_code" := NULL]
fwrite(fullTrips, "/scratch/upenn/hossaine/fullTrips.csv", nThread = threads)
rm(fullTrips, trips)

######################## STEP 7: RETAILERS FILE ################################
path <- "/scratch/upenn/hossaine/nielsen_extracts/HMS/"
retailers <- fread(paste0(path, "Master_Files/Latest/retailers.tsv"))
fwrite(retailers, "/scratch/upenn/hossaine/fullRetailers.csv", nThread = threads)
