# All transferring, unzipping and cleaning of Homescan data
library(data.table)
library(purrr)
library(stringr)
library(readxl)
library(furrr)
library(stargazer)
plan(multiprocess)
yr <- 2004:2017
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
# Drops all students and veterans
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
panel <- rbindlist(map(yr, getPanel))
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
# scp Desktop/Research/OnlineShopping/WarehouseClubs/code/0_data/zipLatLon.csv hossaine@wrds-cloud.wharton.upenn.edu:/home/upenn/hossaine/Nielsen/Data
zipLatLon <- fread("/home/upenn/hossaine/Nielsen/Data/zipLatLon.csv")
panel <- merge(panel, zipLatLon, by = "zip_code")
panel[, "zip_code" := str_pad(zip_code, 5, "left", "0")]
panel[, "state" := substr(fips, 1, 2)]
row4 <- c("Drop ZIPs Not Geocoded:", uniqueN(panel$household_code))

# Adding car ownership and median home size
# scp Desktop/Research/OnlineShopping/WarehouseClubs/code/0_data/carOwnership.csv hossaine@wrds-cloud.wharton.upenn.edu:/home/upenn/hossaine/Nielsen/Data
own <- fread("/home/upenn/hossaine/Nielsen/Data/carAndHome.csv")
own[, "zip_code" := str_pad(zip_code, 5, "left", "0")]
panel <- merge(panel, own, by = "zip_code")
row5 <- c("Cannot Be Matched to Car Access:", uniqueN(panel$household_code))

# Add Unit pricing indicators
# scp Desktop/Research/OnlineShopping/WarehouseClubs/code/0_data/nist130.csv hossaine@wrds-cloud.wharton.upenn.edu:/home/upenn/hossaine/Nielsen/Data
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
fwrite(panel, "/home/upenn/hossaine/Nielsen/Data/fullPanel.csv")

cleanTable <- as.data.table(rbind(row1, row2, row3, row4, row5))
setnames(cleanTable, c("Step", "HH"))
cleanTable[, "HH" := as.integer(HH)]
stargazer(cleanTable, type = "text", summary = FALSE,
          title = "Homescan Sample",
          label = "tab:homeScanClean", digits = 2, rownames = FALSE,
          out = "tables/homeScanClean.tex")

######################## STEP 4: CLEAN PRODUCTS FILE ###########################
# Generating master purchase file for products that will be specifically analyzed
# 3625: Milk
# 7260: Toilet paper
# 7270: Tampons
# 7734: Paper Towels
# 8444: Diapers
keyProds <- c(3625, 7260, 7270, 7734, 8444)

# Remove NAs and keep key products
prod <- fread(paste0(path, "Master_Files/Latest/products.tsv"), quote = "", nThread = threads)
prod <- na.omit(prod[product_module_code %in% keyProds])

# Cleaning toilet paper observations ###########################################
# Recoding based on manual inspection and purchases
# Generally, if the product is in the ALL or RMS data, that seems to be more
# correct than if it is in the HMS dataset. Some are not in RMS, so I use my
# best judgement
# 63 products are recoded
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

### Only keeping products with the most prevalent units in that module.
# There are some products which have different units that the standard
# ones in that category, so this addresses that.
prod[, "count" := .N, by = .(product_module_code, size1_units)]
prod[, "max" := max(count), by = product_module_code]
prod <- prod[count == max]
prod[, c("count", "max") := NULL]

fwrite(prod, "/home/upenn/hossaine/Nielsen/Data/prod.csv")
################################################################################

######################## STEP 5: CLEAN PURCHASE FILE ###########################
# Only purchases of key products
prod <- fread("/home/upenn/hossaine/Nielsen/Data/prod.csv",
              select = c("upc", "upc_ver_uc"), key = c("upc", "upc_ver_uc"))

getPurch <- function(yr) {
  print(yr)
  trips <- fread(paste0(path,yr, "/Annual_Files/trips_", yr, ".tsv"), nThread = threads,
                 select = c("household_code", "panel_year", "trip_code_uc"))
  purch <- fread(paste0(path, yr, "/Annual_Files/purchases_", yr, ".tsv"),
                 key = "trip_code_uc", nThread = threads)

  # Combining the same products purchased on the same trip
  purch <- purch[, .(quantity = sum(quantity),
                     total_price_paid = sum(total_price_paid),
                     coupon_value = sum(coupon_value),
                     deal_flag_uc = sum(deal_flag_uc)),
                 by = .(trip_code_uc, upc, upc_ver_uc)]
  purch <- merge(purch, trips, by = "trip_code_uc")
  purch[, "tripTotal" := sum(total_price_paid), by = trip_code_uc]
  purch[, "hhTotal" := sum(total_price_paid), by = household_code]
  purch[, c("household_code", "panel_year") := NULL]
  purch <- merge(purch, prod, by = c("upc", "upc_ver_uc"))
  return(purch)
}
fullPurch <- rbindlist(map(yr, getPurch), use.names = TRUE, fill = TRUE)
tripCodes <- unique(fullPurch$trip_code_uc)
fwrite(fullPurch, file = "/home/upenn/hossaine/Nielsen/Data/purchase.csv", nThread = threads)
################################################################################

######################## STEP 6: CLEAN TRIPS FILE ##############################
# Only keeping trips for households in cleaned panel data and trips
panel <- fread("/home/upenn/hossaine/Nielsen/Data/fullPanel.csv",
               select = c("household_code", "panel_year"),
               key = c("household_code", "panel_year"))

getTrips <- function(yr) {
  print(yr)
  trips <- fread(paste0(path, yr, "/Annual_Files/trips_", yr, ".tsv"), nThread = threads)
  return(trips)
}
fullTrips <- rbindlist(map(yr, getTrips), use.names = TRUE, fill = TRUE)
setkey(fullTrips, household_code, panel_year)
fullTrips <- merge(fullTrips, panel, by = c("household_code", "panel_year"))
fullTripsSave <- fullTrips[trip_code_uc %in% tripCodes]
fwrite(fullTripsSave, "/scratch/upenn/hossaine/trips.csv", nThread = threads)
# cd /scratch/upenn/hossaine
# Compress: tar -czvf trips.tgz trips.csv
# Move to home: cp trips.tgz /home/upenn/hossaine/Nielsen/Data/
# Extract: tar -xzvf trips.tgz

######################## STEP 7: RETAILERS FILE ################################
retailers <- fread(paste0(path, "Master_Files/Latest/retailers.tsv"))
fwrite(retailers, "/home/upenn/hossaine/Nielsen/Data/retailers.csv")

######################## STEP 8: IMPUTE STORE ZIP ##############################
# Imputes the ZIP of stores based on the modal ZIP of shoppers
path <- "/scratch/upenn/hossaine/"
panel <- fread(paste0(path, "fullPanel.csv"),
               select = c("household_code", "panel_year", "zip_code"))
zipLatLon <- fread(paste0(path, "zipLatLon.csv"))

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

fullTrips <- fullTrips[store_code_uc != 0, .(household_code, panel_year, retailer_code,
                                             store_code_uc, store_zip3)]
fullTrips <- merge(fullTrips, panel, by = c("household_code", "panel_year"))

zipImpute <- fullTrips[, .(zipImpute = Mode(zip_code)),
                       by = .(retailer_code, store_code_uc)]
zipImpute <- merge(zipImpute, zipLatLon, by.x = "zipImpute", by.y = "zip_code")
setnames(zipImpute, c("lat", "lon"), c("store_lat", "store_lon"))
fwrite(zipImpute, file = "/home/upenn/hossaine/Nielsen/Data/zipImpute.csv")
