# Getting the choice data for discrete choice estimation
# Step 1: Get all shopping trips for toilet paper from Homescan
# Step 2: Only keep single-unit purchases of the top 6 brands in the top 6
# most common sizes purchased at grocery or discount stores.
# Step 2: Get all store assortments from Scanner
# Step 3: Combine purchases and assortments to get choice environment
# Step 4: Matching with product characteristics
# Step 5: Identifying choices within choice sets
# Step 6: Restricting sample to choices of the most popular brand-sizes
library(data.table)
library(lubridate)
library(stringr)
library(stargazer)
library(ggplot2)
library(ggthemes)
library(ggridges)
library(lfe)
yrs <- 2016
threads <- 8
moduleCode <- 7260 #tp
path <- "/scratch/upenn/hossaine/nielsen_extracts/RMS/"
topBrands <- c("ANGEL SOFT", "CHARMIN", "CTL BR", "KLEENEX COTTONELLE",
               "QUILTED NORTHERN", "SCOTT 1000")
topSizes <- c(4, 6, 8, 9, 12, 16, 18, 20, 24)

################################################################################
# Step 1: Getting all TP purchases
################################################################################
# Loading full trips data
trips <- fread("/scratch/upenn/hossaine/fullTrips.csv", nThread = threads,
               select = c("store_code_uc", "trip_code_uc", "purchase_date",
                          "household_code", "panel_year", "retailer_code",
                          "FiveFri", "FiveFriLag", "monthStart", "hasCredit"))
retailers <- fread("/scratch/upenn/hossaine/fullRetailers.csv", nThread = threads)
trips <- merge(trips, retailers, by = "retailer_code")

# Getting all purchase data together (~66% have a store_code_uc)
fullPurch <- NULL
for (i in 2004:2017) {
  print(i)
  purch <- fread(paste0("/scratch/upenn/hossaine/fullPurch", i, ".csv"),
                 nThread = threads,
                 select = c("trip_code_uc", "packagePrice", "brand_code_uc",
                            "brand_descr", "product_module_code", "upc", "upc_ver_uc",
                            "quantity", "totalAmount"))[product_module_code == moduleCode]
  purch[, "product_module_code" := NULL]
  fullPurch <- rbindlist(list(fullPurch, purch), use.names = TRUE)
  rm(purch)
}

fullPurch <- merge(fullPurch, trips, by = "trip_code_uc")

# Flagging purchases at non-scanner stores (<25% of purchases and trips)
# Nielsen doesn't have dollar or warehouse club assortments (actually, they only
# have 1 dollar store and 1 warehouse club that reports, but I can't match it
# because it is missing in the Consumer Panel)
# Only looking at grocery and discount stores because they offer wide assortments
# of brands and sizes
stores <- c("Grocery", "Discount Store")
fullPurch[channel_type %in% stores, "uncommonStore" := FALSE]
fullPurch[is.na(uncommonStore), "uncommonStore" := TRUE]
prop.table(table(fullPurch$uncommonStore))

# Flagging any multi-UPC purchases (<2% of shopping trips or <4% of purchases)
singles <- fullPurch[, .N, by = trip_code_uc][N == 1]$trip_code_uc
fullPurch[trip_code_uc %in% singles, "multiUPC" := FALSE]
fullPurch[is.na(multiUPC), "multiUPC" := TRUE]
prop.table(table(fullPurch$multiUPC))

# Flagging non-popular brands (<14% of all purchases)
fullPurch[brand_descr %in% c("KLEENEX", "COTTONELLE"), brand_descr := "KLEENEX COTTONELLE"]
fullPurch[brand_descr %in% topBrands, "uncommonBrand" := FALSE]
fullPurch[is.na(uncommonBrand), "uncommonBrand" := TRUE]
prop.table(table(fullPurch$uncommonBrand))

# Flagging uncommon sizes (<20% of purchases)
fullPurch[totalAmount %in% topSizes, "uncommonSize" := FALSE]
fullPurch[is.na(uncommonSize), "uncommonSize" := TRUE]
prop.table(table(fullPurch$uncommonSize))

# Flagging multi-unit purchases
fullPurch[quantity > 1, "multiUnit" := TRUE]
fullPurch[is.na(multiUnit), "multiUnit" := FALSE]

# Getting week end
fullPurch[, "week_end" := as.Date(purchase_date, format = "%Y-%m-%d")]
fullPurch[, "week_end" := ceiling_date(week_end, "week", week_start = 6,
                                       change_on_boundary = FALSE)]
fullPurch[, "week_end" := as.integer(gsub("-", "", week_end))]

setnames(fullPurch, c("brand_code_uc", "brand_descr", "upc", "upc_ver_uc"),
         c("brand_choice", "brand_descr_choice", "upc_choice", "upc_ver_uc_choice"))
fullPurch[, "drop" := multiUPC + uncommonStore + uncommonBrand + uncommonSize + multiUnit]

# Saving all shopping trips
fwrite(fullPurch, "/scratch/upenn/hossaine/fullTPPurchases.csv", nThread = threads)

################################################################################
########## Step 2: Getting product characteristics
################################################################################
prod <- fread("/scratch/upenn/hossaine/fullProd.csv", nThread = threads,
              select = c("upc", "upc_ver_uc", "upc_descr", "product_module_code",
                         "multi", "size1_amount", "brand_code_uc", "brand_descr"))[product_module_code == moduleCode]
prod[, "product_module_code" := NULL]

# Getting rolls and sheets for each product
prod[, "rolls" := as.integer(multi * size1_amount)]
prod[, "ply" := str_extract_all(upc_descr, "\\s\\dP\\s")]
prod[, "ply" := as.integer(gsub("P", "", ply))]
prod[, "sheet" := str_extract_all(upc_descr, "\\d{2,}S\\s")]
prod[, "sheet" := as.integer(gsub("S", "", sheet)) * ply]
prod[, "totalSheet" := sheet * rolls]
prod[, c("multi", "size1_amount", "ply") := NULL]

# Removing over 100 rolls and packages where sheets cannot be computed
prod <- prod[rolls < 100]
prod <- prod[!is.na(totalSheet)]
fwrite(prod, "/scratch/upenn/hossaine/prodTP.csv")

################################################################################
# Step 3: Getting store assortments for TP
################################################################################
# Transfer data from Globus
# cd /scratch/upenn/hossaine
# tar -xzvf tp.tar.gz

prod <- fread("/scratch/upenn/hossaine/prodTP.csv")
fullTP <- NULL
for (yr in yrs) {
  print(yr)
  tp <- fread(paste0(path, yr, "/Movement_Files/4507_", yr, "/", moduleCode, "_", yr, ".tsv"),
              nThread = threads,
              select = c("store_code_uc", "upc", "week_end", "price", "prmult"))
  upcVer <- fread(paste0(path, yr, "/Annual_Files/rms_versions_", yr, ".tsv"))
  store <- fread(paste0(path, yr, "/Annual_Files/stores_", yr, ".tsv"),
                 select = c("store_code_uc", "retailer_code"))
  tp <- merge(tp, upcVer, by = "upc")[, "panel_year" := NULL]
  tp <- merge(tp, store, by = "store_code_uc")
  fullTP <- rbindlist(list(fullTP, tp))
  rm(tp)
}

# Restricting choice set to top 5 brands + CTL BR and top 5 sizes
choiceUPCs <- prod[brand_descr %in% topBrands & rolls %in% topSizes]
fullTPSave <- merge(fullTP, choiceUPCs[, .(upc, upc_ver_uc)], by = c("upc", "upc_ver_uc"))
fwrite(fullTPSave, "/scratch/upenn/hossaine/fullTPAssortment.csv", nThread = threads)

################################################################################
# Step 4: Combining purchases with store assortments
################################################################################
# ~30% of trips can be matched to the scanner. The match rate peaked in 2011.
# Match rates tend to increase with income, but overall, they're about 25-35%
# depending on the year.
# Restricting purchases to 1 package purchases in the top 6 brands, top 5 sizes
# at grocery or discount stores
fullPurch <- fread("/scratch/upenn/hossaine/fullTPPurchases.csv",
                   nThread = threads)[drop == 0]
fullPurch[, c("channel_type", "uncommonStore", "multiUPC", "uncommonBrand",
              "uncommonSize", "multiUnit", "drop", "quantity",
              "retailer_code") := NULL]
fullTP <- fread("/scratch/upenn/hossaine/fullTPAssortment.csv", nThread = threads)
fullChoice <- merge(fullPurch, fullTP, by = c("store_code_uc", "week_end"))
fullChoice[, "prmult" := NULL]
fwrite(fullChoice, "/scratch/upenn/hossaine/fullTPChoices.csv", nThread = threads)

################################################################################
# Step 5: Matching with product characteristics
################################################################################
# Merging purchase data with available choices and saving
fullChoice <- merge(fullChoice, prod, by = c("upc", "upc_ver_uc"), all.x = TRUE)
fwrite(fullChoice, "/scratch/upenn/hossaine/fullChoice.csv", nThread = threads)

################################################################################
# Step 6: Identifying choices
################################################################################
fullChoice <- unique(fread("/scratch/upenn/hossaine/fullChoice.csv", nThread = threads))
prod <- fread("/scratch/upenn/hossaine/prodTP.csv", nThread = threads,
              select = c("upc", "upc_ver_uc", "rolls", "sheet", "totalSheet"))
setnames(prod, c("rolls", "sheet", "totalSheet"),
         c("rolls_choice", "sheet_choice", "totalSheet_choice"))

# Need to separate into generic and non-generic choices since matching
# strategies are different
generic <- fullChoice[brand_choice == 536746]
brands <- fullChoice[brand_choice != 536746]

# Matching brands (95% of branded trips are matched exactly, I drop the other 5%)
brands[, "choice" := (upc_choice == upc & upc_ver_uc_choice == upc_ver_uc)]
matched <- brands[, sum(choice), by = .(trip_code_uc, panel_year)]
round(prop.table(table(matched$V1)), 2)
brands <- brands[trip_code_uc %in% matched[V1 == 1]$trip_code_uc]
brands[, c("upc_choice", "upc_ver_uc_choice", "packagePrice", "brand_choice",
           "brand_descr_choice") := NULL]

# Matching generics. A match is a generic with the same rolls and sheets
# 84% are matched here
generic <- merge(generic, prod, by.x = c("upc_choice", "upc_ver_uc_choice"),
                 by.y = c("upc", "upc_ver_uc"))
generic[, "choice" := (brand_code_uc == 536746 & rolls_choice == rolls &
                         sheet_choice == sheet & totalSheet_choice == totalSheet)]
matched <- generic[, sum(choice), by = .(trip_code_uc, panel_year)]
round(prop.table(table(matched$V1)), 2)
generic <- generic[trip_code_uc %in% matched[V1 == 1]$trip_code_uc]
generic[, c("upc_choice", "upc_ver_uc_choice", "packagePrice", "brand_choice",
            "brand_descr_choice", "rolls_choice", "sheet_choice",
            "totalSheet_choice") := NULL]

# Combining to get full choice set. About 80% are still single-unit purchases
fullChoiceFinal <- rbindlist(list(brands, generic), use.names = TRUE)

# Housekeeping
# Sometimes there are duplicate UPCs with slightly different prices. To be
# conservative, I take the minimum of the posted prices
fullChoiceFinal <- fullChoiceFinal[, .(price = min(price),
                                       choice = max(choice)),
                                   by = .(upc, upc_ver_uc, store_code_uc,
                                          week_end, trip_code_uc,
                                          household_code, panel_year,
                                          brand_descr, rolls, sheet, totalSheet,
                                          FiveFri, FiveFriLag, monthStart, hasCredit)]

# There are many cases where the underlying sheet count is similar to another
# product (e.g. 396 sheets vs 400 sheets). To cut down on creating artifical
# products when these are basically the same, I collapse these into groups that
# mirror the brand's own delineation into regular, double, big, mega, jumbo, etc.
# size rolls. Scott is the only one that doesn't need this because all their
# rolls are 1000 sheets.

# Scott
fullChoiceFinal[brand_descr == "SCOTT 1000" & sheet == 1000,
                ':=' (stdSheet = 1000L,
                      stdTotalSheet = 1000L * rolls)]
fullChoiceFinal[brand_descr == "SCOTT 1000" & sheet == 1100,
                ':=' (stdSheet = 1100L,
                      stdTotalSheet = 1100L * rolls)]

# Angel Soft
fullChoiceFinal[brand_descr == "ANGEL SOFT" & sheet %in% c(264:300),
                ':=' (stdSheet = 264L,
                      stdTotalSheet = 264L * rolls)]
fullChoiceFinal[brand_descr == "ANGEL SOFT" & sheet %in% c(352:450),
                ':=' (stdSheet = 352L,
                      stdTotalSheet = 352L * rolls)]
fullChoiceFinal[brand_descr == "ANGEL SOFT" & sheet %in% c(484:528),
                ':=' (stdSheet = 528L,
                      stdTotalSheet = 528L * rolls)]
fullChoiceFinal[brand_descr == "ANGEL SOFT" & sheet %in% c(572:638),
                ':=' (stdSheet = 600L,
                      stdTotalSheet = 600L * rolls)]
fullChoiceFinal[brand_descr == "ANGEL SOFT" & sheet %in% c(704:720),
                ':=' (stdSheet = 704L,
                      stdTotalSheet = 704L * rolls)]
fullChoiceFinal[brand_descr == "ANGEL SOFT" & sheet %in% c(792:800),
                ':=' (stdSheet = 800L,
                      stdTotalSheet = 800L * rolls)]
fullChoiceFinal[brand_descr == "ANGEL SOFT" & sheet %in% c(938:1056),
                ':=' (stdSheet = 938L,
                      stdTotalSheet = 938L * rolls)]

# Charmin
fullChoiceFinal[brand_descr == "CHARMIN" & sheet %in% c(154:264),
                ':=' (stdSheet = 200L,
                      stdTotalSheet = 200L * rolls)]
fullChoiceFinal[brand_descr == "CHARMIN" & sheet %in% c(275:370),
                ':=' (stdSheet = 352L,
                      stdTotalSheet = 352L * rolls)]
fullChoiceFinal[brand_descr == "CHARMIN" & sheet %in% c(374:528),
                ':=' (stdSheet = 400L,
                      stdTotalSheet = 400L * rolls)]
fullChoiceFinal[brand_descr == "CHARMIN" & sheet %in% c(560:660),
                ':=' (stdSheet = 660L,
                      stdTotalSheet = 660L * rolls)]
fullChoiceFinal[brand_descr == "CHARMIN" & sheet %in% c(680:800),
                ':=' (stdSheet = 704L,
                      stdTotalSheet = 704L * rolls)]

# Ctl Br
fullChoiceFinal[brand_descr == "CTL BR" & sheet %in% 198:280,
                ':=' (stdSheet = 198L,
                      stdTotalSheet = 198L * rolls)]
fullChoiceFinal[brand_descr == "CTL BR" & sheet %in% 286:369,
                ':=' (stdSheet = 330L,
                      stdTotalSheet = 330L * rolls)]
fullChoiceFinal[brand_descr == "CTL BR" & sheet %in% 396:440,
                ':=' (stdSheet = 400L,
                      stdTotalSheet = 400L * rolls)]
fullChoiceFinal[brand_descr == "CTL BR" & sheet %in% 484:550,
                ':=' (stdSheet = 484L,
                      stdTotalSheet = 484L * rolls)]
fullChoiceFinal[brand_descr == "CTL BR" & sheet %in% 560:660,
                ':=' (stdSheet = 600L,
                      stdTotalSheet = 600L * rolls)]
fullChoiceFinal[brand_descr == "CTL BR" & sheet %in% 680:792,
                ':=' (stdSheet = 704L,
                      stdTotalSheet = 704L * rolls)]
fullChoiceFinal[brand_descr == "CTL BR" & sheet %in% 744:1408,
                ':=' (stdSheet = 1000L,
                      stdTotalSheet = 1000L * rolls)]

# Cottonelle
fullChoiceFinal[brand_descr == "KLEENEX COTTONELLE" & sheet %in% 150:222,
                ':=' (stdSheet = 208L,
                      stdTotalSheet = 208L * rolls)]
fullChoiceFinal[brand_descr == "KLEENEX COTTONELLE" & sheet %in% 230:277,
                ':=' (stdSheet = 260L,
                      stdTotalSheet = 260L * rolls)]
fullChoiceFinal[brand_descr == "KLEENEX COTTONELLE" & sheet %in% 300:352,
                ':=' (stdSheet = 352L,
                      stdTotalSheet = 352L * rolls)]
fullChoiceFinal[brand_descr == "KLEENEX COTTONELLE" & sheet %in% 380:470,
                ':=' (stdSheet = 400L,
                      stdTotalSheet = 400L * rolls)]
fullChoiceFinal[brand_descr == "KLEENEX COTTONELLE" & sheet %in% 498:664,
                ':=' (stdSheet = 600L,
                      stdTotalSheet = 600L * rolls)]

# Quilted Nortern
fullChoiceFinal[brand_descr == "QUILTED NORTHERN" & sheet %in% 264:400,
                ':=' (stdSheet = 380L,
                      stdTotalSheet = 380L * rolls)]
fullChoiceFinal[brand_descr == "QUILTED NORTHERN" & sheet %in% 412:572,
                ':=' (stdSheet = 528L,
                      stdTotalSheet = 528L * rolls)]
fullChoiceFinal[brand_descr == "QUILTED NORTHERN" & sheet %in% 594:660,
                ':=' (stdSheet = 600L,
                      stdTotalSheet = 600L * rolls)]
fullChoiceFinal[brand_descr == "QUILTED NORTHERN" & sheet %in% 704:770,
                ':=' (stdSheet = 704L,
                      stdTotalSheet = 704L * rolls)]
fullChoiceFinal[brand_descr == "QUILTED NORTHERN" & sheet %in% 792:990,
                ':=' (stdSheet = 990L,
                      stdTotalSheet = 990L * rolls)]

# Computing new price for the given number of sheets
fullChoiceFinal[, "stdPrice" := stdSheet / sheet * price]

# There are many UPCs that appear to code for the same product (at least based
# upon the package size, quantity, and brand), so this has to either be variation
# beyond a double, mega, ultra roll or something silly like any packaging changes
# would require a new UPC. I generate a new id which is just brandRollSheet
fullChoiceFinal[, "brandRollSheet" := paste(brand_descr, rolls, stdTotalSheet, sep = "_")]

fwrite(fullChoiceFinal, "/scratch/upenn/hossaine/fullChoiceFinal.csv", nThread = threads)
