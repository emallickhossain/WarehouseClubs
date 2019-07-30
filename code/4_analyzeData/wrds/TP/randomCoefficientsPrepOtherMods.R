# Getting the choice data for discrete choice estimation
# Step 1: Get all shopping trips for diapers from Homescan (removing any
# purchases where multiple UPCs were purchased
# Step 2: Get all store assortments from Scanner
# Step 3: Combine purchases and assortments to get choice environment
# Step 4: Matching with product characteristics
# Step 5: Identifying choices within choice sets
# Step 6: Restricting sample to choices of the most popular brand-sizes
library(data.table)
library(lubridate)
library(stringr)
library(stargazer)
yrs <- 2006:2016
threads <- 8
moduleCode <- 8444 #tp
path <- "/scratch/upenn/hossaine/nielsen_extracts/HMS/"

################################################################################
# Step 1: Getting all TP purchases
################################################################################
# Loading full trips data
trips <- fread("/scratch/upenn/hossaine/fullTrips.csv", nThread = threads,
               select = c("store_code_uc", "trip_code_uc", "purchase_date",
                          "household_code", "panel_year", "retailer_code"))
retailers <- fread("/scratch/upenn/hossaine/fullRetailers.csv", nThread = threads)
trips <- merge(trips, retailers, by = "retailer_code")

# Getting all purchase data together (~66% have a store_code_uc)
fullPurch <- NULL
for (i in yrs) {
  print(i)
  purch <- fread(paste0("/scratch/upenn/hossaine/fullPurch", i, ".csv"),
                 nThread = threads,
                 select = c("trip_code_uc", "packagePrice", "brand_code_uc",
                            "product_module_code", "upc", "upc_ver_uc",
                            "quantity"))[product_module_code == moduleCode]
  purch[, "product_module_code" := NULL]
  fullPurch <- rbindlist(list(fullPurch, purch), use.names = TRUE)
  rm(purch)
}

fullPurch <- merge(fullPurch, trips, by = "trip_code_uc")

# Getting week end
fullPurch[, "week_end" := as.Date(purchase_date, format = "%Y-%m-%d")]
fullPurch[, "week_end" := ceiling_date(week_end, "week", week_start = 6,
                                       change_on_boundary = FALSE)]
fullPurch[, "week_end" := as.integer(gsub("-", "", week_end))]
fullPurch[, "purchase_date" := NULL]

setnames(fullPurch, c("brand_code_uc", "upc", "upc_ver_uc"),
         c("brand_choice", "upc_choice", "upc_ver_uc_choice"))

# Removing any multi-UPC purchases (<2% of purchases)
singles <- fullPurch[, .N, by = trip_code_uc][N == 1]$trip_code_uc
fullPurch <- fullPurch[trip_code_uc %in% singles]

# Saving all shopping trips
fwrite(fullPurch, "/scratch/upenn/hossaine/fullTPPurchases.csv", nThread = threads)

################################################################################
# Step 2: Getting store assortments for TP
################################################################################
# Transfer data from Globus
# cd /scratch/upenn/hossaine
# tar -xzvf tp.tar.gz

fullTP <- NULL
for (yr in yrs) {
  print(yr)
  tp <- fread(paste0(path, yr, "/Movement_Files/4507_", yr, "/", moduleCode, "_", yr, ".tsv"),
              nThread = threads,
              select = c("store_code_uc", "upc", "week_end", "price", "prmult"))
  upcVer <- fread(paste0(path, yr, "/Annual_Files/rms_versions_", yr, ".tsv"))
  tp <- merge(tp, upcVer, by = "upc")[, "panel_year" := NULL]
  fullTP <- rbindlist(list(fullTP, tp))
  rm(tp)
}

fwrite(fullTP, "/scratch/upenn/hossaine/fullTPAssortment.csv", nThread = threads)

################################################################################
# Step 3: Combining purchases with store assortments
################################################################################
# ~30% of trips can be matched to the scanner. The match rate peaked in 2011.
# Match rates tend to increase with income, but overall, they're about 25-35%
# depending on the year.
fullPurch <- fread("/scratch/upenn/hossaine/fullTPPurchases.csv", nThread = threads)
fullTP <- fread("/scratch/upenn/hossaine/fullTPAssortment.csv", nThread = threads)
fullChoice <- merge(fullPurch, fullTP, by = c("store_code_uc", "week_end"))
fwrite(fullChoice, "/scratch/upenn/hossaine/fullTPChoices.csv", nThread = threads)

################################################################################
# Step 4: Matching with product characteristics
################################################################################
prod <- fread(paste0(path, "Master_Files/Latest/products.tsv"), nThread = threads,
              select = c("upc", "upc_ver_uc", "upc_descr", "product_module_code",
                         "brand_code_uc", "brand_descr", "multi", "size1_amount"),
              quote = "")[product_module_code == moduleCode]
prod[, "product_module_code" := NULL]

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

# Getting rolls and sheets for each product
prod[, "rolls" := as.integer(multi * size1_amount)]
prod[, "ply" := str_extract_all(upc_descr, "\\s\\dP\\s")]
prod[, "ply" := as.integer(gsub("P", "", ply))]
prod[, "sheet" := str_extract_all(upc_descr, "\\d{2,}S\\s")]
prod[, "sheet" := as.integer(gsub("S", "", sheet))]
prod[, "sheets" := ply * sheet * rolls]
prod[, c("upc_descr", "multi", "size1_amount", "ply", "sheet") := NULL]

# Removing products with more than 100 rolls because these are mistakes, but I
# cannot correct them
prod <- prod[rolls < 100]
prod <- na.omit(prod)

# Merging purchase data with available choices
fullChoice <- merge(fullChoice, prod, by = c("upc", "upc_ver_uc"), all.x = TRUE)
fwrite(fullChoice, "/scratch/upenn/hossaine/fullChoice.csv", nThread = threads)

################################################################################
#   QUALITY CONTROL
################################################################################
# How many multiple unit purchases are there?
# 80% of purchases are for single units. 15% are for 2 units. The remainder are
# for 3 or more units
matchedPurch <- unique(fullChoice[, .(trip_code_uc, upc_choice, upc_ver_uc_choice,
                                      quantity, panel_year)])
multi <- matchedPurch[, sum(quantity), by = .(trip_code_uc, panel_year)]
round(prop.table(table(multi$V1)), 2)

# Summary stats of purchasing
fullPurch <- fread("/scratch/upenn/hossaine/fullTPPurchases.csv", nThread = threads)
tableData <- merge(fullPurch, prod, by.x = c("upc_choice", "upc_ver_uc_choice"),
                   by.y = c("upc", "upc_ver_uc"))
tableData[brand_descr %in% c("KLEENEX", "COTTONELLE"), brand_descr := "KLEENEX COTTONELLE"]
tableData <- tableData[, .(sheets = sum(sheets * quantity)), by = .(brand_descr, rolls)]
tableData[, "sheetShare" := sheets / sum(sheets)]
tableData[, "choice" := paste(brand_descr, rolls, sep = "_")]
setorder(tableData, -sheetShare)
tableData[, "cumsum" := cumsum(sheetShare)]
plot(cumsum(sort(tableData$sheetShare, decreasing = TRUE)))
top5 <- tableData[, .(sum(sheetShare)), by = brand_descr][, "cumsum" := cumsum(V1)]
# Top 5 brands + CTL BR are about 89% of purchases

################################################################################
# Step 5: Identifying choices
################################################################################
fullChoice <- fread("/scratch/upenn/hossaine/fullChoice.csv", nThread = threads)

# Need to separate into generic and non-generic choices since matching
# strategies are different
generic <- fullChoice[brand_choice == 536746]
brands <- fullChoice[brand_choice != 536746]

# Matching brands (92% of branded trips are matched exactly, I drop the other 8%)
brands[, "choice" := (upc_choice == upc & brand_choice == brand_code_uc &
                        upc_ver_uc_choice == upc_ver_uc)]
matched <- brands[, sum(choice), by = .(trip_code_uc, panel_year)]
round(prop.table(table(matched$V1)), 2)
brands <- brands[trip_code_uc %in% matched[V1 == 1]$trip_code_uc]

# Matching generics. A match is a generic with the same rolls and sheets
# 85% are matched here
generic <- merge(generic, prod, by.x = c("upc_choice", "upc_ver_uc_choice"),
                 by.y = c("upc", "upc_ver_uc"))
generic[, "choice" := (brand_code_uc.x == 536746 & rolls.x == rolls.y &
                         sheets.x == sheets.y)]
matched <- generic[, sum(choice), by = .(trip_code_uc, panel_year)]
round(prop.table(table(matched$V1)), 2)
generic <- generic[trip_code_uc %in% matched[V1 == 1]$trip_code_uc]
generic[, c("brand_code_uc.y", "brand_descr.y", "rolls.y", "sheets.y") := NULL]
setnames(generic, c("brand_code_uc.x", "brand_descr.x", "rolls.x", "sheets.x"),
         c("brand_code_uc", "brand_descr", "rolls", "sheets"))

# Combining to get full choice set. About 80% are still multi-unit purchases
fullChoiceFinal <- rbindlist(list(brands, generic), use.names = TRUE)

# Combining Kleenex, Cottonelle, and Kleenex Cottonelle together
fullChoiceFinal[brand_descr %in% c("KLEENEX", "COTTONELLE"),
                brand_descr := "KLEENEX COTTONELLE"]

################################################################################
# Step 6: Restricting sample to choices of the most popular brand-sizes
# (top 5 brands and generics at popular store types).
################################################################################
# Only getting trips where one of those items was chosen
topBrands <- c("ANGEL SOFT", "CHARMIN", "KLEENEX COTTONELLE", "QUILTED NORTHERN",
               "SCOTT 1000", "CTL BR")
fullChoiceFinal[, "brandSize" := paste(brand_descr, rolls, sep = "_")]
fullChoiceFinal <- fullChoiceFinal[brand_descr %in% topBrands]
fullChoiceFinal[, "chosen" := sum(choice), by = trip_code_uc]
fullChoiceFinal <- fullChoiceFinal[chosen == 1][, "chosen" := NULL]

# Housekeeping
# Sometimes there are duplicate UPCs with slightly different prices. To be
# conservative, I take the minimum of the posted prices
fullChoiceFinal[, c("brand_choice", "upc_choice", "upc_ver_uc_choice", "quantity",
                    "prmult", "brand_code_uc", "packagePrice") := NULL]
fullChoiceFinal <- unique(fullChoiceFinal)
fullChoiceFinal <- fullChoiceFinal[, .(price = min(price),
                                       choice = max(choice)),
                                   by = .(upc, upc_ver_uc, store_code_uc,
                                          week_end, trip_code_uc, retailer_code,
                                          household_code, panel_year, channel_type,
                                          brand_descr, rolls, sheets, brandSize)]

fwrite(fullChoiceFinal, "/scratch/upenn/hossaine/fullChoiceFinal.csv", nThread = threads)
