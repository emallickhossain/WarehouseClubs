# Get product data
# Drops all magnet data
# Drops NA's
# Drops wood chips (only product measured in CF)
# Drops film (only product measured in EXP)
# Drops floss (only product measured in YD)

# For storage purposes, only keep the following products:
# 4000: Bread
# 3625: Milk
# 7260: Toilet paper
# 7265: Sanitary Napkins
# 7270: Tampons
# 4100: Eggs
# 1362: Cookies
# 1344: Cereal
# 1290: Canned soup
# 1484: Carbonated drinks
# 1493: Candy (chocolate)
# 1323: Potato chips
# 7870: Batteries
# 7734: Paper towels
# 2672: Ice cream
# 8404: Tooth cleaners
# 3603: Yogurt
# 7008: Detergent (light-duty)
# 7012: Detergent (heavy-duty, liquid)
# 7003: Detergent (packaged)

library(data.table)
library(stringr)
path <- "/home/mallick/Desktop/Nielsen/Data/Consumer_Panel_Data_2004-2016/nielsen_extracts/HMS/"
keyProds <- c(4000, 3625, 7260, 7265, 7270, 4100, 1362, 1344, 1290, 1484, 1493, 1323, 7870,
              7734, 2672, 8404, 3603, 7008, 7012, 7003)

prod <- na.omit(fread(paste0(path, "Master_Files/Latest/products.tsv"), quote = ""))
prod <- prod[department_code != 99]
prod <- prod[!product_module_code %in% 445:468]
prod <- prod[product_module_code != 7371]
prod <- prod[product_module_code != 7421]
prod <- prod[product_module_code != 8522]
prod <- prod[product_module_code %in% keyProds]

# Convert pounds, milliliters, and quarts to ounces
prod[size1_units == "ML", ':=' (size1_amount = size1_amount * 0.03381413,
                                size1_units = "OZ")]
prod[size1_units == "PO", ':=' (size1_amount = size1_amount * 16,
                                size1_units = "OZ")]
prod[size1_units == "QT", ':=' (size1_amount = size1_amount * 32,
                                size1_units = "OZ")]

# Cleaning toilet paper observations ###########################################
# Recoding based on manual inspection and purchases
# Generally, if the product is in the ALL or RMS data, that seems to be more
# correct than if it is in the HMS dataset. Some are not in RMS, so I use my
# best judgement
prod[upc == 1115032045 & upc_ver_uc %in% 1:2, "multi" := 6]
prod[upc == 2898513456, "upc_descr" := gsub("350", "350S", upc_descr)]
prod[upc == 3003402434 & upc_ver_uc == 2, "multi" := 6]
prod[upc == 3600010669 & upc_ver_uc == 1, "multi" := 3]
prod[upc == 3600011643 & upc_ver_uc == 3, "multi" := 6]
prod[upc == 3600041605 & upc_ver_uc == 1, "multi" := 5]
prod[upc == 3600064119 & upc_ver_uc %in% 1:2, "multi" := 6]
prod[upc == 3600067652 & upc_ver_uc == 1, "multi" := 6]
prod[upc == 3600067667 & upc_ver_uc == 1, "multi" := 5]
prod[upc == 3600067794 & upc_ver_uc == 1, "multi" := 2]
prod[upc == 3700002045 & upc_ver_uc == 1, "multi" := 2]
prod[upc == 3700006470 & upc_ver_uc %in% 1:2, "multi" := 6]
prod[upc == 3700006474 & upc_ver_uc %in% 1:2, "multi" := 9]
prod[upc == 3700012387 & upc_ver_uc %in% 1:3, c("multi", "size1_amount") := .(6, 4)]
prod[upc == 3700024064 & upc_ver_uc == 1, "multi" := 5]
prod[upc == 3700029315 & upc_ver_uc == 2, "size1_amount" := 24]
prod[upc == 3700029319 & upc_ver_uc == 3, "multi" := 4]
prod[upc == 3700032666 & upc_ver_uc == 2, "multi" := 4]
prod[upc == 3700034030 & upc_ver_uc == 2, c("multi", "size1_amount") := .(6, 6)]
prod[upc == 3700044975 & upc_ver_uc == 1, "multi" := 2]
prod[upc == 3700046162 & upc_ver_uc == 1, "multi" := 4]
prod[upc == 3700046821 & upc_ver_uc == 1, "multi" := 6]
prod[upc == 3700047936 & upc_ver_uc == 2, "multi" := 9]
prod[upc == 3700083747 & upc_ver_uc == 1, "multi" := 4]
prod[upc == 4116344619 & upc_ver_uc %in% 1:2, "multi" := 6]
prod[upc == 4116344624 & upc_ver_uc %in% 1:2, "multi" := 6]
prod[upc == 4116344628 & upc_ver_uc == 1, "multi" := 1]
prod[upc == 4127079116 & upc_ver_uc == 2, "upc_descr" := "CTL BR DR W 2P 264S TT"]
prod[upc == 4127079116 & upc_ver_uc == 1, "size1_amount" := 12]
prod[upc == 4200086237 & upc_ver_uc == 1, "multi" := 5]
prod[upc == 4200086510 & upc_ver_uc %in% 1:2, "multi" := 6]
prod[upc == 4200087146 & upc_ver_uc == 2, "size1_amount" := 9]
prod[upc == 4218739989 & upc_ver_uc == 1, "size1_amount" := 6]
prod[upc == 4218739989 & upc_ver_uc == 2, c("multi", "size1_amount") := .(6, 4)]
prod[upc == 4303202208 & upc_ver_uc == 2, "multi" := 1]
prod[upc == 4303202208 & upc_ver_uc == 3, "size1_amount" := 20]
prod[upc == 4303203408 & upc_ver_uc == 1, "size1_amount" := 1]
prod[upc == 5400041210 & upc_ver_uc == 1, "multi" := 20]
prod[upc == 5400042320 & upc_ver_uc %in% 1:2, "multi" := 20]
prod[upc == 5400042330 & upc_ver_uc == 2, "multi" := 30]
prod[upc == 6132835112 & upc_ver_uc == 1, "size1_amount" := 12]
prod[upc == 61429940221 & upc_ver_uc == 2, "size1_amount" := 4]
prod[upc == 7200015561 & upc_ver_uc == 1, "multi" := 1]
prod[upc == 7417505815 & upc_ver_uc == 1, "size1_amount" := 6]
prod[upc == 7417505831 & upc_ver_uc == 1, "size1_amount" := 24]
prod[upc == 7789022903 & upc_ver_uc == 1, "multi" := 1]
prod[upc == 7874201206 & upc_ver_uc == 2, "size1_amount" := 4]
prod[upc == 88867001012 & upc_ver_uc == 2, "multi" := 1]
prod[upc == 9661915988 & upc_ver_uc == 1, "multi" := 6]
prod[upc == 9661915988 & upc_ver_uc == 2, "size1_amount" := 6]
prod[upc == 18368900019 & upc_ver_uc == 2, "size1_amount" := 24]
prod[upc == 72645900024 & upc_ver_uc == 2, "size1_amount" := 24]
prod[upc == 72796901557 & upc_ver_uc == 1, "size1_amount" := 12]

### Only keeping products with the most prevalent units in that module.
# There are some products which have different units that the standard
# ones in that category, so this addresses that.
prod[, "count" := .N, by = .(product_module_code, size1_units)]
prod[, "max" := max(count), by = product_module_code]
prod <- prod[count == max]
prod[, c("count", "max") := NULL]

# ### Adding "Large volume" indicator using 2004 as benchmark year.
# # Large is defined as the 90th percentile of sizes purchased in 2004.
# purch <- fread("/home/mallick/Desktop/Nielsen/Data/Consumer_Panel_Data_2004-2016/Clean/Purchases/purchase2004.csv",
#                select = c("upc", "upc_ver_uc"))
# bench <- merge(purch, prod, by = c("upc", "upc_ver_uc"))
# bench[, "cutoff" := quantile(size1_amount, probs = 0.9), by = product_module_code]
# cutoffs <- unique(bench[, .(product_module_code, cutoff)])
# prod <- merge(prod, cutoffs, by = "product_module_code")
# prod[, "large" := size1_amount > cutoff]

fwrite(prod, "/home/mallick/Desktop/Nielsen/Data/Clean/prod.csv")
