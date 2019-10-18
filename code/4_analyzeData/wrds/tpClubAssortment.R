# Gets TP assortment at warehouse clubs and at dollar stores
# find /scratch/upenn/hossaine/nielsen_extracts/RMS/ -exec touch {} \;

library(data.table)
threads <- 8
path <- "/scratch/upenn/hossaine/nielsen_extracts/RMS/"

prod <- fread("/scratch/upenn/hossaine/prodTP.csv",
              drop = c("upc_descr", "brand_code_uc"))
prod <- prod[brand_descr %in% c("CHARMIN", "SCOTT 1000", "ANGEL SOFT", "CTL BR",
                                "KLEENEX COTTONELLE", "QUILTED NORTHERN")]
tp <- fread(paste0(path, "2016/Movement_Files/4507_2016/7260_2016.tsv"),
            nThread = threads, select = c("store_code_uc", "upc", "price"))
store <- fread(paste0(path, "2016/Annual_Files/stores_2016.tsv"),
               select = c("store_code_uc", "retailer_code"))
rms <- fread(paste0(path, "2016/Annual_Files/rms_versions_2016.tsv"), drop = "panel_year")
tp <- merge(tp, store, by = "store_code_uc")
tp <- merge(tp, rms, by = "upc")

# Club assortment
tpClub <- tp[retailer_code %in% 9101:9111]
tpClub <- merge(tpClub, prod, by = c("upc", "upc_ver_uc"))
tpClub <- tpClub[, .(price = mean(price)), by = .(brand_descr, rolls, totalSheet)]
tpClub[, ':=' (unitPrice = price / totalSheet * 100 * 2,
               large12 = (rolls > 12),
               brandRollSheet = paste(brand_descr, rolls, totalSheet, sep = "_"),
               channel_type = "Warehouse Club")]
fwrite(tpClub, file = "/scratch/upenn/hossaine/tpClub.csv")

# Dollar store assortment
tpDollar <- tp[retailer_code %in% 5850:5865]
tpDollar <- merge(tpDollar, prod, by = c("upc", "upc_ver_uc"))
tpDollar <- tpDollar[, .(price = mean(price)), by = .(brand_descr, rolls, totalSheet)]
tpDollar[, ':=' (unitPrice = price / totalSheet * 100 * 2,
                 large12 = (rolls > 12),
                 brandRollSheet = paste(brand_descr, rolls, totalSheet, sep = "_"),
                 channel_type = "Dollar Store")]
fwrite(tpDollar, file = "/scratch/upenn/hossaine/tpDollar.csv")

# Drug store assortment
tpDrug <- tp[retailer_code %in% 4901:4998]
tpDrug <- merge(tpDrug, prod, by = c("upc", "upc_ver_uc"))
tpDrug <- tpDrug[, .(price = mean(price)), by = .(brand_descr, rolls, totalSheet)]
tpDrug[, ':=' (unitPrice = price / totalSheet * 100 * 2,
               large12 = (rolls > 12),
               brandRollSheet = paste(brand_descr, rolls, totalSheet, sep = "_"),
               channel_type = "Drug Store")]
fwrite(tpDrug, file = "/scratch/upenn/hossaine/tpDrug.csv")

# Discount store assortment
tpDiscount <- tp[retailer_code %in% 6901:6926]
tpDiscount <- merge(tpDiscount, prod, by = c("upc", "upc_ver_uc"))
tpDiscount <- tpDiscount[, .(price = mean(price)), by = .(brand_descr, rolls, totalSheet)]
tpDiscount[, ':=' (unitPrice = price / totalSheet * 100 * 2,
                   large12 = (rolls > 12),
                   brandRollSheet = paste(brand_descr, rolls, totalSheet, sep = "_"),
                   channel_type = "Discount Store")]
fwrite(tpDiscount, file = "/scratch/upenn/hossaine/tpDiscount.csv")

# Grocery store assortment
tpGrocery <- tp[retailer_code %in% 1:1006]
tpGrocery <- merge(tpGrocery, prod, by = c("upc", "upc_ver_uc"))
tpGrocery <- tpGrocery[, .(price = mean(price)), by = .(brand_descr, rolls, totalSheet)]
tpGrocery[, ':=' (unitPrice = price / totalSheet * 100 * 2,
                  large12 = (rolls > 12),
                  brandRollSheet = paste(brand_descr, rolls, totalSheet, sep = "_"),
                  channel_type = "Grocery Store")]
fwrite(tpGrocery, file = "/scratch/upenn/hossaine/tpGrocery.csv")
