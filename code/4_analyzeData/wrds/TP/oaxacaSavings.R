# Do Oaxaca decomposition on unit cost savings in each product module
library(data.table)
library(oaxaca)
library(readxl)
library(lfe)
threads <- 8
path <- "/scratch/upenn/hossaine/nielsen_extracts/HMS/"

# Classifying products as bulk/not and storable/not
storable <- fread(paste0("/scratch/upenn/hossaine/storableClassification.csv"))
retailers <- fread("/scratch/upenn/hossaine/retailers.csv")
panel <- fread("/scratch/upenn/hossaine/fullPanel.csv",
               select = c("panel_year", "fips", "household_code",
                          "projection_factor", "household_income", "household_size",
                          "age", "child", "market"))
panel[, "poor" := NA_integer_]
panel[household_income < 15, "poor" := 1L]
panel[household_income >= 27, "poor" := 0L]

# Getting products
prod <- na.omit(fread(paste0(path, "Master_Files/Latest/products.tsv"), quote = "",
                      nThread = threads, key = c("upc", "upc_ver_uc")))

# Keeping most common size categories for each module
prod[, "count" := .N, by = .(product_module_code, size1_units)]
prod[, "max" := max(count), by = product_module_code]
prod <- prod[count == max]
prod[, c("count", "max") := NULL]
prod[, "totalAmount" := multi * size1_amount]

# Excluding "deferred" modules per Nielsen's suggestion (while there are 164 of
# these, only 60 are actually in the products file)
deferred <- setDT(read_xlsx("/home/upenn/hossaine/Nielsen/Data/Product_Hierarchy.xlsx"))
deferred <- unique(deferred[, .(product_module_code, `Deferred (Please see documentation for explanation and notes)`)])
setnames(deferred, c("product_module_code", "deferred"))
prod <- merge(prod, deferred, by = "product_module_code")
prod <- prod[is.na(deferred)]
prod[, "deferred" := NULL]

# Excluding all alcohol purchases
prod <- prod[!product_module_code %in% c(5000:5060, 7806)]

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
prod <- merge(prod, storable, by = c("product_group_code", "product_group_descr"))
prod <- prod[, .(upc, upc_ver_uc, product_module_code, brand_code_uc,
                 totalAmount, size1_units, quartile, storable)]

# Getting all purchases by year
for (i in 2004:2017) {
  print(i)
  purch <- fread(paste0(path, i, "/Annual_Files/purchases_", i, ".tsv"), nThread = threads)
  purch <- merge(purch, prod, by = c("upc", "upc_ver_uc"))
  purch[, ':=' (coupon = ifelse(coupon_value > 0, 1L, 0L),
                sale = ifelse(deal_flag_uc > 0 & coupon_value == 0, 1L, 0L),
                generic = ifelse(brand_code_uc == 536746, 1L, 0L),
                price_paid_wCoupon = total_price_paid - coupon_value)]
  purch[, "packagePrice" := price_paid_wCoupon / quantity]
  purch[, c("upc_ver_uc", "quantity", "total_price_paid", "coupon_value", "deal_flag_uc") := NULL]

  trips <- fread(paste0(path, i, "/Annual_Files/trips_", i, ".tsv"), nThread = threads,
                 select = c("trip_code_uc", "household_code", "panel_year", "retailer_code"))
  purch <- merge(purch, trips, by = "trip_code_uc")
  purch <- merge(purch, retailers, by = "retailer_code")
  purch <- merge(purch, panel, by = c("household_code", "panel_year"))

  # Only keeping modules with more than 100 recorded purchases
  purch[, "modCount" := .N, by = product_module_code]
  purch <- purch[modCount > 100]

  # Only keeping modules with more than 3 unique sizes purchased
  purch[, "uniqueSizes" := uniqueN(totalAmount), by = product_module_code]
  purch <- purch[uniqueSizes > 3]
  purch[, c("modCount", "uniqueSizes", "trip_code_uc") := NULL]

  purch[, ':=' (club = ifelse(channel_type == "Warehouse Club", 1L, 0L),
                dollar = ifelse(channel_type == "Dollar Store", 1L, 0L),
                bulk = ifelse(quartile >= 4, 1L, 0L))]
  purch[, "unitCost" := packagePrice / totalAmount]
  purch <- purch[unitCost > 0.01]
  purch[, "lUnitCost" := log(unitCost)]
  purch[, "retailerBrand" := paste0(retailer_code, product_module_code, brand_code_uc)]
  purch[, "household_income" := factor(household_income)]

  # Computing Oaxaca decomposition of unit cost differences
  tp <- purch[product_module_code == 7260]
  tp <- na.omit(tp, cols = "poor")
  regAll <- oaxaca(data = tp,
                   lUnitCost ~ coupon + sale + generic + bulk | poor)
  regAll$threefold$overall
}
