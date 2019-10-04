# Getting a quick and dirty estimate of how different unit prices could be
library(data.table)
threads <- 8

# Loading purchase, panel, and product data
tp <- fread("/scratch/upenn/hossaine/fullTPPurchases.csv", nThread = threads,
            select = c("household_code", "panel_year", "packagePrice",
                       "totalAmount", "retailer_code", "store_code_uc",
                       "week_end", "channel_type", "upc_choice", "upc_ver_uc_choice"))
setnames(tp, c("upc_choice", "upc_ver_uc_choice"), c("upc", "upc_ver_uc"))
panel <- fread("/scratch/upenn/hossaine/fullPanel.csv",
               select = c("household_code", "panel_year", "household_income_coarse"))
tp <- merge(tp, panel, by = c("household_code", "panel_year"))
prod <- fread("/scratch/upenn/hossaine/prodTP.csv")
tp <- merge(tp, prod, by = c("upc", "upc_ver_uc"))

# computing unit price
tp[, "unitPrice" := packagePrice / totalSheet]

# Getting max and min unit price by brand-store-week
tp[, c("maxPrice", "minPrice", "patrons") :=
     .(max(unitPrice), min(unitPrice), uniqueN(household_income_coarse)),
   by = .(retailer_code, store_code_uc, channel_type, brand_code_uc, brand_descr, upc_descr)]
tp[, "range" := maxPrice - minPrice]

# Checking for some examples from each store type
disc <- tp[patrons >= 3 & channel_type == "Discount Store" & brand_descr != "CTL BR" &
     store_code_uc != 0 & household_income_coarse %in% c("<25k", ">100k"),
   .(household_income_coarse, store_code_uc, packagePrice, totalAmount,
     retailer_code, channel_type, upc_descr, unitPrice, range)]
setorder(disc, store_code_uc, household_income_coarse)

# Checking on these two households
purch <- fread("/scratch/upenn/hossaine/fullPurch2011.csv", nThread = threads)
trips <- fread("/scratch/upenn/hossaine/fullTrips.csv", nThread = threads,
               select = c("retailer_code", "store_code_uc", "household_code",
                          "panel_year", "trip_code_uc", "purchase_date",
                          "channel_type"))
purch <- merge(purch, trips, by = c("trip_code_uc"))
purch <- merge(purch, panel, by= c("household_code", "panel_year"))
apple <- purch[store_code_uc == 8192375 & trip_code_uc %in% c(1014029279, 1023382611)]


# Looking at paper towels
pt <- purch[product_module_code == 7734 & channel_type == "Discount Store" &
              household_income_coarse %in% c("<25k", ">100k") &
              brand_descr != "CTL BR" & store_code_uc != 0]
pt[, "unitPrice" := packagePrice / totalAmount]
pt[, c("maxPrice", "minPrice", "patrons") :=
     .(max(unitPrice), min(unitPrice), uniqueN(household_income_coarse)),
   by = .(retailer_code, store_code_uc, channel_type, brand_code_uc, brand_descr, upc_descr)]
pt[, "range" := maxPrice - minPrice]

apple <- pt[patrons == 2, .(household_code, trip_code_uc, packagePrice, upc_descr,
                            brand_descr, totalAmount, store_code_uc,
                            household_income_coarse, unitPrice, range)]
setorder(apple, -range)
apple[trip_code_uc %in% c(1012947074, 1022296426)]
