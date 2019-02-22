# Computing basket share that is storable bulk discounted items
library(data.table)
threads <- 8

# Getting panel and purchase data
prod <- fread("/home/upenn/hossaine/Nielsen/Data/prod.csv",
              select = c("upc", "upc_ver_uc", "product_module_descr"),
              key = c("upc", "upc_ver_uc"))
panel <- fread("/home/upenn/hossaine/Nielsen/Data/fullPanel.csv",
               select = c("household_code", "panel_year", "projection_factor",
                          "household_size", "household_income"),
               key = c("household_code", "panel_year"))
purch <- fread("/home/upenn/hossaine/Nielsen/Data/purchase.csv", nThread = threads,
               key = c("trip_code_uc", "upc", "upc_ver_uc"))
trips <- fread("/scratch/upenn/hossaine/trips.csv", nThread = threads,
               key = c("trip_code_uc", "household_code", "panel_year"))

purch <- merge(purch, trips, by = "trip_code_uc")
purch <- merge(purch, panel, by = c("household_code", "panel_year"))
purch <- merge(purch, prod, by = c("upc", "upc_ver_uc"))
rm(trips, panel, prod)

purch[product_module_descr == "DETERGENTS - HEAVY DUTY - LIQUID", "product_module_descr" := "DETERGENT"]
purch[product_module_descr == "DETERGENTS-PACKAGED", "product_module_descr" := "DETERGENT"]
purch[product_module_descr == "DETERGENTS - LIGHT DUTY", "product_module_descr" := "DETERGENT"]
purch[product_module_descr == "PAPER TOWELS - REGULAR", "product_module_descr" := "PAPER TOWELS"]
purch[product_module_descr == "PAPER TOWELS - JUMBO", "product_module_descr" := "PAPER TOWELS"]

meanSpend <- purch[, .(spend = sum(total_price_paid)),
                   by = .(household_code, panel_year, household_income,
                          projection_factor, product_module_descr)]
meanSpend <- meanSpend[, weighted.mean(spend, w = projection_factor),
                       by = .(product_module_descr, household_income)]

meanTot <- purch[, .(tot = mean(hhTotal)),
                 by = .(household_code, panel_year, projection_factor, household_income)]
meanTot[, weighted.mean(tot, w = projection_factor), by = household_income]
