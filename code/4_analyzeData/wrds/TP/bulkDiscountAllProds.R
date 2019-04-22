# Computes bulk discounts across all products
library(data.table)
library(lfe)
library(ggplot2)
library(ggthemes)
library(readxl)
threads <- 8
path <- "/scratch/upenn/hossaine/nielsen_extracts/HMS/"

# Getting products
prod <- na.omit(fread(paste0(path, "Master_Files/Latest/products.tsv"), quote = "",
                      nThread = threads, key = c("upc", "upc_ver_uc")))
storable <- fread(paste0("/scratch/upenn/hossaine/storableClassification.csv"))
prod <- merge(prod, storable, by = c("product_group_code", "product_group_descr"))

# Keeping most common size categories for each module
prod[, "count" := .N, by = .(product_module_code, size1_units)]
prod[, "max" := max(count), by = product_module_code]
prod <- prod[count == max]
prod[, c("count", "max") := NULL]
prod[, "totalAmount" := multi * size1_amount]
prod <- prod[, .(upc, upc_ver_uc, product_module_code, brand_code_uc, totalAmount,
                 size1_units, storable)]

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

fullPurch <- NULL
for (yr in 2004:2017) {
  print(yr)
  # Getting purchases and only keeping those with positive prices
  # Computing unit costs for a single package (e.g if they purchased 2 10oz
  # packages for $10, then each was $5, so the unit cost is $0.50/oz and the
  # pack size is 10oz)
  purch <- fread(paste0(path, yr, "/Annual_Files/purchases_", yr, ".tsv"),
                 nThread = threads, key = c("upc", "upc_ver_uc"))
  purch <- merge(purch, prod, by = c("upc", "upc_ver_uc"))[total_price_paid > 0]
  purch[, "logUnitCost" := log((total_price_paid / quantity) / totalAmount)]
  purch <- purch[, .(trip_code_uc, product_module_code, brand_code_uc, logUnitCost,
                     logQ = log(totalAmount))]
  setkey(purch, trip_code_uc)

  # Getting trips
  trips <- fread(paste0(path, yr, "/Annual_Files/trips_", yr, ".tsv"),
                 nThread = threads, key = "trip_code_uc")
  purch <- merge(purch, trips, by = "trip_code_uc")
  purch <- purch[, .(product_module_code, brand_code_uc, logUnitCost, logQ, retailer_code,
                     store_code_uc, purchase_date, household_code, panel_year)]
  purch[, "yearMonth" := substr(purchase_date, 1, 7)]
  setkey(purch, household_code, panel_year)

  # Adding markets from panel
  panel <- fread(paste0(path, yr, "/Annual_Files/panelists_", yr, ".tsv"), nThread = threads,
                 select = c("Panel_Year", "Household_Cd", "DMA_Cd"),
                 col.names = c("panel_year", "household_code", "dma"))
  purch <- merge(purch, panel, by = c("household_code", "panel_year"))
  purch[, c("household_code", "panel_year", "purchase_date") := NULL]

  # Only keeping modules with more than 100 recorded purchases
  purch[, "modCount" := .N, by = product_module_code]
  purch <- purch[modCount > 100]

  # Only keeping modules with more than 3 unique sizes purchased
  purch[, "uniqueSizes" := uniqueN(logQ), by = product_module_code]
  purch <- purch[uniqueSizes > 3]
  fullPurch <- rbindlist(list(fullPurch, purch), use.names = TRUE)
}

# Saving data
rm(purch, trips, panel)
fwrite(fullPurch, "/scratch/upenn/hossaine/fullPurchBulkAllProds.csv")

# Reading data
fullPurch <- fread("/scratch/upenn/hossaine/fullPurchBulkAllProds.csv", nThread = threads)

# Generating brand-retailer fixed effect
fullPurch[, "brandRetailer" := paste0(brand_code_uc, retailer_code)]
# Running regression
coefs <- NULL
for (i in sort(unique(fullPurch$product_module_code))) {
  print(i)
  reg <- felm(logUnitCost ~ logQ | brandRetailer + yearMonth + dma,
              data = fullPurch[product_module_code == i])
  coefDT <- as.data.table(summary(reg)$coefficients)
  coefs <- rbindlist(list(coefs, data.table(coefDT, module = i)), use.names = TRUE)
}

fwrite(coefs, "/home/upenn/hossaine/Nielsen/Data/bulkDiscountBetas.csv")

coefs <- fread("/home/upenn/hossaine/Nielsen/Data/bulkDiscountBetas.csv")
stor <- unique(prod[, .(module = product_module_code,
                        storable = factor(storable, levels = c(0, 1),
                                          labels = c("Non-Storable", "Storable")))])
coefs <- merge(coefs, stor, by = "module")
fwrite(coefs, "/home/upenn/hossaine/Nielsen/Data/bulkDiscountBetas.csv")
ggplot(data = coefs[Estimate > -2 & Estimate < 1], aes(x = Estimate)) +
  geom_histogram(aes(y = ..density..), bins = 50) +
  geom_density() +
  scale_x_continuous(limits = c(-2, 1)) +
  labs(title = "Bulk Discounts Are Common and Vary Widely",
       x = "Unit Price Elasticity",
       y = "Density",
       caption = paste0("Source: Author calulations from Nielsen Consumer Panel. \n",
                        "Note: Unit price elasticity denotes percent reduction in ",
                        "unit price per 1 percent increase \nin package size after ",
                        "controlling for brand-retailer, year-month, and market effects. \n",
                        "Sample consists of products purchased between 2004-2017.")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0))
ggsave(filename = "./figures/bulkDiscountAllProds.png")

# scp hossaine@wrds-cloud.wharton.upenn.edu:/home/upenn/hossaine/Nielsen/Data/bulkDiscountBetas.csv /home/mallick/Downloads
library(ggplot2)
library(ggthemes)
coefs <- fread("/home/mallick/Downloads/bulkDiscountBetas.csv")
ggplot(data = coefs[Estimate > -2 & Estimate < 1],
       aes(x = Estimate, group = storable, fill = storable)) +
  geom_density(alpha = 0.5) +
  scale_x_continuous(limits = c(-2, 1)) +
  labs(title = "Bulk Discounts Are Common and Vary Widely",
       x = "Unit Price Elasticity",
       y = "Density",
       caption = paste0("Source: Author calulations from Nielsen Consumer Panel. \n",
                        "Note: Unit price elasticity denotes percent reduction in ",
                        "unit price per 1 percent increase \nin package size after ",
                        "controlling for brand-retailer, year-month, and market effects. \n",
                        "Sample consists of products purchased between 2004-2017.")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0))
ggsave(filename = "./code/5_figures/bulkDiscountAllProdsByStorability.png")

