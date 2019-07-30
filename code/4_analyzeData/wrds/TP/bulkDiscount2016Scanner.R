# Computes bulk discounts in the 2016 Scanner data
library(data.table)
library(lfe)
library(purrr)
library(ggplot2)
library(ggthemes)
library(stringr)
threads <- 8
path <- "/scratch/upenn/hossaine/nielsen_extracts/RMS/"

# Download 2016 Scanner from Globus for Sanitary products and departments 1:7
# Unzip

# Load data
prod <- fread("/scratch/upenn/hossaine/fullProd.csv", nThread = threads,
              select = c("upc", "upc_ver_uc", "brand_code_uc",
                         "totalAmount", "product_module_code"))
rms <- fread(paste0("/scratch/upenn/hossaine/nielsen_extracts/RMS/2016/",
                    "Annual_Files/rms_versions_2016.tsv"), drop = "panel_year")

fileNames <- list.files("/scratch/upenn/hossaine/nielsen_extracts/RMS/2016/Movement_Files",
                        recursive = TRUE, full.names = TRUE)

fullBetas <- NULL
for (i in fileNames) {
  tryCatch({
    assort <- fread(i, select = c("upc", "store_code_uc", "week_end", "price"), nThread = threads)
    assort <- merge(assort, rms, by = "upc")
    assort <- merge(assort, prod, by = c("upc", "upc_ver_uc"))[, c("upc", "upc_ver_uc") := NULL]
    assort[, "storeWeekBrand" := paste(store_code_uc, week_end, brand_code_uc, sep = "_")]
    assort[, c("store_code_uc", "week_end", "brand_code_uc") := NULL]
    assort[, c("lunitPrice", "lq", "price") := .(log(price / totalAmount), log(totalAmount), NULL)]
    modCode <- unique(assort$product_module_code)
    reg <- felm(data = assort, lunitPrice ~ lq | storeWeekBrand)
    betaCoef <- as.data.table(summary(reg)$coefficients)
    betaCoef[, "mod" := modCode]
    fullBetas <- rbindlist(list(fullBetas, betaCoef), use.names = TRUE)
    print(modCode)
  }, error = function(e){})
}

# Estimating savings of moving from 2nd quintile to 4th quintile
prod <- fread("/scratch/upenn/hossaine/fullProd.csv",
              select = c("product_module_code", "totalAmount", "quintile", "food"))
quintileSize <- prod[quintile %in% c(2, 4), .(avgSize = mean(totalAmount)),
                     by = .(quintile, food, product_module_code)]
quintileSizeWide <- dcast(quintileSize, food + product_module_code ~ quintile)
quintileSizeWide[, "logSizeRatio" := log(`4` / `2`)]
fwrite(quintileSizeWide, "/scratch/upenn/hossaine/toCopy.csv")

#fwrite(fullBetas, "/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas1x.csv")
# tampons and pads
#fwrite(fullBetas, "/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas1z.csv")
# 1z covers groups 508:1001
#fwrite(fullBetas, "/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas1y.csv")
# 1y covers groups 1002:1021
#fwrite(fullBetas, "/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas1a.csv")
# 1a only ran through until 1092 and 1125 (groups 501:507)
#fwrite(fullBetas, "/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas1b.csv")
# 1b only ran through 1362, 4000:4009, 4011, 4012, 1484, and 1553
#fwrite(fullBetas, "/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas1c.csv")
#fwrite(fullBetas, "/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas2.csv")
#fwrite(fullBetas, "/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas3-4.csv")
#fwrite(fullBetas, "/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas5-6.csv")
#fwrite(fullBetas, "/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas7.csv")

# Getting modules and food classification
prod <- unique(fread("/scratch/upenn/hossaine/fullProd.csv",
                     select = c("product_module_code", "food")))
fwrite(prod, "/home/upenn/hossaine/Nielsen/Data/prodFood.csv")

# Download and generate graph
# scp hossaine@wrds-cloud.wharton.upenn.edu:/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas*.csv /home/mallick/Downloads
# scp hossaine@wrds-cloud.wharton.upenn.edu:/home/upenn/hossaine/Nielsen/Data/prodFood.csv /home/mallick/Downloads
getCoefs <- function(i) {
  data <- fread(paste0("/home/mallick/Downloads/scannerBulkDiscountBetas", i, ".csv"))
  return(data)
}
coefs <- rbindlist(map(c("1a", "1b", "1c", "1x", "1y", "1z", "2", "3-4", "5-6", "7"), getCoefs), use.names = TRUE)
coefs[abs(`t value`) <= 3, "Estimate" := 0]

# Getting food classification
prod <- fread("/home/mallick/Downloads/prodFood.csv")
setnames(prod, "product_module_code", "mod")
coefs <- merge(coefs, prod, by = "mod")
coefs[, "foodChar" := ifelse(food == 1, "Food", "Non-Food")]

# Getting share of products with bulk discounts
sum(coefs$Estimate < 0) / nrow(prod)

# Plotting distribution of betas
ggplot(data = coefs, aes(x = Estimate, fill = foodChar)) +
  geom_histogram(aes(y = ..density..), bins = 50, alpha = 0.5, position = "identity") +
  geom_density(alpha = 0.8) +
  scale_x_continuous(limits = c(-2, 1)) +
  labs(title = "Bulk Discounts Are Common",
       subtitle = "Discounts are Larger for Non-Food Items",
       x = "Unit Price Change",
       y = "Density",
       fill = "Product Type",
       caption = paste0("Source: Author calculations from Nielsen Scanner Data for 2016. \n",
                        "Note: 'Unit price change' denotes percent reduction in ",
                        "unit price per 1 percent increase \nin package size after ",
                        "controlling for brand-store-week fixed effects.")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0)) +
  scale_fill_grey()

ggsave(filename = "./code/5_figures/bulkDiscountAllProdsScanner.png", height = 6, width = 6)

# Getting estimated savings of moving from 2nd to 4th quintile
savings <- fread("/home/mallick/Downloads/toCopy.csv")
savings <- merge(savings, coefs, by.x = "product_module_code", by.y = "mod")
savings[, "totalSavings" := 1 - exp(Estimate * logSizeRatio)]
savings[, "foodChar" := ifelse(food == 1, "Food", "Non-Food")]

ggplot(data = savings, aes(x = totalSavings * 100, fill = foodChar)) +
  geom_histogram(aes(y = ..density..), bins = 50, alpha = 0.5, position = "identity") +
  geom_density(alpha = 0.8) +
  scale_x_continuous(limits = c(-10, 100)) +
  labs(title = "Bulk Discounts Offer Large Savings",
       subtitle = "Discounts are Larger for Non-Food Items",
       x = "Savings (%)",
       y = "Density",
       fill = "Product Type",
       caption = str_wrap(paste0("Source: Author calculations from Nielsen Scanner ",
                                 "Data for 2016. Note: Savings denote average percent ",
                                 "decrease in unit price from moving from a package ",
                                 "in the 2nd quintile of the size distribution to ",
                                 "the 4th quintile of the size distribution"))) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0)) +
  scale_fill_grey()
ggsave(filename = "./code/5_figures/bulkDiscountAllProdsScannerSavings.png", height = 6, width = 6)
