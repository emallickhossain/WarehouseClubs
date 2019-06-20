# Computes bulk discounts across all products
# Fact 1: Bulk discounts apply to 97% of product modules with more than 3 sizes
# Fact 2: Bulk discounts are deeper for storable products compared to perishable products.
library(data.table)
library(lfe)
library(ggplot2)
library(ggthemes)
library(readxl)
threads <- 8

# Get purchase data
purch <- NULL
for (yr in 2004:2017){
  dat <- fread(paste0("/scratch/upenn/hossaine/fullPurch", yr, ".csv"), nThread = threads,
               select = c("trip_code_uc", "packagePrice", "product_module_code",
                          "brand_code_uc", "totalAmount", "storable"))
  purch <- rbindlist(list(purch, dat), use.names = TRUE)
  rm(dat)
}

purch[, c("unitPrice", "packagePrice") := .(packagePrice / totalAmount, NULL)]

trips <- fread("/scratch/upenn/hossaine/fullTrips.csv", nThread = threads,
               select = c("trip_code_uc", "retailer_code", "household_code",
                          "panel_year", "purchase_date"))

panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("panel_year", "household_code", "dma_cd"))

# Getting CPI to deflate values to Jan-2010 base
cpi <- fread("/scratch/upenn/hossaine/cpi.csv")
cpi[, "base" := cpi[date == "2010-01-01"]$value]
cpi[, "newIndex" := value / base * 100]
cpi[, c("base", "value") := NULL]
cpi[, "yearMonth" := .(substr(date, 1, 7))]
cpi[, "date" := NULL]

# Combining trips and panel to get market, retailer, and dates
setkey(trips, household_code, panel_year)
setkey(panel, household_code, panel_year)
fullData <- merge(trips, panel, by = c("household_code", "panel_year"))
fullData[, c("household_code", "panel_year") := NULL]
rm(trips, panel)

# Combining with CPI for price deflation
fullData[, "yearMonth" := .(substr(purchase_date, 1, 7))]
setkey(fullData, yearMonth)
setkey(cpi, yearMonth)
fullData <- merge(fullData, cpi, by = "yearMonth")
fullData[, c("purchase_date") := NULL]
rm(cpi)

# Combining with purchase data
setkey(fullData, trip_code_uc)
setkey(purch, trip_code_uc)
fullData <- merge(fullData, purch, by = "trip_code_uc")
fullData[, "trip_code_uc" := NULL]
rm(purch)

# Deflating unit prices and taking logs
fullData[, "unitPriceReal" := unitPrice / newIndex * 100]
fullData[, c("unitPrice", "newIndex") := NULL]
fullData[, c("logAmount", "logUnitPriceReal") := .(log(totalAmount), log(unitPriceReal))]
fullData[, c("totalAmount", "unitPriceReal") := NULL]
fwrite(fullData, "/scratch/upenn/hossaine/bulkDiscountAllProds.csv", nThread = threads)

# Getting fixed effects
fullData[, "brandRetailer" := paste(brand_code_uc, retailer_code, sep = "_")]

# Running regression of log unit price on log package size
fullData <- fread("/scratch/upenn/hossaine/bulkDiscountAllProds.csv", nThread = threads)
badMods <- c(1117, 7529, 7697)
coefs <- NULL
for (i in sort(unique(fullData$product_module_code))) {
  print(i)
  if (!i %in% badMods) {
    reg <- felm(logUnitPriceReal ~ logAmount | brandRetailer + yearMonth + dma_cd,
                data = fullData[product_module_code == i])
    coefDT <- as.data.table(summary(reg)$coefficients)
    coefs <- rbindlist(list(coefs, data.table(coefDT, module = i)), use.names = TRUE)
  }
}
fwrite(coefs, "/scratch/upenn/hossaine/bulkDiscountAllProdsBetas.csv")

# Combining betas with storability and generating quantity discount distributions
coefs <- fread("/scratch/upenn/hossaine/bulkDiscountAllProdsBetas.csv")
prod <- fread("/scratch/upenn/hossaine/fullProd.csv", nThread = threads)
stor <- unique(prod[, .(module = product_module_code,
                        storable = factor(storable, levels = c(0, 1),
                                          labels = c("Non-Storable", "Storable")))])
coefs <- merge(coefs, stor, by = "module")
fwrite(coefs, "/scratch/upenn/hossaine/toCopyCoefs.csv")

# scp hossaine@wrds-cloud.wharton.upenn.edu:/scratch/upenn/hossaine/toCopyCoefs.csv /home/mallick/Downloads
coefs <- fread("/home/mallick/Downloads/toCopyCoefs.csv")
coefs[abs(`t value`) <= 3, "Estimate" := 0]

# Getting share of products with bulk discounts
sum(coefs$Estimate < 0) / nrow(coefs)
coefs[, median(Estimate), by = storable]

# Plotting distribution of betas
ggplot(data = coefs, aes(x = Estimate, fill = storable)) +
  geom_histogram(aes(y = ..density..), bins = 50, alpha = 0.5) +
  geom_density(alpha = 0.8) +
  scale_x_continuous(limits = c(-2, 1)) +
  labs(title = "Bulk Discounts Are Common and Substantial",
       subtitle = "Discounts are Larger for Storable Items",
       x = "Unit Price Elasticity",
       y = "Density",
       caption = paste0("Source: Author calculations from Nielsen Consumer Panel. \n",
                        "Note: Unit price elasticity denotes percent reduction in ",
                        "unit price per 1 percent increase \nin package size after ",
                        "controlling for brand-retailer, year-month, and market effects. \n",
                        "Sample consists of products purchased between 2004-2017.")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0)) +
  scale_fill_grey()

ggsave(filename = "./code/5_figures/bulkDiscountAllProds.png", height = 6, width = 6)

##############  EXTREME FIXED EFFECTS ##########################################
# Running regression of log unit price on log package size
fullData <- fread("/scratch/upenn/hossaine/bulkDiscountAllProds.csv", nThread = threads)
fullData[, "brandRetTimeMarket" := paste(brandRetailer, yearMonth, dma_cd, sep = "_")]
badMods <- c(1117, 1245, 2662, 2663, 3584, 7141, 7184, 7186, 7529, 7697, 7742, 8620)
coefs <- NULL
for (i in sort(unique(fullData$product_module_code))) {
  print(i)
  if (!i %in% badMods) {
    reg <- felm(logUnitPriceReal ~ logAmount | brandRetTimeMarket,
                data = fullData[product_module_code == i])
    coefDT <- as.data.table(summary(reg)$coefficients)
    coefs <- rbindlist(list(coefs, data.table(coefDT, module = i)), use.names = TRUE)
  }
}
fwrite(coefs, "/scratch/upenn/hossaine/bulkDiscountAllProdsBetas2.csv")

# Combining betas with storability and generating quantity discount distributions
coefs <- fread("/scratch/upenn/hossaine/bulkDiscountAllProdsBetas2.csv")
prod <- fread("/scratch/upenn/hossaine/fullProd.csv", nThread = threads)
stor <- unique(prod[, .(module = product_module_code,
                        storable = factor(storable, levels = c(0, 1),
                                          labels = c("Non-Storable", "Storable")))])
coefs <- merge(coefs, stor, by = "module")
fwrite(coefs, "/scratch/upenn/hossaine/toCopyCoefs.csv")

# scp hossaine@wrds-cloud.wharton.upenn.edu:/scratch/upenn/hossaine/toCopyCoefs.csv /home/mallick/Downloads
coefs <- fread("/home/mallick/Downloads/toCopyCoefs.csv")
coefs[abs(`t value`) <= 3, "Estimate" := 0]

# Getting share of products with bulk discounts
sum(coefs$Estimate < 0) / nrow(coefs)

# Plotting distribution of betas
ggplot(data = coefs, aes(x = Estimate, fill = storable)) +
  geom_histogram(aes(y = ..density..), bins = 50, alpha = 0.5) +
  geom_density(alpha = 0.8) +
  scale_x_continuous(limits = c(-2, 1)) +
  labs(title = "Bulk Discounts Are Common and Substantial",
       subtitle = "Discounts are Larger for Storable Items",
       x = "Unit Price Elasticity",
       y = "Density",
       caption = paste0("Source: Author calculations from Nielsen Consumer Panel. \n",
                        "Note: Unit price elasticity denotes percent reduction in ",
                        "unit price per 1 percent increase \nin package size after ",
                        "controlling for brand-retailer-year-month-market fixed effects. \n",
                        "Sample consists of products purchased between 2004-2017.")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0)) +
  scale_fill_grey()

ggsave(filename = "./code/5_figures/bulkDiscountAllProds.png", height = 6, width = 6)
