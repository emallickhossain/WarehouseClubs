# Computing coupon savings from Nielsen
library(data.table)
library(ggplot2)
library(ggthemes)
library(purrr)
threads <- 8

################## ON WRDS #####################################################
fullPurch <- NULL
for (i in 2004:2017) {
  print(i)
  purch <- fread(paste0("/scratch/upenn/hossaine/fullPurch", i, ".csv"),
                 nThread = threads,
                 select = c("coupon_value", "packagePrice", "product_module_code",
                            "food", "trip_code_uc"))
  fullPurch <- rbindlist(list(fullPurch, purch), use.names = TRUE)
}

# Getting share of coupon redemptions
fullPurch[, "coupon" := (coupon_value > 0)]
fullPurch[, .(mean = mean(coupon),
              weight = weighted.mean(coupon, w = packagePrice))]
fullPurch[, .(mean = mean(coupon),
              weight = weighted.mean(coupon, w = packagePrice)), by = food]

# Getting savings by product module
fullPurch[, "savings" := coupon_value / packagePrice]
betas <- fullPurch[savings > 0 & savings < 1, .(beta = mean(savings)),
                   by = .(product_module_code, food)]
fwrite(betas, "/scratch/upenn/hossaine/couponBetas.csv")

# Estimating savings of moving from 2nd quintile to 4th quintile
prod <- fread("/scratch/upenn/hossaine/fullProd.csv",
              select = c("product_module_code", "totalAmount", "quintile", "food"))
quintileSize <- prod[quintile %in% c(2, 3, 4), .(avgSize = min(totalAmount, na.rm = TRUE)),
                     by = .(quintile, food, product_module_code)]
quintileSizeWide <- dcast(quintileSize, food + product_module_code ~ quintile,
                          value.var = "avgSize")
quintileSizeWide[, "logSizeRatio24" := log(`4` / `2`)]
quintileSizeWide[, "logSizeRatio34" := log(`4` / `3`)]
quintileSizeWide[, "logSizeRatio23" := log(`3` / `2`)]
fwrite(quintileSizeWide, "/scratch/upenn/hossaine/toCopy.csv")

################## OFF WRDS ####################################################
# Downloading betas and plotting histogram
# scp hossaine@wrds-cloud.wharton.upenn.edu:/scratch/upenn/hossaine/couponBetas.csv /home/mallick/Downloads
betas <- fread("/home/mallick/Downloads/couponBetas.csv")
betas[, "foodChar" := ifelse(food == 1, "Food", "Non-Food")]

# Getting estimated savings of moving from 2nd to 4th quintile
# scp hossaine@wrds-cloud.wharton.upenn.edu:/scratch/upenn/hossaine/toCopy.csv /home/mallick/Downloads
getCoefs <- function(i) {
  data <- fread(paste0("/home/mallick/Downloads/scannerBulkDiscountBetas", i, ".csv"))
  return(data)
}
coefs <- rbindlist(map(c("1a", "1b", "1c1501",
                         #"1c1503", "1c1505",
                         "1c1506", "1c1507", "1c1508", "2", "3", "4", "5", "6", "7", "7b"),
                       getCoefs), use.names = TRUE)
coefs[abs(`t value`) <= 3, "Estimate" := 0]
coefs <- coefs[reg == "Store-Week-Brand FE"]

savings <- fread("/home/mallick/Downloads/toCopy.csv")
savings <- merge(savings, coefs, by.x = "product_module_code", by.y = "mod")
savings[, "totalSavings" := 1 - exp(Estimate * logSizeRatio24)]
savings[, "foodChar" := ifelse(food == 1, "Food", "Non-Food")]

# Combining two datasets together and then using facet_grid
bulkSavings <- savings[, .(product_module_code, food, foodChar,
                           beta = totalSavings,
                           savings = "Bulk")]
couponSavings <- betas[, .(product_module_code, food, foodChar,
                           beta,
                           savings = "Coupon")]
graphData <- rbindlist(list(bulkSavings, couponSavings), use.names = TRUE)

# Combining bulk discounts and coupon savings
# Get coupon savings chart from couponSavings.R
ggplot(data = graphData,
       aes(x = beta * 100,
           y = stat(density),
           fill = foodChar)) +
  facet_grid(cols = vars(savings)) +
  geom_histogram(bins = 50, alpha = 0.65, position = "identity") +
  geom_hline(yintercept = 0) +
  scale_x_continuous(limits = c(-10, 100)) +
  #scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Savings (%)",
       y = "Density",
       fill = "Product Type") +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom") +
  scale_fill_grey()
  # scale_fill_fivethirtyeight()

ggsave(filename = "./code/5_figures/couponBulkSavings.pdf", height = 4, width = 6)
# ggsave(filename = "./code/5_figures/couponBulkSavingsColor.pdf", height = 4, width = 6)

