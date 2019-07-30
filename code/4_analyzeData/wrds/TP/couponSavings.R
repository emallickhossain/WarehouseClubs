# Computing coupon savings from Nielsen
library(data.table)
library(ggplot2)
library(ggthemes)
threads <- 8

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

# Downloading betas and plotting histogram
# scp hossaine@wrds-cloud.wharton.upenn.edu:/scratch/upenn/hossaine/couponBetas.csv /home/mallick/Downloads
betas <- fread("/home/mallick/Downloads/couponBetas.csv")
betas[, "foodChar" := ifelse(food == 1, "Food", "Non-Food")]
ggplot(data = betas, aes(x = beta * 100, fill = foodChar)) +
  geom_histogram(aes(y = ..density..), bins = 50, alpha = 0.5, position = "identity") +
  geom_density(alpha = 0.8) +
  scale_x_continuous(limits = c(0, 100)) +
  labs(title = "Coupon Savings Are Substantial",
       subtitle = "Discounts are Larger for Food Items",
       x = "Percent Savings",
       y = "Density",
       fill = "Product Type",
       caption = paste0("Source: Author calculations from Nielsen Consumer Panel. \n",
                        "Note: Histogram plots unweighted average discount of \n",
                        "all coupons redeemed within a product category.")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0)) +
  scale_fill_grey()
ggsave(filename = "./code/5_figures/couponSavings.png", height = 6, width = 6)
