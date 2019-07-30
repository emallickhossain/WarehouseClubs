# Looks at size distributions available by brand and module
library(data.table)
library(ggplot2)
library(ggthemes)
library(ggridges)
prod <- fread("/scratch/upenn/hossaine/fullProd.csv")
prod[, "brand_code_uc" := as.factor(brand_code_uc)]
prod[, "quintile" := factor(quintile, 1:5, ordered = TRUE)]

# Removing top and bottom 1% of sizes since these often are outliers or are miscoded
prod[, c("top1", "bottom1") := .(quantile(totalAmount, 0.99), quantile(totalAmount, 0.01)),
     by = product_module_code]
prod <- prod[totalAmount < top1 & totalAmount > bottom1]

# Getting number of products in each quintile by brand and module
ggplot(prod[product_module_code %in% 1000:1100],
       aes(x = reorder(as.factor(brand_descr), totalAmount), y = totalAmount)) +
  geom_point(size = 0.3) +
  facet_wrap(vars(product_module_code), scales = "free") +
  theme_fivethirtyeight() +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
