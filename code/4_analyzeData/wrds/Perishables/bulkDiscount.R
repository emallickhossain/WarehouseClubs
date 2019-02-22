# Gets bulk discount magnitude
library(data.table)
library(purrr)
library(stringr)
library(lfe)
library(stargazer)
library(ggplot2)
path <- "/scratch/upenn/hossaine/"

# TP
tpPurch <- fread(paste0(path, "tpPurch.csv"))
tpPurch[, c("quantity", "size1_units", "purchase_date") := NULL]
reg1 <- felm(data = tpPurch, log(total_price_paid_real) ~ log(size) |
               as.factor(panel_year) + market +
               as.factor(brand_code_uc) +
               as.factor(retailer_code)| 0 | market)

# Milk
milkPurch <- fread(paste0(path, "milkPurch.csv"))
milkPurch[, c("quantity", "size1_units", "purchase_date") := NULL]
reg2 <- felm(data = milkPurch, log(total_price_paid_real) ~ log(size) |
               as.factor(panel_year) + market +
               as.factor(brand_code_uc) +
               as.factor(retailer_code)| 0 | market)

# Smoothed log prices (Full Data)
ggplot(milkPurch, aes(size, total_price_paid_real)) +
  geom_point(size = 1, color = "gray") +
  geom_smooth(se = FALSE) +
  geom_smooth(method = "lm") +
  ggtitle("Milk Prices") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Size") +
  ylab("Real Price")
ggsave("milkRealLog.png")

# Eggs
eggsPurch <- fread(paste0(path, "eggsPurch.csv"))
eggsPurch[, c("quantity", "size1_units", "purchase_date") := NULL]
reg3 <- felm(data = eggsPurch, log(total_price_paid_real) ~ log(size) |
               as.factor(panel_year) + market +
               as.factor(brand_code_uc) +
               as.factor(retailer_code)| 0 | market)

# Smoothed log prices (Full Data)
ggplot(eggsPurch, aes(log(size), log(total_price_paid_real))) +
  geom_point(size = 1, color = "gray") +
  geom_smooth(se = FALSE) +
  geom_smooth(method = "lm") +
  ggtitle("Egg Prices") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(0, 5) + xlab("Log Size") +
  ylim(0, 5) + ylab("Log Real Price")
ggsave("eggRealLog.png")


# Making regression table
stargazer(reg1, reg2, reg3, type = "text",
          add.lines = list(c("Year/MSA FE", "Y", "Y", "Y"),
                           c("Brand FE", "Y", "Y", "Y"),
                           c("Retailer FE", "Y", "Y", "Y")),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("TP", "Milk", "Eggs"),
          column.separate = c(1, 1, 1),
          dep.var.caption = "Log(Price)", dep.var.labels.include = FALSE,
          covariate.labels = c("Log(Size)"),
          notes.align = "l",
          notes = c("Standard errors are clustered at the market level."),
          digits = 2,
          label = "tab:bulkDiscountPerishable",
          title = "Bulk Discount Estimation (Perishable)",
          out = "tables/bulkDiscountPerishables.tex")
