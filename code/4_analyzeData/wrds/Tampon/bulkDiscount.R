# Gets bulk discount magnitude
library(data.table)
library(purrr)
library(stringr)
library(lfe)
library(stargazer)
library(ggplot2)
library(ggthemes)
library(ggridges)
path <- "/scratch/upenn/hossaine/"

tamponPurch <- fread(paste0(path, "tamponPurch.csv"))
tamponPurch[, c("upc", "upc_ver_uc", "trip_code_uc", "quantity", "product_module_code",
            "upc_descr", "product_module_descr", "product_group_code",
            "product_group_descr", "department_code", "department_descr",
            "size1_code_uc", "size1_units", "dataset_found_uc",
            "size1_change_flag_uc", "purchase_date") := NULL]
tamponPurch[, "deal_type" := relevel(as.factor(deal_type), ref = "No Deal")]

# Bulk discount (logs)
reg1 <- felm(data = tamponPurch, log(unitCost) ~ log(size) |
               as.factor(panel_year) + market | 0 | market)
reg2 <- felm(data = tamponPurch, log(unitCost) ~ log(size) |
               as.factor(panel_year) + market +
               as.factor(brand_code_uc) | 0 | market)
reg3 <- felm(data = tamponPurch, log(unitCost) ~ log(size) |
               as.factor(panel_year) + market +
               as.factor(brand_code_uc) +
               as.factor(retailer_code)| 0 | market)
reg4 <- felm(data = tamponPurch[channel_type == "Discount Store"], log(unitCost) ~ log(size) |
               as.factor(panel_year) + market +
               as.factor(brand_code_uc) +
               as.factor(retailer_code)| 0 | market)
reg5 <- felm(data = tamponPurch[channel_type == "Grocery"], log(unitCost) ~ log(size) |
               as.factor(panel_year) + market +
               as.factor(brand_code_uc) +
               as.factor(retailer_code)| 0 | market)
reg6 <- felm(data = tamponPurch[channel_type == "Dollar Store"], log(unitCost) ~ log(size) |
               as.factor(panel_year) + market +
               as.factor(brand_code_uc) +
               as.factor(retailer_code)| 0 | market)
reg7 <- felm(data = tamponPurch[channel_type == "Warehouse Club"], log(unitCost) ~ log(size) |
               as.factor(panel_year) + market +
               as.factor(brand_code_uc) +
               as.factor(retailer_code)| 0 | market)

# Making regression table
stargazer(reg1, reg2, reg3, type = "text",
          add.lines = list(c("Time/MSA FE", "Y", "Y", "Y"),
                           c("Brand FE", "N", "Y", "Y"),
                           c("Retailer FE", "N", "N", "Y")),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("Full Sample"),
          column.separate = c(3),
          dep.var.caption = "Log(Price)", dep.var.labels.include = FALSE,
          covariate.labels = c("Log(Size)"),
          notes.align = "l",
          notes = c("Standard errors are clustered ",
                    "at the market level."),
          digits = 2,
          label = "tab:bulkDiscount",
          title = "Bulk Discount Estimation",
          out = "tables/bulkDiscountTampon.tex")

# Adding in sales/deals
reg1 <- felm(data = tamponPurch, log(unitCost) ~ log(size) + deal_type |
               as.factor(panel_year) + market | 0 | market)
reg2 <- felm(data = tamponPurch, log(unitCost) ~ log(size) + deal_type |
               as.factor(panel_year) + market +
               as.factor(brand_code_uc) | 0 | market)
reg3 <- felm(data = tamponPurch, log(unitCost) ~ log(size) + deal_type |
               as.factor(panel_year) + market +
               as.factor(brand_code_uc) +
               as.factor(retailer_code)| 0 | market)
reg4 <- felm(data = tamponPurch[channel_type == "Discount Store"], log(unitCost) ~ log(size) + deal_type |
               as.factor(panel_year) + market +
               as.factor(brand_code_uc) +
               as.factor(retailer_code)| 0 | market)
reg5 <- felm(data = tamponPurch[channel_type == "Grocery"], log(unitCost) ~ log(size) + deal_type |
               as.factor(panel_year) + market +
               as.factor(brand_code_uc) +
               as.factor(retailer_code)| 0 | market)
reg6 <- felm(data = tamponPurch[channel_type == "Dollar Store"], log(unitCost) ~ log(size) + deal_type |
               as.factor(panel_year) + market +
               as.factor(brand_code_uc) +
               as.factor(retailer_code)| 0 | market)
reg7 <- felm(data = tamponPurch[channel_type == "Warehouse Club"], log(unitCost) ~ log(size) + deal_type |
               as.factor(panel_year) + market +
               as.factor(brand_code_uc) +
               as.factor(retailer_code)| 0 | market)

# Making regression table
stargazer(reg1, reg2, reg3, type = "text",
          add.lines = list(c("Time/MSA FE", "Y", "Y", "Y"),
                           c("Brand FE", "N", "Y", "Y"),
                           c("Retailer FE", "N", "N", "Y")),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("Full Sample"),
          column.separate = c(3),
          dep.var.caption = "Log(Price)", dep.var.labels.include = FALSE,
          covariate.labels = c("Log(Size)", "Sale and/or Coupon", "Sale Only"),
          notes.align = "l",
          notes = c("Standard errors are clustered ",
                    "at the market level."),
          digits = 2,
          label = "tab:bulkDiscountSale",
          title = "Bulk Discount Estimation",
          out = "tables/bulkDiscountSaleTampon.tex")
prop.table(table(tamponPurch$deal_type))

# Unit cost distributions
# Getting top brands
topBrands <- tamponPurch[, .(sales = sum(total_price_paid_real)),
                         by = .(brand_code_uc, brand_descr, household_income_coarse)]
setorder(topBrands, -sales)
brands <- topBrands[, .SD[1:10], by = .(household_income_coarse)]
tampon <- tamponPurch[brand_code_uc %in% brands$brand_code_uc]
tampon[brand_code_uc == 5367466905, "brand_descr" := "DISCOUNT LABEL 2"]
tampon[brand_code_uc == 5367466920, "brand_descr" := "DISCOUNT LABEL 1"]

ggplot(tampon, aes(y = as.character(brand_descr), x = unitCost)) +
  geom_density_ridges(rel_min_height = 0.01) +
  theme_fivethirtyeight() +
  xlim(0, 0.5) +
  labs(x = "Unit Cost",
       y = "Brand",
       title = "Unit Cost Distribution by Brand",
       caption = "Source: Nielsen Consumer Panel")
ggsave("./figures/tamponUnitCostBrand.png")

ggplot(tampon, aes(y = size, x = unitCost, group = size)) +
  geom_density_ridges(rel_min_height = 0.01) +
  theme_fivethirtyeight() +
  xlim(0, 0.5) +
  labs(x = "Unit Cost",
       y = "Package Size",
       title = "Unit Cost Distribution by Package Size",
       caption = "Source: Nielsen Consumer Panel")
ggsave("./figures/tamponUnitCostSize.png")
