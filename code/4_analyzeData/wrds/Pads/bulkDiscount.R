# Gets bulk discount magnitude
library(data.table)
library(purrr)
library(stringr)
library(lfe)
library(stargazer)
path <- "/scratch/upenn/hossaine/"

padsPurch <- fread(paste0(path, "padsPurch.csv"))
padsPurch[, c("upc", "upc_ver_uc", "trip_code_uc", "quantity", "product_module_code",
            "upc_descr", "product_module_descr", "product_group_code",
            "product_group_descr", "department_code", "department_descr",
            "size1_code_uc", "size1_units", "dataset_found_uc",
            "size1_change_flag_uc", "purchase_date") := NULL]

# Bulk discount (logs)
reg1 <- felm(data = padsPurch, log(total_price_paid_real) ~ log(size) |
               as.factor(panel_year) + market | 0 | market)
reg2 <- felm(data = padsPurch, log(total_price_paid_real) ~ log(size) |
               as.factor(panel_year) + market +
               as.factor(brand_code_uc) | 0 | market)
reg3 <- felm(data = padsPurch, log(total_price_paid_real) ~ log(size) |
               as.factor(panel_year) + market +
               as.factor(brand_code_uc) +
               as.factor(retailer_code)| 0 | market)
reg4 <- felm(data = padsPurch[channel_type == "Discount Store"], log(total_price_paid_real) ~ log(size) |
               as.factor(panel_year) + market +
               as.factor(brand_code_uc) +
               as.factor(retailer_code)| 0 | market)
reg5 <- felm(data = padsPurch[channel_type == "Grocery"], log(total_price_paid_real) ~ log(size) |
               as.factor(panel_year) + market +
               as.factor(brand_code_uc) +
               as.factor(retailer_code)| 0 | market)
reg6 <- felm(data = padsPurch[channel_type == "Dollar Store"], log(total_price_paid_real) ~ log(size) |
               as.factor(panel_year) + market +
               as.factor(brand_code_uc) +
               as.factor(retailer_code)| 0 | market)
reg7 <- felm(data = padsPurch[channel_type == "Warehouse Club"], log(total_price_paid_real) ~ log(size) |
               as.factor(panel_year) + market +
               as.factor(brand_code_uc) +
               as.factor(retailer_code)| 0 | market)

# Making regression table
stargazer(reg1, reg2, reg3, reg4, reg5, reg6, reg7, type = "text",
          add.lines = list(c("Year/MSA FE", "Y", "Y", "Y", "Y", "Y", "Y", "Y"),
                           c("Brand FE", "N", "Y", "Y", "Y", "Y", "Y", "Y"),
                           c("Retailer FE", "N", "N", "Y", "Y", "Y", "Y", "Y")),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("Full Sample", "Discount", "Grocery", "Dollar", "Warehouse"),
          column.separate = c(3, 1, 1, 1, 1),
          dep.var.caption = "Log(Price)", dep.var.labels.include = FALSE,
          covariate.labels = c("Log(Size)"),
          notes.align = "l",
          notes = c("Standard errors are clustered at the market level."),
          digits = 2,
          label = "tab:bulkDiscount",
          title = "Bulk Discount Estimation",
          out = "tables/bulkDiscountPads.tex")
