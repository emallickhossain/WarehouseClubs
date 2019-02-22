# Gets package size purchase differences between rich and poor households
library(data.table)
library(purrr)
library(lfe)
library(stargazer)
path <- "/scratch/upenn/hossaine/"

# TP
tpPurch <- fread(paste0(path, "tpPurch.csv"))
tpPurch[, c("quantity", "size1_units", "purchase_date") := NULL]

reg1 <- felm(data = tpPurch, log(size) ~ household_income |
               panel_year + market + household_size + marital_status + race +
               hispanic_origin + age + urban + college +
               channel_type +
               brand_code_uc | 0 | market,
             weights = tpPurch$projection_factor)

# Eggs
eggsPurch <- fread(paste0(path, "eggsPurch.csv"))
eggsPurch[, c("quantity", "size1_units", "purchase_date") := NULL]

reg2 <- felm(data = eggsPurch, log(size) ~ household_income |
               panel_year + market + household_size + marital_status + race +
               hispanic_origin + age + urban + college +
               channel_type +
               brand_code_uc | 0 | market,
             weights = eggsPurch$projection_factor)

# Milk
milkPurch <- fread(paste0(path, "milkPurch.csv"))
milkPurch[, c("quantity", "size1_units", "purchase_date") := NULL]

reg3 <- felm(data = milkPurch, log(size) ~ household_income |
               panel_year + market + household_size + marital_status + race +
               hispanic_origin + age + urban + college +
               channel_type +
               brand_code_uc | 0 | market,
             weights = milkPurch$projection_factor)

# Making regression table
stargazer(reg1, reg2, reg3, type = "text",
          add.lines = list(c("Year/MSA/Demog. FE", "Y", "Y", "Y"),
                           c("Channel FE", "Y", "Y", "Y"),
                           c("Brand FE", "Y", "Y", "Y")),
          single.row = TRUE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("TP", "Milk", "Eggs"), column.separate = c(1, 1, 1),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          covariate.labels = c(">100k", "50-100k", "25-50k"),
          notes.align = "l",
          notes = c("Standard errors are clustered at the market level."),
          order = c(1, 3, 2),
          digits = 2,
          label = "tab:packageSizeFullPerishable",
          title = "Perishables",
          out = "tables/packageSizeFullPerishable.tex")
