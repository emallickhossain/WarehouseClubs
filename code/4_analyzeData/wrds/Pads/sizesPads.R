# Gets package size purchase differences between rich and poor households
library(data.table)
library(purrr)
library(lfe)
library(stargazer)
path <- "/scratch/upenn/hossaine/"

tamponPurch <- fread(paste0(path, "tamponPurch.csv"))
tamponPurch[, c("upc", "upc_ver_uc", "trip_code_uc", "quantity", "product_module_code",
            "upc_descr", "product_module_descr", "product_group_code",
            "product_group_descr", "department_code", "department_descr",
            "size1_code_uc", "size1_units", "dataset_found_uc",
            "size1_change_flag_uc", "purchase_date") := NULL]
tamponPurch[, "share" := size / annualTampon]
tamponPurch[method_of_payment_cd %in% c(1, 2, 8), "payment" := "Cash"]
tamponPurch[method_of_payment_cd %in% 3:7, "payment" := "Credit"]

getReg <- function(stores, fileName, titleName) {
  regData <- tamponPurch[channel_type %in% stores]
  # Regression for package size (logs)
  reg1 <- felm(data = regData, log(size) ~ household_income |
                 panel_year + market + household_size + marital_status + race +
                 hispanic_origin + age + urban + college | 0 | market,
               weights = regData$projection_factor)
  reg2 <- felm(data = regData, log(size) ~ household_income |
                 panel_year + market + household_size + marital_status + race +
                 hispanic_origin + age + urban + college +
                 channel_type | 0 | market,
               weights = regData$projection_factor)
  reg3 <- felm(data = regData, log(size) ~ household_income |
                 panel_year + market + household_size + marital_status + race +
                 hispanic_origin + age + urban + college +
                 channel_type +
                 brand_code_uc | 0 | market,
               weights = regData$projection_factor)

  # Checking credit purchases for liquidity constraints
  reg4 <- felm(data = regData, log(size) ~ household_income * payment |
                 panel_year + market + household_size + marital_status + race +
                 hispanic_origin + age + urban + college | 0 | market,
               weights = regData$projection_factor)
  reg5 <- felm(data = regData, log(size) ~ household_income * payment |
                 panel_year + market + household_size + marital_status + race +
                 hispanic_origin + age + urban + college +
                 channel_type | 0 | market,
               weights = regData$projection_factor)
  reg6 <- felm(data = regData, log(size) ~ household_income * payment |
                 panel_year + market + household_size + marital_status + race +
                 hispanic_origin + age + urban + college +
                 channel_type +
                 brand_code_uc | 0 | market,
               weights = regData$projection_factor)

  # Making regression table
  stargazer(reg1, reg2, reg3, type = "text",
            add.lines = list(c("Year/MSA/Demog. FE", "Y", "Y", "Y"),
                             c("Channel FE", "N", "Y", "Y"),
                             c("Brand FE", "N", "N", "Y")),
            single.row = TRUE, no.space = TRUE, omit.stat = c("ser", "rsq"),
            out.header = FALSE,
            column.labels = c("Log(Size)"), column.separate = c(3),
            dep.var.caption = "", dep.var.labels.include = FALSE,
            covariate.labels = c(">100k", "50-100k", "25-50k"),
            notes.align = "l",
            notes = c("Standard errors are clustered at the market level."),
            order = c(1, 3, 2),
            digits = 2,
            label = paste0("tab:", fileName),
            title = titleName,
            out = paste0("tables/", fileName, ".tex"))

  stargazer(reg4, reg5, reg6, type = "text",
            add.lines = list(c("Year/MSA/Demog. FE", "Y", "Y", "Y"),
                             c("Channel FE", "N", "Y", "Y"),
                             c("Brand FE", "N", "N", "Y")),
            single.row = TRUE, no.space = TRUE, omit.stat = c("ser", "rsq"),
            out.header = FALSE,
            column.labels = c("Log(Size)"), column.separate = c(3),
            dep.var.caption = "", dep.var.labels.include = FALSE,
            covariate.labels = c(">100k", "50-100k", "25-50k", "Credit",
                                 "Credit:>100k", "Credit:50-100k", "Credit:25-50k"),
            notes.align = "l",
            notes = c("Standard errors are clustered at the market level."),
            order = c(1, 3, 2, 4, 5, 7, 6),
            digits = 2,
            label = paste0("tab:", fileName),
            title = titleName,
            out = paste0("tables/", fileName, "Liq.tex"))
}

getReg(unique(tamponPurch$channel_type), "packageSizeFullTampon", "Full Sample")
getReg("Dollar Store", "packageSizeDollarTampon", "Dollar Stores")
getReg("Discount Store", "packageSizeDiscountTampon", "Discount Stores")
getReg("Grocery", "packageSizeGroceryTampon", "Grocery Stores")
getReg("Warehouse Club", "packageSizeWarehouseTampon", "Warehouse Clubs")

# Getting channel FEs
regData <- tamponPurch[channel_type %in% unique(tamponPurch$channel_type)]
regData[, "channel_type" := relevel(as.factor(channel_type), ref = "Grocery")]
regFE <- felm(data = regData, log(size) ~ household_income + annualTampon + channel_type |
               panel_year + market + household_size + marital_status + race +
               hispanic_origin + age + urban + college +
               brand_code_uc | 0 | market,
             weights = regData$projection_factor)
stargazer(regFE, type = "text",
          add.lines = list(c("Year/MSA/Demog. FE", "Y"),
                           c("Channel FE", "Y"),
                           c("Brand FE", "Y")),
          keep = c("household_income*", "*Dollar", "*Discount", "*Warehouse"),
          single.row = TRUE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("Log(Size)"), column.separate = c(1),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          covariate.labels = c(">100k", "50-100k", "25-50k",
                               "Discount", "Dollar", "Warehouse"),
          notes.align = "l",
          notes = c("Standard errors are clustered at the market level.",
                    "Package size is of standardized 250, 2-ply rolls."),
          order = c(1, 3, 2, 4, 5, 6),
          digits = 2,
          label = "tab:tamponFEs",
          title = "Channel FEs",
          out = "tables/tamponFEs.tex")
