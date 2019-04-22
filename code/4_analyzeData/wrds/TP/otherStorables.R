# Runs quantity purchase regression for other storables
source("./Nielsen/getItem.R")
source("./Nielsen/runReg.R")
library(knitr)
library(stargazer)
library(stringr)
library(lfe)

y <- "log(size)"
x <- "household_income_coarse"
controls <- paste0("panel_year + month + market + household_size + ",
                   "married + white + hispanic_origin + age + urban + college")
cluster <- "market"
prod <- fread(paste0(path, "prod.csv"),
              select = c("upc", "upc_ver_uc", "upc_descr", "product_module_code",
                         "brand_code_uc", "brand_descr", "multi", "size1_amount",
                         "size1_units"))
fullProd <- getItem(prod)
fullProd[, "size" := multi * size1_amount * quantity]
fullProd[, "purchase_date" := as.Date(purchase_date, format = "%Y-%m-%d")]
fullProd[, "month" := month(purchase_date)]
fullProd[, "day" := mday(purchase_date)]
fullProd[, "brandRetailer" := paste0(brand_descr, retailer_code)]
fullProd[, "unitCost" := total_price_paid / size]

# Computing size difference by income group
getReg <- function(prodCode) {
  fullData <- fullProd[product_module_code %in% prodCode]
  weights <- fullData$projection_factor

  reg1 <- runReg(x, y, fullData, controls, cluster, weights)
  reg2 <- runReg(x, y, fullData, paste0(controls, "+brand_code_uc"), cluster, weights)
  reg3 <- runReg(x, y, fullData, paste0(controls, "+brand_code_uc+retailer_code"), cluster, weights)
  reg4 <- runReg(x, y, fullData, paste0(controls, "+brandRetailer"), cluster, weights)
  stargazer(reg1, reg2, reg3, reg4, type = "text",
            add.lines = list(c("Time/MSA/Demog. FE", "Y", "Y", "Y", "Y"),
                             c("Brand FE", "N", "Y", "Y", "N"),
                             c("Retailer FE", "N", "N", "Y", "N"),
                             c("Retailer x Brand FE", "N", "N", "N", "Y")),
            single.row = TRUE, no.space = TRUE, omit.stat = c("ser", "rsq"),
            out.header = FALSE,
            omit = "rate",
            column.labels = c("Log(Size)"),
            dep.var.caption = "", dep.var.labels.include = FALSE,
            order = c(2, 3, 1),
            covariate.labels = c("25-50k", "50-100k", ">100k"),
            notes.align = "l",
            notes = c("Standard errors are clustered at the market level. Fixed ",
                      "effects include indicators for year, month, week, MSA, ",
                      "retail chain, and brand. Demographics include household ",
                      "size, marital status, race, ethnicity, age group, ",
                      "urban/rural indicator, and education."),
            digits = 2,
            label = paste0("tab:packageSizeFull", prodCode),
            out = paste0("./tables/packageSizeFull", prodCode, ".tex"))
}

getReg(7270) # Tampons
getReg(7265) # Sanitary Napkins
getReg(7870) # Batteries
getReg(c(7734, 7255, 7256)) # Paper Towels
getReg(8444) # Diapers
getReg(7245) # Kleenex
getReg(7020) # Dishwasher Detergent
getReg(c(7003, 7008, 7012)) # Detergent
getReg(3625) # Milk
getReg(4100) # Eggs


# Computing Bulk discount for each item
y <- "log(unitCost)"
x <- "log(size)"
controls <- "panel_year + month + market"
cluster <- "market"

getBulk <- function(prodCode) {
  fullData <- fullProd[product_module_code %in% prodCode & total_price_paid > 0]
  print(c(unique(fullData$upc_descr)[1], unique(fullData$brand_descr)[1]))
  weights <- fullData$projection_factor
  reg1 <- runReg(x, y, fullData, controls, cluster)
  reg2 <- runReg(x, y, fullData, paste0(controls, "+brand_code_uc"), cluster)
  reg3 <- runReg(x, y, fullData, paste0(controls, "+retailer_code+brand_code_uc"), cluster)

  # Making regression table
  stargazer(reg1, reg2, reg3, type = "text",
            add.lines = list(c("Year/MSA FE", "Y", "Y", "Y"),
                             c("Brand FE", "N", "Y", "Y"),
                             c("Retailer FE", "N", "N", "Y")),
            single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
            out.header = FALSE,
            dep.var.caption = "Log(Unit Cost)", dep.var.labels.include = FALSE,
            covariate.labels = c("Log(Size)"),
            notes.align = "l",
            notes = c("Standard errors are clustered at market level."),
            digits = 2,
            label = paste0("tab:bulkDiscountUnitCost", prodCode),
            out = paste0("tables/bulkDiscountUnitCost", prodCode, ".tex"))
}

getBulk(7270) # Tampons
getBulk(7265) # Sanitary Napkins
getBulk(7870) # Batteries
getBulk(c(7734, 7255, 7256)) # Paper Towels
getBulk(8444) # Diapers
getBulk(7245) # Kleenex
getBulk(7020) # Dishwasher Detergent
getBulk(c(7003, 7008, 7012)) # Detergent
getBulk(3625) # Milk
getBulk(4100) # Eggs

