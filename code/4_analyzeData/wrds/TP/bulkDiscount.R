# Gets bulk discount magnitude
library(data.table)
library(purrr)
library(stringr)
library(lfe)
library(stargazer)
library(ggplot2)
library(ggridges)
library(ggthemes)
source("./Nielsen/runReg.R")
path <- "/scratch/upenn/hossaine/"

tpPurch <- fread(paste0(path, "7260Purch.csv"))
tpPurch[, c("upc", "upc_ver_uc", "trip_code_uc", "quantity", "product_module_code",
            "upc_descr", "size1_units") := NULL]
tpPurch[, "deal_type" := relevel(as.factor(deal_type), ref = "No Deal")]
tpPurch[, "purchWeek" := week(purchase_date)]

# Bulk discount (logs) for unit cost
y <- "log(unitCost)"
x <- "log(size)"
controls <- "panel_year + month + purchWeek + market"
cluster <- "market"

reg1 <- runReg(x, y, tpPurch, controls, cluster)
reg2 <- runReg(x, y, tpPurch, paste0(controls, "+brand_code_uc"), cluster)
reg3 <- runReg(x, y, tpPurch, paste0(controls, "+retailer_code+brand_code_uc"), cluster)
reg4 <- runReg(x, y, tpPurch[channel_type == "Discount Store"],
               paste0(controls, "+retailer_code+brand_code_uc"), cluster)
reg5 <- runReg(x, y, tpPurch[channel_type == "Grocery"],
               paste0(controls, "+retailer_code+brand_code_uc"), cluster)
reg6 <- runReg(x, y, tpPurch[channel_type == "Dollar Store"],
               paste0(controls, "+retailer_code+brand_code_uc"), cluster)
reg7 <- runReg(x, y, tpPurch[channel_type == "Warehouse Club"],
               paste0(controls, "+retailer_code+brand_code_uc"), cluster)

# Making regression table
stargazer(reg1, reg2, reg3, type = "text",
          add.lines = list(c("Year/MSA FE", "Y", "Y", "Y"),
                           c("Brand FE", "N", "Y", "Y"),
                           c("Retailer FE", "N", "N", "Y")),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("Full Sample"),
          column.separate = c(3),
          dep.var.caption = "Log(Unit Cost)", dep.var.labels.include = FALSE,
          covariate.labels = c("Log(Size)"),
          notes.align = "l",
          notes = c("Standard errors are clustered at",
                    "the market level."),
          digits = 2,
          label = "tab:bulkDiscountUnitCost",
          title = "Bulk Discounts Generate Substantial Savings",
          out = "tables/bulkDiscountUnitCost.tex")

# Adding in sales/deals
y <- "log(unitCost)"
x <- "log(size) + deal_type"
controls <- paste0("panel_year + month + purchWeek + market")
cluster <- "market"

reg1 <- runReg(x, y, tpPurch, controls, cluster)
reg2 <- runReg(x, y, tpPurch, paste0(controls, "+brand_code_uc"), cluster)
reg3 <- runReg(x, y, tpPurch, paste0(controls, "+retailer_code+brand_code_uc"), cluster)
reg4 <- runReg(x, y, tpPurch[channel_type == "Discount Store"],
               paste0(controls, "+retailer_code+brand_code_uc"), cluster)
reg5 <- runReg(x, y, tpPurch[channel_type == "Grocery"],
               paste0(controls, "+retailer_code+brand_code_uc"), cluster)
reg6 <- runReg(x, y, tpPurch[channel_type == "Dollar Store"],
               paste0(controls, "+retailer_code+brand_code_uc"), cluster)
reg7 <- runReg(x, y, tpPurch[channel_type == "Warehouse Club"],
               paste0(controls, "+retailer_code+brand_code_uc"), cluster)

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
          notes = c("Standard errors are clustered at",
                    "the market level."),
          digits = 2,
          label = "tab:bulkDiscountSale",
          title = "Bulk Discounts Provide More Savings Than Sales",
          out = "tables/bulkDiscountSale.tex")
prop.table(table(tpPurch$deal_type))

# Ridge plot for top brands
topBrands <- tpPurch[, .(sales = sum(total_price_paid_real)),
                     by = .(brand_code_uc, brand_descr, household_income_coarse)]
setorder(topBrands, -sales)
brands <- topBrands[, .SD[1:10], by = .(household_income_coarse)]
tp <- tpPurch[brand_code_uc %in% brands$brand_code_uc]
tp[brand_code_uc == 5367469101, "brand_descr" := "WAREHOUSE LABEL 1"]
tp[brand_code_uc == 5367469103, "brand_descr" := "WAREHOUSE LABEL 2"]
tp[brand_code_uc == 5367466920, "brand_descr" := "DISCOUNT LABEL"]

ggplot(tp, aes(y = as.character(brand_descr), x = unitCost)) +
  geom_density_ridges(rel_min_height = 0.01) +
  theme_fivethirtyeight() +
  labs(x = "Unit Cost",
       y = "Brand",
       title = "Unit Cost Distribution by Brand",
       caption = "Source: Nielsen Consumer Panel")
ggsave("./figures/tpUnitCostBrand.png")

ggplot(tp, aes(y = size, x = unitCost, group = size)) +
  geom_density_ridges(rel_min_height = 0.02) +
  theme_fivethirtyeight() +
  labs(x = "Unit Cost",
       y = "Package Size",
       title = "Unit Cost Distribution by Package Size",
       caption = "Source: Nielsen Consumer Panel")
ggsave("./figures/tpUnitCostSize.png")

# Redoing bulk discounts for scanner data
scanner <- NULL
for (i in 2006:2016) {
  print(i)
  # Getting store assortment
  assort <- na.omit(fread(paste0(path, "Assortment/", i, ".csv"), nThread = 16),
                    cols = "unitCost")
  scanner <- rbind(scanner, assort)
}
scanner[, c("lcost", "lsize") := .(log(unitCost), log(size))]
scanner[, c("upc", "upc_ver_uc", "pkgSize", "ply", "pCents") := NULL]
rm(assort)

# Bulk discounts without any fixed effects
reg1 <- felm(lcost ~ lsize, data = scanner, keepX = FALSE, keepCX = FALSE, keepModel = FALSE)
stargazer(reg1, type = "text", single.row = FALSE, no.space = TRUE,
          omit.stat = c("ser", "rsq"), out.header = FALSE,
          dep.var.caption = "Log(Price)", dep.var.labels.include = FALSE,
          covariate.labels = "Log(Size)",
          notes.align = "l",
          digits = 2,
          label = "tab:bulkDiscountScanner1",
          title = "Bulk Discounts Are Common Across Retailers",
          out = "tables/bulkDiscountScanner1.tex")
rm(reg1)

reg2 <- felm(lcost ~ lsize | brand_code_uc, data = scanner, keepX = FALSE,
             keepCX = FALSE, keepModel = FALSE)
stargazer(reg2, type = "text", single.row = FALSE, no.space = TRUE,
          omit.stat = c("ser", "rsq"), out.header = FALSE,
          dep.var.caption = "Log(Price)", dep.var.labels.include = FALSE,
          covariate.labels = "Log(Size)",
          notes.align = "l",
          digits = 2,
          label = "tab:bulkDiscountScanner2",
          title = "Bulk Discounts Are Common Across Retailers",
          out = "tables/bulkDiscountScanner2.tex")
rm(reg2)

reg3 <- felm(lcost ~ lsize | brand_code_uc + store_code_uc, data = scanner,
             keepX = FALSE, keepCX = FALSE, keepModel = FALSE)
stargazer(reg3, type = "text", single.row = FALSE, no.space = TRUE,
          omit.stat = c("ser", "rsq"), out.header = FALSE,
          dep.var.caption = "Log(Price)", dep.var.labels.include = FALSE,
          covariate.labels = "Log(Size)",
          notes.align = "l",
          digits = 2,
          label = "tab:bulkDiscountScanner3",
          title = "Bulk Discounts Are Common Across Retailers",
          out = "tables/bulkDiscountScanner3.tex")
rm(reg3)

reg4 <- felm(lcost ~ lsize | week_end + brand_code_uc + store_code_uc, data = scanner,
             keepX = FALSE, keepCX = FALSE, keepModel = FALSE)
stargazer(reg4, type = "text", single.row = FALSE, no.space = TRUE,
          omit.stat = c("ser", "rsq"), out.header = FALSE,
          dep.var.caption = "Log(Price)", dep.var.labels.include = FALSE,
          covariate.labels = "Log(Size)",
          notes.align = "l",
          digits = 2,
          label = "tab:bulkDiscountScanner4",
          title = "Bulk Discounts Are Common Across Retailers",
          out = "tables/bulkDiscountScanner4.tex")
