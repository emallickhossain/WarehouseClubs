# Look at pricing for TP from scanner data
library(data.table)
library(ggplot2)
library(ggthemes)
library(lfe)
library(stargazer)
threads <- 8
saleCutoff <- 0.05

# Getting products data
prod <- fread("/scratch/upenn/hossaine/prodTP.csv", nThread = threads,
              key = c("upc", "upc_ver_uc"))
tp <- fread("/scratch/upenn/hossaine/fullTPAssortment.csv", nThread = threads,
            key = c("upc", "upc_ver_uc"))
tp <- merge(tp, prod, by = c("upc", "upc_ver_uc"))
tp[, "week_end" := as.Date(as.character(week_end), "%Y%m%d")]
tp[, "panel_year" := year(week_end)]

# Looking at promotions
# Products are rarely ever on a multi-buy promotion (< 0.01% happen)
round(prop.table(table(tp$prmult)), 4)

# Identifying sales
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

setorder(tp, panel_year, store_code_uc, upc, upc_ver_uc, -price)
tp[, "modePrice" := Mode(price), by = .(upc, upc_ver_uc, store_code_uc, panel_year)]
tp[, "priceDiff" := (modePrice - price) / modePrice]
tp[, "sale" := (priceDiff > saleCutoff)]

# Computing share of weeks on sale for each product
saleShare <- tp[, .(saleShare = sum(sale) / .N),
                by = .(upc, upc_ver_uc, store_code_uc, brand_descr, rolls)]
saleShare[, .(mean(saleShare), sd(saleShare)), keyby = .(brand_descr, rolls)]

reg <- felm(saleShare ~ as.factor(rolls) | brand_descr + store_code_uc, data = saleShare)
summary(reg)

ggplot(data = saleShare[rolls < 30], aes(x = saleShare, color = as.factor(rolls))) +
  geom_density() +
  facet_wrap(vars(brand_descr))

# Computing bulk discounts overall, during sale weeks, and during sale weeks for big sizes
tp[, "lPrice" := log(price / sheets)]
tp[, "lQ" := log(sheets)]
tp[, "brandStoreWeek" := paste(brand_code_uc, store_code_uc, week_end, sep = "_")]
tp[, "Nsale" := sum(sale), by = .(store_code_uc, week_end)]
tp[, "saleWeek" := (Nsale > 0)]
tp[, "bigSale" := (sale == TRUE & rolls > 12)]
tp[, "NbigSale" := sum(bigSale), by = .(store_code_uc, week_end)]
tp[, "bigSaleWeek" := (NbigSale > 0)]
tp[, "smallSale" := (sale == TRUE & rolls <= 12)]
tp[, "NsmallSale" := sum(smallSale), by = .(store_code_uc, week_end)]
tp[, "smallSaleWeek" := (NsmallSale > 0)]
tp[, c("")]

reg1 <- felm(lPrice ~ lQ | brandStoreWeek, data = tp)
reg2 <- felm(lPrice ~ lQ | brandStoreWeek, data = tp[saleWeek == 1])
reg3 <- felm(lPrice ~ lQ | brandStoreWeek, data = tp[bigSaleWeek == 1])
reg4 <- felm(lPrice ~ lQ | brandStoreWeek, data = tp[smallSaleWeek == 1])

stargazer(reg1, reg2, reg3, reg4, type = "text",
          add.lines = list(c("Brand-Store-Week FE", "Y", "Y", "Y", "Y", "Y")),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("All Weeks", "Sale Weeks", "Big Package Sales",
                            "Small Package Sales"),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          notes.align = "l",
          notes = c("Table shows estimates of log unit prices regressed on log package quantities",
                    "A sale is defined as a week with a price more than 5 percent below the modal price",
                    "for a specific UPC in a given store-year."),
          digits = 3,
          label = "tab:bulkDiscountSales",
          title = "Bulk Discounting and Sales",
          out = "tables/bulkDiscountSales.tex")
