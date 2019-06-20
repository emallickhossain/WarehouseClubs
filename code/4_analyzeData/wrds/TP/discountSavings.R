# Computes savings from different kinds of discounting behavior
# Fact 1: Bulk savings are larger for storable items than for perishable items
# Fact 2: Bulk savings are larger than savings offered by coupons
# Fact 3: Coupons offer about 30% savings compared to 40-80% for bulk savings on
# storable items and 16-50% on perishable items.
library(data.table)
library(lfe)
library(ggplot2)
library(ggthemes)
library(stargazer)
library(purrr)
threads <- 8
critValue <- 2

# Getting trips and panel data for demographics and to deflate expenditures
trips <- fread("/scratch/upenn/hossaine/fullTrips.csv", nThread = threads,
               select = c("trip_code_uc", "purchase_date", "retailer_code"))
trips[, c("yearMonth", "purchase_date") := .(substr(purchase_date, 1, 7), NULL)]

# Getting CPI to deflate values to Jan-2010 base
cpi <- fread("/scratch/upenn/hossaine/cpi.csv")
cpi[, "base" := cpi[date == "2010-01-01"]$value]
cpi[, "newIndex" := value / base * 100]
cpi[, c("base", "value") := NULL]
cpi[, "yearMonth" := .(substr(date, 1, 7))]
cpi[, "date" := NULL]

# Combining with CPI for price deflation
setkey(trips, yearMonth)
setkey(cpi, yearMonth)
fullData <- merge(trips, cpi, by = "yearMonth")[, "yearMonth" := NULL]

# Housekeeping
rm(trips, cpi)
setkey(fullData, trip_code_uc)

getSavings <- function(yr) {
  print(yr)
  purch <- fread(paste0("/scratch/upenn/hossaine/fullPurch", yr, ".csv"),
                 nThread = threads,
                 select = c("trip_code_uc", "coupon_value",
                            "product_module_code", "brand_code_uc", "storable",
                            "packagePrice", "totalAmount", "quintile", "quantity"),
                 key = "trip_code_uc")
  purch[, ':=' (coupon = as.integer(coupon_value > 0),
                generic = as.integer(brand_code_uc == 536746),
                totalExpenditure = (packagePrice - coupon_value) * quantity,
                unitPrice = (packagePrice - coupon_value) / totalAmount)]
  purch[, "quintile" := relevel(factor(quintile), ref = "2")]
  purch[, c("coupon_value", "packagePrice", "totalAmount", "quantity") := NULL]

  # Deflating expendtures
  setkey(purch, trip_code_uc)
  regData <- merge(purch, fullData, by = "trip_code_uc")[, "trip_code_uc" := NULL]
  regData <- regData[unitPrice > 0]
  regData[, "realExp" := totalExpenditure / newIndex * 100]
  regData[, "realUnitPrice" := unitPrice / newIndex * 100]
  regData[, c("totalExpenditure", "unitPrice", "newIndex") := NULL]
  regData[, "logRealUnitPrice" := log(realUnitPrice)]
  regData[, "retailerModule" := paste0(retailer_code, product_module_code)]
  regData[, "retailerBrand" := paste0(retailer_code, product_module_code, brand_code_uc)]

  # Running regression to see how prices are affected by different factors
  reg2 <- felm(data = regData, logRealUnitPrice ~ coupon + quintile |
                 retailerBrand, weights = regData$realExp)
  reg3 <- felm(data = regData[storable == 1], logRealUnitPrice ~ coupon + quintile |
                 retailerBrand, weights = regData[storable == 1]$realExp)
  reg4 <- felm(data = regData[storable == 0], logRealUnitPrice ~ coupon + quintile |
                 retailerBrand, weights = regData[storable == 0]$realExp)
  stargazer(reg2, reg3, reg4, type = "text",
            add.lines = list(c("Fixed Effect", "Retailer-Brand",
                               "Retailer-Brand", "Retailer-Brand")),
            single.row = TRUE, no.space = TRUE, omit.stat = c("ser", "rsq"),
            out.header = FALSE,
            column.labels = c("All Products", "Storable", "Non-Storable"),
            column.separate = c(1, 1, 1),
            dep.var.caption = "Log(Unit Price)", dep.var.labels.include = FALSE,
            notes.align = "l",
            digits = 2,
            label = "tab:overallSavings",
            out = paste0("/home/upenn/hossaine/tables/overallSavings", yr, ".tex"))

  coefStorable <- as.data.table(summary(reg3)$coefficients, keep.rownames = TRUE)
  coefStorable[, "storable" := 1]
  coefNonStorable <- as.data.table(summary(reg4)$coefficients, keep.rownames = TRUE)
  coefNonStorable[, "storable" := 0]
  coefsAll <- rbindlist(list(coefStorable, coefNonStorable))[, "panel_year" := yr]
  return(coefsAll)
}

coefsAll <- rbindlist(map(2004:2017, getSavings))

# Getting coefficients to graph
coefsAll[, "storable" := factor(storable, levels = c(0, 1),
                                labels = c("Non-Storable", "Storable"))]
coefsAll[, "rn" := gsub("quintile", "Quintile ", rn)]
coefsAll[rn == "coupon", "rn" := "Coupon"]
setnames(coefsAll, c("rn", "beta", "se", "t", "p", "Storable", "Year"))
fwrite(coefsAll, "/scratch/upenn/hossaine/coefsAll.csv")

# Making graphs
coefsAll <- fread("/scratch/upenn/hossaine/coefsAll.csv")
discounts <- c("Coupon", "Quintile 3", "Quintile 4", "Quintile 5")
coefsAll[abs(t) < critValue, "beta" := 0]
graphData <- coefsAll[rn != "Quintile 1", .(beta = mean(beta),
                          se = mean(se)), by = .(rn, Storable)]
ggplot(data = graphData, aes(x = as.factor(rn), y = beta, fill = Storable)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = beta - 1.96 * se, ymax = beta + 1.96 * se),
                width = 0.2, position = position_dodge(0.9)) +
  scale_x_discrete(limits = discounts) +
  labs(title = "Bulk Discounts Provide Largest Savings",
       subtitle = "Storable Items Offer Larger Quantity Discounts than Non-Storable Items",
       x = NULL,
       y = "Unit Price Savings",
       caption = paste0("Note: Black bars denote standard errors. Bars denote estimate ",
                        "of unit price discount \nafter controlling for retailer-brand fixed effects.")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0)) +
  scale_fill_grey()

ggsave(filename = "./figures/savingsStoreBrand.png", height = 6, width = 6)
