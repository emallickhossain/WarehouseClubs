# Estimates how much households could save if they purchased at the lowest
# unit cost to varying degrees of conservativeness
library(data.table)
library(lfe)
threads <- 8

# Getting trips and panel data for demographics and to deflate expenditures
trips <- fread("/scratch/upenn/hossaine/fullTrips.csv", nThread = threads,
               select = c("trip_code_uc", "household_code", "panel_year", "purchase_date"))
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

# Identifying lowest unit cost item purchased by each household
missedSavingsAll <- NULL
for (yr in 2004:2017) {
  print(yr)
  purch <- fread(paste0("/scratch/upenn/hossaine/fullPurch", yr, ".csv"),
                 nThread = threads,
                 select = c("trip_code_uc", "coupon_value", "deal_flag_uc",
                            "brand_code_uc", "product_module_code", "storable",
                            "packagePrice", "quartile", "quantity", "totalAmount"),
                 key = "trip_code_uc")
  purch[, ':=' (coupon = as.numeric(coupon_value > 0),
                unitPrice = packagePrice / totalAmount,
                totalExpenditure = (packagePrice - coupon_value) * quantity)]
  purch[, c("coupon_value", "deal_flag_uc", "packagePrice", "quartile", "quantity") := NULL]

  # Deflating expendtures
  setkey(purch, trip_code_uc)
  purch <- merge(purch, fullData, by = "trip_code_uc")
  purch[, "realExp" := totalExpenditure / newIndex * 100]
  purch[, "realUnitPrice" := unitPrice / newIndex * 100]
  purch[, c("totalExpenditure", "unitPrice", "newIndex") := NULL]

  # Adding in HH DMA
  panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
                 select = c("panel_year", "household_code", "dma_cd"),
                 key = c("household_code", "panel_year"))
  purch <- merge(purch, panel, by = c("household_code", "panel_year"))

  # Identifying lowest priced items purchased by a household in a given year
  # and combining that with purchase data
  minPricesHH <- purch[, .(realUnitPrice = min(realUnitPrice)),
                     by = .(household_code, panel_year, dma_cd,
                            product_module_code, brand_code_uc)]
  minPricesHH <- merge(minPricesHH, purch,
                       by = c("household_code", "panel_year", "dma_cd",
                              "product_module_code", "brand_code_uc", "realUnitPrice"))
  setnames(minPricesHH, c("realUnitPrice", "totalAmount", "coupon"),
           c("minRealUnitPriceHH", "minTotalAmountHH", "minCouponHH"))
  minPricesHH[, c("storable", "realExp") := NULL]
  purch <- merge(purch, minPricesHH, by = c("household_code", "panel_year", "dma_cd",
                                            "product_module_code", "brand_code_uc"))

  # Identifying lowest priced items purchased by any household in a DMA in a given year
  minPricesDMA <- purch[, .(realUnitPrice = min(realUnitPrice)),
                       by = .(dma_cd, panel_year, product_module_code, brand_code_uc)]
  minPricesDMA <- merge(minPricesDMA, purch,
                        by = c("dma_cd", "panel_year", "product_module_code",
                               "brand_code_uc", "realUnitPrice"))
  setnames(minPricesDMA, c("realUnitPrice", "totalAmount", "coupon"),
           c("minRealUnitPriceDMA", "minTotalAmountDMA", "minCouponDMA"))
  minPricesDMA[, c("storable", "realExp") := NULL]

  # Combining with purchase data
  purch <- merge(purch, minPricesHH, by = c("household_code", "panel_year", "dma_cd",
                                          "product_module_code", "brand_code_uc"))
  purch <- merge(purch, minPricesDMA, by = c("household_code", "panel_year", "dma_cd",
                                             "product_module_code", "brand_code_uc"))
  purch[, "lowerHH" := (minRealUnitPriceHH < realUnitPrice)]
  purch[, "lowerDMA" := (minRealUnitPriceDMA < realUnitPrice)]

  # Getting "missed savings"
  purch[, "pctSavings" := (realUnitPrice - minRealUnitPrice) / realUnitPrice]
  purch[, "biggerSize" := as.numeric(minTotalAmount > totalAmount)]

  # Replacing flag with actual savings for decomposition computations
  purch[biggerSize == 1, "biggerSize" := pctSavings]
  purch[coupon == 1, "coupon" := pctSavings]

  # Computing savings
  cols <- c("minCoupon", "pctSavings", "lower", "biggerSize")
  missedSavings <- purch[, lapply(.SD, weighted.mean, w = realExp),
                          by = .(household_code, panel_year, storable),
                          .SDcols = cols]

  # Combining together
  missedSavingsAll <- rbindlist(list(missedSavingsAll, missedSavings), use.names = TRUE)
}

# Adding demographics
panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("panel_year", "household_code", "projection_factor",
                          "household_income", "household_size", "age", "child",
                          "dma_cd"),
               key = c("household_code", "panel_year"))
panel[, "household_income" := as.factor(household_income)]
missedSavingsAll <- merge(missedSavingsAll, panel, by = c("household_code", "panel_year"))

# Running regressions
runRegAll <- function(y) {
  regForm <- as.formula(paste0(y, "~", "household_income +",
                               "+ age + household_size + child + 0"))

  # All products
  regData <- missedSavingsAll
  regAll <- felm(data = regData, formula = regForm, weights = regData$projection_factor)
  coefsAll <- as.data.table(summary(regAll)$coefficients, keep.rownames = TRUE)

  # Storables
  regData <- missedSavingsAll[storable == 1]
  regStorable <- felm(data = regData, formula = regForm, weights = regData$projection_factor)

  # Non-Storables
  regData <- missedSavingsAll[storable == 0]
  regNonStorable <- felm(data = regData, formula = regForm, weights = regData$projection_factor)

  # Combining
  coefs <- rbindlist(list(coefsAll, coefsStorable, coefsNonStorable), use.names = TRUE)
  return(coefs)
}

discounts <- c("coupon", "sale", "generic", "bulk",
               "CouponOnly", "SaleOnly", "GenericOnly", "BulkOnly", "SG",
               "SB", "CG", "CB", "GB",
               "none", "one", "two", "ThreePlus")
finalCoefs <- rbindlist(map(discounts, runRegAll), use.names = TRUE)

# Organizing graph data
graphData <- finalCoefs[grepl("household_income", rn)]
graphData <- graphData[!grepl("panel_year", rn)]
graphData[, "rn" := gsub("household_income", "", rn)]
graphData[, "rn" := factor(rn, levels = c(8, 10, 11, 13, 15, 16, 17, 18, 19, 21, 23, 26, 27),
                           labels = c(11, 13.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 55, 65, 85, 100),
                           ordered = TRUE)]
graphData[, "rn" := as.numeric(as.character(rn))]
setnames(graphData, c("rn", "beta", "se", "t", "p", "disc", "Product Type"))

# Housekeeping
graphData[disc == "coupon", "disc" := "Coupon"]
graphData[disc == "bulk", "disc" := "Bulk"]
graphData[disc == "generic", "disc" := "Generic"]
graphData[disc == "sale", "disc" := "Sale"]
graphData[disc == "none", "disc" := "None"]
graphData[disc == "one", "disc" := "One"]
graphData[disc == "two", "disc" := "Two"]

# Graphing
panels <- c("Coupon", "Sale", "Generic", "Bulk")
panels <- c("CouponOnly", "SaleOnly", "GenericOnly", "BulkOnly")
ggplot(data = graphData[disc %in% panels], aes(x = rn, y = beta, color = `Product Type`)) +
  geom_errorbar(aes(ymin = beta - 1.96 * se, ymax = beta + 1.96 * se), width = 0.2) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  facet_wrap(vars(disc), scales = "fixed") +
  labs(title = "Rich Households Bulk Buy More",
       x = "Household Income", y = "Share of Purchases",
       caption = paste0("Source: Author calulations from Nielsen Consumer Panel. \n",
                        "Note: Demographic adjustments control for household size, \n",
                        "age, and presence of children. Income-specific time trends \n",
                        "are also controlled for.")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0)) +
  scale_color_grey()

ggsave("./figures/savingsBehavior.png", height = 6, width = 6)

panels <- c("None", "One", "Two")
ggplot(data = graphData[disc %in% panels], aes(x = rn, y = beta, color = `Product Type`)) +
  geom_errorbar(aes(ymin = beta - 1.96 * se, ymax = beta + 1.96 * se), width = 0.2) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  facet_wrap(vars(disc), scales = "fixed") +
  labs(title = "Most Households Use One or Fewer Discounts",
       x = "Household Income", y = "Share of Purchases",
       caption = paste0("Source: Author calulations from Nielsen Consumer Panel. \n",
                        "Note: Demographic adjustments control for household size, \n",
                        "age, and presence of children. Income-specific time trends \n",
                        "are also controlled for.")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0)) +
  scale_color_grey()

ggsave("./figures/numberOfDiscounts.png", height = 6, width = 6)

# Getting distributions around binned scatterplot by subtracting coefficients
# on household characteristics from the households propensity
discBehaviorAll[, "ageComp" := age * coefsAll[rn == "age"]$Estimate]
discBehaviorAll[, "sizeComp" := household_size * coefsAll[rn == "household_size"]$Estimate]
discBehaviorAll[, "childComp" := child * coefsAll[rn == "child"]$Estimate]
discBehaviorAll[, "bulkAdj" := bulk - ageComp - sizeComp - childComp]
discBehaviorAll[, "couponAdj" := coupon - ageComp - sizeComp - childComp]
discBehaviorAll[, "saleAdj" := sale - ageComp - sizeComp - childComp]
discBehaviorAll[, "genericAdj" := generic - ageComp - sizeComp - childComp]

# Graphing distributions
graphData <- melt(discBehaviorAll, id.vars = c("household_code", "panel_year", "household_income"),
                  measure.vars = patterns("*Adj"))
graphData[, "household_income" := factor(household_income,
                                         levels = c(8, 10, 11, 13, 15, 16, 17, 18, 19, 21, 23, 26, 27),
                                         labels = c(11, 13.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 55, 65, 85, 100),
                                         ordered = TRUE)]
ggplot(na.omit(graphData), aes(x = value, y = as.factor(household_income))) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2) +
  scale_x_continuous(limits = c(0, 1)) +
  facet_wrap(vars(variable), scales = "fixed") +
  labs(title = "Most Households Use One or Fewer Discounts",
       x = "Share of Purchases", y = "Household Income",
       caption = paste0("Source: Author calulations from Nielsen Consumer Panel. \n",
                        "Note: Demographic adjustments control for household size, \n",
                        "age, and presence of children.")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0)) +
  scale_color_grey()

# Storable / Non-Storable items
discBehaviorStorage[, "ageComp" := age * coefsAll[rn == "age"]$Estimate]
discBehaviorStorage[, "sizeComp" := household_size * coefsAll[rn == "household_size"]$Estimate]
discBehaviorStorage[, "childComp" := child * coefsAll[rn == "child"]$Estimate]
discBehaviorStorage[, "bulkAdj" := bulk - ageComp - sizeComp - childComp]
discBehaviorStorage[, "couponAdj" := coupon - ageComp - sizeComp - childComp]
discBehaviorStorage[, "saleAdj" := sale - ageComp - sizeComp - childComp]
discBehaviorStorage[, "genericAdj" := generic - ageComp - sizeComp - childComp]

# Graphing distributions
graphData <- melt(discBehaviorStorage,
                  id.vars = c("household_code", "panel_year", "household_income", "storable"),
                  measure.vars = patterns("*Adj"))
graphData[, "household_income" := factor(household_income,
                                         levels = c(8, 10, 11, 13, 15, 16, 17, 18, 19, 21, 23, 26, 27),
                                         labels = c(11, 13.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 55, 65, 85, 100),
                                         ordered = TRUE)]
ggplot(na.omit(graphData[storable == 0]), aes(x = value, y = as.factor(household_income))) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2) +
  scale_x_continuous(limits = c(0, 1)) +
  facet_wrap(vars(variable), scales = "fixed") +
  labs(title = "Most Households Use One or Fewer Discounts",
       x = "Share of Purchases", y = "Household Income",
       caption = paste0("Source: Author calulations from Nielsen Consumer Panel. \n",
                        "Note: Demographic adjustments control for household size, \n",
                        "age, and presence of children.")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0)) +
  scale_color_grey()
