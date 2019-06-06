# Computes household propensity for different discounting behaviors
# Fact 1: About 40% of purchases are made without any discounts and another 45%
# are made with only 1 type of discount. Less than 15% of purchases combine 2 or
# more discount methods.
# Fact 2: These patterns are correlated with income with high income households
# more likely to use any kind of discounting as well as using multiple kinds of
# discounts. Separating out single discounts versus multiple discounts
# does not qualitatively change the results.
# Fact 3: Buying in bulk (defined as a purchase in the top quartile of the
# product module's size distribution) is the most common form of discounting
# and it is more common for storable products.
# Fact 4: Rich households are more likely to buy in bulk and this differences is
# primarily concentrated among storable products.
# Fact 5: Buying generic brands is decreasing in income.
# Fact 6: There are some changes in behavior over time (especially during the
# Great Recession), but even allowing for these trends by including a income * year
# regressor does not change the core relationship between bulk purchasing and income.
# Fact 7: After adjusting for household demographics, as income increases, both
# the median propensity to make bulk purchases increases as well as the overall
# tail of the distribution.
library(data.table)
library(ggplot2)
library(ggthemes)
library(ggridges)
library(lfe)
library(purrr)
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

# Getting all purchases and coding them by discounting behavior
discBehaviorAll <- NULL
discBehaviorStorage <- NULL
for (yr in 2004:2017) {
  print(yr)
  purch <- fread(paste0("/scratch/upenn/hossaine/fullPurch", yr, ".csv"),
                 nThread = threads,
                 select = c("trip_code_uc", "coupon_value", "deal_flag_uc",
                            "brand_code_uc", "product_module_code", "storable",
                            "packagePrice", "quartile", "quantity"),
                 key = "trip_code_uc")
  purch[, ':=' (coupon = as.integer(coupon_value > 0),
                sale = as.integer(deal_flag_uc > 0 & coupon_value == 0),
                generic = as.integer(brand_code_uc == 536746),
                bulk = as.integer(quartile == 4),
                totalExpenditure = (packagePrice - coupon_value) * quantity)]
  purch[, c("coupon_value", "deal_flag_uc", "packagePrice", "quantity", "brand_code_uc") := NULL]

  # Deflating expendtures
  setkey(purch, trip_code_uc)
  purch <- merge(purch, fullData, by = "trip_code_uc")[, "trip_code_uc" := NULL]
  purch[, "realExp" := totalExpenditure / newIndex * 100]
  purch[, c("totalExpenditure", "newIndex") := NULL]

  # Coding combination discounts
  purch[, "savingsTypes" := coupon + sale + generic + bulk]
  purch[, ':=' (BulkOnly = bulk * (savingsTypes == 1),
                SaleOnly = sale * (savingsTypes == 1),
                GenericOnly = generic * (savingsTypes == 1),
                CouponOnly = coupon * (savingsTypes == 1),
                SG = (sale * generic) * (savingsTypes == 2),
                SB = (sale * bulk) * (savingsTypes == 2),
                CG = (coupon * generic) * (savingsTypes == 2),
                CB = (coupon * bulk) * (savingsTypes == 2),
                GB = (generic * bulk) * (savingsTypes == 2),
                none = (savingsTypes == 0),
                one = (savingsTypes == 1),
                two = (savingsTypes == 2),
                ThreePlus = (savingsTypes == 3))]

  # Getting household propensities by product type
  cols <- c("coupon", "sale", "generic", "bulk",
            "CouponOnly", "SaleOnly", "GenericOnly", "BulkOnly",
            "SG", "SB", "CG", "CB", "GB",
            "none", "one", "two", "ThreePlus")
  householdAvgAll <- purch[, lapply(.SD, weighted.mean, w = realExp),
                           .SDcols = cols,
                           by = .(household_code, panel_year)]
  householdAvgStorage <- purch[, lapply(.SD, weighted.mean, w = realExp),
                               .SDcols = cols,
                               by = .(household_code, panel_year, storable)]

  # Combining
  discBehaviorAll <- rbindlist(list(discBehaviorAll, householdAvgAll),
                               use.names = TRUE)
  discBehaviorStorage <- rbindlist(list(discBehaviorStorage, householdAvgStorage),
                                   use.names = TRUE)
}

# Saving
fwrite(discBehaviorAll, "/scratch/upenn/hossaine/discBehaviorAll.csv", nThread = threads)
fwrite(discBehaviorStorage, "/scratch/upenn/hossaine/discBehaviorStorage.csv", nThread = threads)

# Adding demographics
panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("panel_year", "household_code", "projection_factor",
                          "household_income", "household_size", "age", "child",
                          "dma_cd"),
               key = c("household_code", "panel_year"))
panel[, "household_income" := as.factor(household_income)]

discBehaviorAll <- merge(discBehaviorAll, panel, by = c("household_code", "panel_year"))
discBehaviorStorage <- merge(discBehaviorStorage, panel, by = c("household_code", "panel_year"))

# Running regressions
runRegAll <- function(y) {
  regForm <- as.formula(paste0(y, "~", "household_income * as.factor(panel_year) ",
                               "+ age + household_size + child + 0"))

  # All products
  regData <- discBehaviorAll
  regAll <- felm(data = regData, formula = regForm, weights = regData$projection_factor)
  coefsAll <- as.data.table(summary(regAll)$coefficients, keep.rownames = TRUE)
  residAll <- as.data.table(summary(regAll)$residuals, keep.rownames = TRUE)
  coefsAll <- merge(coefsAll, residAll, by = "rn")
  coefsAll[, c("discount", "reg") := .(y, "All")]

  # Storables
  regData <- discBehaviorStorage[storable == 1]
  regStorable <- felm(data = regData, formula = regForm, weights = regData$projection_factor)
  coefsStorable <- as.data.table(summary(regStorable)$coefficients, keep.rownames = TRUE)
  coefsStorable[, c("discount", "reg") := .(y, "Storable")]

  # Non-Storables
  regData <- discBehaviorStorage[storable == 0]
  regNonStorable <- felm(data = regData, formula = regForm, weights = regData$projection_factor)
  coefsNonStorable <- as.data.table(summary(regNonStorable)$coefficients, keep.rownames = TRUE)
  coefsNonStorable[, c("discount", "reg") := .(y, "Non-Storable")]

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
