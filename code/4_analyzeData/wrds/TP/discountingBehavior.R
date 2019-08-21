# Computes household propensity for different discounting behaviors
# Fact 1: About 40% of purchases are made without any discounts and another 45%
# are made with only 1 type of discount. Less than 15% of purchases combine 2 or
# more discount methods.
# Fact 2: These patterns are correlated with income with high income households
# more likely to use any kind of discounting as well as using multiple kinds of
# discounts. Separating out single discounts versus multiple discounts
# does not qualitatively change the results.
# Fact 3: Buying in bulk (defined as a purchase in the top 2 quintiles of the
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
library(stringr)
library(stargazer)
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
discBehaviorFood <- NULL
for (yr in 2004:2017) {
  print(yr)
  purch <- fread(paste0("/scratch/upenn/hossaine/fullPurch", yr, ".csv"),
                 nThread = threads,
                 select = c("trip_code_uc", "coupon_value",
                            "brand_code_uc", "product_module_code", "food",
                            "packagePrice", "quintile", "quantity"),
                 key = "trip_code_uc")
  purch[, ':=' (coupon = as.integer(coupon_value > 0),
                generic = as.integer(brand_code_uc == 536746),
                bulk = as.integer(quintile >= 4),
                totalExpenditure = packagePrice * quantity)]
  purch[, c("coupon_value", "packagePrice", "quantity", "brand_code_uc") := NULL]

  # Deflating expenditures
  setkey(purch, trip_code_uc)
  purch <- merge(purch, fullData, by = "trip_code_uc")[, "trip_code_uc" := NULL]
  purch[, "realExp" := totalExpenditure / newIndex * 100]
  purch[, c("totalExpenditure", "newIndex") := NULL]

  # Coding combination discounts
  purch[, "savingsTypes" := coupon + generic + bulk]
  purch[, ':=' (none = (savingsTypes == 0),
                one = (savingsTypes == 1),
                two = (savingsTypes == 2),
                three = (savingsTypes == 3))]

  # Getting household propensities by product type
  cols <- c("coupon", "generic", "bulk", "none", "one", "two", "three")
  householdAvgAll <- purch[, lapply(.SD, weighted.mean, w = realExp),
                           .SDcols = cols,
                           by = .(household_code, panel_year)]
  householdAvgFood <- purch[, lapply(.SD, weighted.mean, w = realExp),
                               .SDcols = cols,
                               by = .(household_code, panel_year, food)]

  # Combining
  discBehaviorAll <- rbindlist(list(discBehaviorAll, householdAvgAll),
                               use.names = TRUE)
  discBehaviorFood <- rbindlist(list(discBehaviorFood, householdAvgFood),
                                   use.names = TRUE)
}

# Saving
fwrite(discBehaviorAll, "/scratch/upenn/hossaine/discBehaviorAll.csv", nThread = threads)
fwrite(discBehaviorFood, "/scratch/upenn/hossaine/discBehaviorFood.csv", nThread = threads)

# Adding demographics
discBehaviorAll <- fread("/scratch/upenn/hossaine/discBehaviorAll.csv", nThread = threads)
discBehaviorFood <- fread("/scratch/upenn/hossaine/discBehaviorFood.csv", nThread = threads)

panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("panel_year", "household_code", "projection_factor",
                          "household_income", "household_size", "age", "child",
                          "dma_cd", "household_income_coarse", "married",
                          "carShare", "law", "zip_code", "college", "men", "women",
                          "nChildren"),
               key = c("household_code", "panel_year"))
panel[, "household_income" := as.factor(household_income)]
panel[, "household_income_coarse" := as.factor(household_income_coarse)]
panel[, "lawInd" := (law >= 3)]

discBehaviorAll <- merge(discBehaviorAll, panel, by = c("household_code", "panel_year"))
discBehaviorFood <- merge(discBehaviorFood, panel, by = c("household_code", "panel_year"))

# Running regressions
# If you add an interaction with panel year, you can test if there have been
# different trends over time, but nothing seemed significant or big enough
# to change the overall picture of increasing bulk with income.
runRegAll <- function(y) {
  regForm <- as.formula(paste0(y, "~", "household_income + married + ",
                               "age + men + women + nChildren + college | ",
                               "dma_cd + panel_year"))

  # All products
  regData <- discBehaviorAll
  regAll <- felm(data = regData, formula = regForm, weights = regData$projection_factor)
  coefsAll <- as.data.table(summary(regAll)$coefficients, keep.rownames = TRUE)
  confIntAll <- as.data.table(confint(regAll), keep.rownames = TRUE)
  coefsAll <- merge(coefsAll, confIntAll, by = "rn")
  coefsAll[, c("discount", "reg") := .(y, "All")]

  # Storables
  regData <- discBehaviorFood[food == 1]
  regFood <- felm(data = regData, formula = regForm, weights = regData$projection_factor)
  coefsFood <- as.data.table(summary(regFood)$coefficients, keep.rownames = TRUE)
  confIntFood <- as.data.table(confint(regFood), keep.rownames = TRUE)
  coefsFood <- merge(coefsFood, confIntFood, by = "rn")
  coefsFood[, c("discount", "reg") := .(y, "Food")]

  # Non-Storables
  regData <- discBehaviorFood[food == 0]
  regNonFood <- felm(data = regData, formula = regForm, weights = regData$projection_factor)
  coefsNonFood <- as.data.table(summary(regNonFood)$coefficients, keep.rownames = TRUE)
  confIntNonFood <- as.data.table(confint(regNonFood), keep.rownames = TRUE)
  coefsNonFood <- merge(coefsNonFood, confIntNonFood, by = "rn")
  coefsNonFood[, c("discount", "reg") := .(y, "Non-Food")]

  # Combining
  coefs <- rbindlist(list(coefsAll, coefsFood, coefsNonFood), use.names = TRUE)
  return(coefs)
}

discounts <- c("coupon", "generic", "bulk")
finalCoefs <- rbindlist(map(discounts, runRegAll), use.names = TRUE)

# Organizing graph data
graphData <- finalCoefs[grepl("household_income", rn)]
graphData[, "rn" := gsub("household_income", "", rn)]
graphData[, "rn" := factor(rn, levels = c(4, 6, 8, 10, 11, 13, 15, 16, 17, 18, 19, 21, 23, 26, 27),
                           labels = c(6.5, 9, 11, 13.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 55, 65, 85, 100),
                           ordered = TRUE)]
graphData[, "rn" := as.numeric(as.character(rn))]
setnames(graphData, c("rn", "beta", "se", "t", "p", "LCL", "UCL", "disc", "Product Type"))

# Housekeeping
graphData[disc == "coupon", "disc" := "Coupon"]
graphData[disc == "bulk", "disc" := "Bulk"]
graphData[disc == "generic", "disc" := "Generic"]
graphData[disc == "none", "disc" := "None"]
graphData[disc == "one", "disc" := "One"]
graphData[disc == "two", "disc" := "Two"]
graphData[disc == "three", "disc" := "Three"]

# Graphing
panels <- c("Coupon", "Generic", "Bulk")
ggplot(data = graphData[disc %in% "Bulk" & `Product Type` != "All"],
       aes(x = rn, y = beta * 100, color = `Product Type`)) +
  geom_errorbar(aes(ymin = 100 * LCL,
                    ymax = 100 * UCL), width = 0.2) +
  geom_point(aes(shape = `Product Type`), size = 3) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  labs(x = "Household Income",
       y = "Difference in Bulk Purchasing (Percentage Points)") +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom") +
  scale_color_grey()

ggsave("./figures/savingsBehavior.png", height = 4, width = 6)

################################################################################
############### ROBUSTNESS #####################################################
################################################################################
#Redoing regressions slowly dropping in covariates and looking at coefficient changes
reg1 <- felm(formula = bulk ~ household_income,
             data = discBehaviorAll, weights = discBehaviorAll$projection_factor)
reg2 <- felm(formula = bulk ~ household_income + married,
             data = discBehaviorAll, weights = discBehaviorAll$projection_factor)
reg3 <- felm(formula = bulk ~ household_income + married + age,
             data = discBehaviorAll, weights = discBehaviorAll$projection_factor)
reg4 <- felm(formula = bulk ~ household_income + married + age + men + women +
               nChildren,
             data = discBehaviorAll, weights = discBehaviorAll$projection_factor)
reg5 <- felm(formula = bulk ~ household_income + married + age + men + women +
               nChildren + college,
             data = discBehaviorAll, weights = discBehaviorAll$projection_factor)
reg6 <- felm(formula = bulk ~ household_income + married + age + men + women +
               nChildren + college | dma_cd | 0 | dma_cd,
             data = discBehaviorAll, weights = discBehaviorAll$projection_factor)
reg7 <- felm(formula = bulk ~ household_income + married + age + men + women +
               nChildren + college | dma_cd + panel_year | 0 | dma_cd,
             data = discBehaviorAll, weights = discBehaviorAll$projection_factor)
stargazer(reg1, reg2, reg3, reg4, reg5, reg6, reg7, type = "text")

#Redoing regressions slowly dropping in covariates and looking at coefficient changes
reg1 <- felm(formula = bulk ~ household_income,
             data = discBehaviorFood[food == 1],
             weights = discBehaviorFood[food == 1]$projection_factor)
reg2 <- felm(formula = bulk ~ household_income + married,
             data = discBehaviorFood[food == 1],
             weights = discBehaviorFood[food == 1]$projection_factor)
reg3 <- felm(formula = bulk ~ household_income + married + age,
             data = discBehaviorFood[food == 1],
             weights = discBehaviorFood[food == 1]$projection_factor)
reg4 <- felm(formula = bulk ~ household_income + married + age + men + women +
               nChildren,
             data = discBehaviorFood[food == 1],
             weights = discBehaviorFood[food == 1]$projection_factor)
reg5 <- felm(formula = bulk ~ household_income + married + age + men + women +
               nChildren + college,
             data = discBehaviorFood[food == 1],
             weights = discBehaviorFood[food == 1]$projection_factor)
reg6 <- felm(formula = bulk ~ household_income + married + age + men + women +
               nChildren + college | dma_cd | 0 | dma_cd,
             data = discBehaviorFood[food == 1],
             weights = discBehaviorFood[food == 1]$projection_factor)
reg7 <- felm(formula = bulk ~ household_income + married + age + men + women +
               nChildren + college | dma_cd + panel_year | 0 | dma_cd,
             data = discBehaviorFood[food == 1],
             weights = discBehaviorFood[food == 1]$projection_factor)
stargazer(reg1, reg2, reg3, reg4, reg5, reg6, reg7, type = "text",
          add.lines = list(c("Market FE's", "N", "N", "N", "N", "N", "N", "Y", "Y"),
                           c("Year FE's",   "N", "N", "N", "N", "N", "N", "N", "Y")),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          dep.var.caption = "", dep.var.labels.include = FALSE,
          omit = c("Constant"),
          covariate.labels = c("8-10k", "10-12k", "12-15k", "15-20k", "20-25k",
                               "25-30k", "30-35k", "35-40k", "40-45k", "45-50k",
                               "50-60k", "60-70k", "70-100k", ">100k", "Married",
                               "Age", "Men", "Women", "Children", "College"),
          notes.align = "l",
          notes.append = TRUE,
          digits = 3,
          notes = c("Source: Author calulations from Nielsen Consumer Panel. Columns (7) and (8) ",
                    "cluster standard errors at the market level"),
          label = "tab:discountingBehaviorFood",
          title = "Correlation of Bulk Buying and Demographics (Food Products)",
          out = "tables/AppendixDiscountingBehaviorFood.tex")

#Redoing regressions slowly dropping in covariates and looking at coefficient changes
reg1 <- felm(formula = bulk ~ household_income,
             data = discBehaviorFood[food == 0],
             weights = discBehaviorFood[food == 0]$projection_factor)
reg2 <- felm(formula = bulk ~ household_income + married,
             data = discBehaviorFood[food == 0],
             weights = discBehaviorFood[food == 0]$projection_factor)
reg3 <- felm(formula = bulk ~ household_income + married + age,
             data = discBehaviorFood[food == 0],
             weights = discBehaviorFood[food == 0]$projection_factor)
reg4 <- felm(formula = bulk ~ household_income + married + age + men + women +
               nChildren,
             data = discBehaviorFood[food == 0],
             weights = discBehaviorFood[food == 0]$projection_factor)
reg5 <- felm(formula = bulk ~ household_income + married + age + men + women +
               nChildren + college,
             data = discBehaviorFood[food == 0],
             weights = discBehaviorFood[food == 0]$projection_factor)
reg6 <- felm(formula = bulk ~ household_income + married + age + men + women +
               nChildren + college | dma_cd | 0 | dma_cd,
             data = discBehaviorFood[food == 0],
             weights = discBehaviorFood[food == 0]$projection_factor)
reg7 <- felm(formula = bulk ~ household_income + married + age + men + women +
               nChildren + college | dma_cd + panel_year | 0 | dma_cd,
             data = discBehaviorFood[food == 0],
             weights = discBehaviorFood[food == 0]$projection_factor)
stargazer(reg1, reg2, reg3, reg4, reg5, reg6, reg7, type = "text",
          add.lines = list(c("Market FE's", "N", "N", "N", "N", "N", "N", "Y", "Y"),
                           c("Year FE's",   "N", "N", "N", "N", "N", "N", "N", "Y")),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          dep.var.caption = "", dep.var.labels.include = FALSE,
          omit = c("Constant"),
          covariate.labels = c("8-10k", "10-12k", "12-15k", "15-20k", "20-25k",
                               "25-30k", "30-35k", "35-40k", "40-45k", "45-50k",
                               "50-60k", "60-70k", "70-100k", ">100k", "Married",
                               "Age", "Men", "Women", "Children", "College"),
          notes.align = "l",
          notes.append = TRUE,
          digits = 3,
          notes = c("Source: Author calulations from Nielsen Consumer Panel. Columns (7) and (8) ",
                    "cluster standard errors at the market level"),
          label = "tab:discountingBehaviorNonFood",
          title = "Correlation of Bulk Buying and Demographics (Non-Food Products)",
          out = "tables/AppendixDiscountingBehaviorNonFood.tex")

################################################################################
########### END ROBUSTNESS #####################################################
################################################################################
# Redoing savings behavior within locations by adding ZIP-year FEs (DMA-year FE's
# don't change much at all. ZIP reduces the difference more.)
discBehaviorFood[, "zipYear" := paste(zip_code, panel_year, sep = "_")]

reg0 <- felm(bulk ~ household_income + men + women + nChildren + age +
               married + college | dma_cd + panel_year,
             data = discBehaviorFood[food == 0],
             weights = discBehaviorFood[food == 0]$projection_factor)
reg1 <- felm(bulk ~ household_income + men + women + nChildren + age +
               married + college | zipYear,
             data = discBehaviorFood[food == 0],
             weights = discBehaviorFood[food == 0]$projection_factor)

coef0 <- as.data.table(summary(reg0)$coefficients, keep.rownames = TRUE)
confInt0 <- as.data.table(confint(reg0), keep.rownames = TRUE)
coef0 <- merge(coef0, confInt0, by = "rn")
coef0[, "reg" := "Without Zip-Year FE"]

coef1 <- as.data.table(summary(reg1)$coefficients, keep.rownames = TRUE)
confInt1 <- as.data.table(confint(reg1), keep.rownames = TRUE)
coef1 <- merge(coef1, confInt1, by = "rn")
coef1[, "reg" := "With ZIP-Year FE"]

finalCoefs <- rbindlist(list(coef0, coef1), use.names = TRUE)
graphData <- finalCoefs[grepl("household_income", rn)]
graphData[, "rn" := gsub("household_income", "", rn)]
graphData[, "rn" := factor(rn, levels = c(4, 6, 8, 10, 11, 13, 15, 16, 17, 18, 19, 21, 23, 26, 27),
                           labels = c(6.5, 9, 11, 13.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 55, 65, 85, 100),
                           ordered = TRUE)]
graphData[, "rn" := as.numeric(as.character(rn))]
setnames(graphData, c("rn", "beta", "se", "t", "p", "LCL", "UCL", "Type"))

ggplot(data = graphData,
       aes(x = rn, y = beta * 100, color = Type)) +
  geom_errorbar(aes(ymin = 100 * LCL,
                    ymax = 100 * UCL), width = 0.2) +
  geom_point(aes(shape = Type), size = 3) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  labs(x = "Household Income ($000s)",
       y = "Difference in Bulk Purchasing (Percentage Points)") +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom") +
  scale_color_grey()
ggsave(filename = "./figures/discountingBehaviorZipYearFE.png", height = 4, width = 6)


#
# panels <- c("None", "One", "Two")
# ggplot(data = graphData[disc %in% panels], aes(x = rn, y = beta, color = `Product Type`)) +
#   geom_errorbar(aes(ymin = beta - 1.96 * se, ymax = beta + 1.96 * se), width = 0.2) +
#   geom_point() +
#   geom_hline(yintercept = 0) +
#   geom_vline(xintercept = 0) +
#   facet_wrap(vars(disc), scales = "fixed") +
#   labs(title = "Most Households Use One or Fewer Discounts",
#        x = "Household Income", y = "Share of Purchases",
#        caption = paste0("Source: Author calulations from Nielsen Consumer Panel. \n",
#                         "Note: Demographic adjustments control for household size, \n",
#                         "age, presence of children, year, and market.")) +
#   theme_fivethirtyeight() +
#   theme(axis.title = element_text(), plot.caption = element_text(hjust = 0)) +
#   scale_color_grey()
#
# ggsave("./figures/numberOfDiscounts.png", height = 6, width = 6)
#
# # Getting distributions around binned scatterplot by subtracting coefficients
# # on household characteristics from the households propensity
# discBehaviorAll[, "ageComp" := age * finalCoefs[rn == "age" & reg == "All"]$Estimate]
# discBehaviorAll[, "sizeComp" := household_size * finalCoefs[rn == "household_size" & reg == "All"]$Estimate]
# discBehaviorAll[, "childComp" := child * finalCoefs[rn == "child" & reg == "All"]$Estimate]
# discBehaviorAll[, "bulkAdj" := bulk - ageComp - sizeComp - childComp]
# discBehaviorAll[, "couponAdj" := coupon - ageComp - sizeComp - childComp]
# discBehaviorAll[, "genericAdj" := generic - ageComp - sizeComp - childComp]
#
# # Graphing distributions
# graphData <- melt(discBehaviorAll, id.vars = c("household_code", "panel_year", "household_income"),
#                   measure.vars = patterns("*Adj"))
# graphData[, "household_income" := factor(household_income,
#                                          levels = c(8, 10, 11, 13, 15, 16, 17, 18, 19, 21, 23, 26, 27),
#                                          labels = c(11, 13.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 55, 65, 85, 100),
#                                          ordered = TRUE)]
# ggplot(na.omit(graphData), aes(x = value, y = as.factor(household_income))) +
#   stat_density_ridges(quantile_lines = TRUE, quantiles = 2) +
#   #scale_x_continuous(limits = c(0, 1)) +
#   facet_wrap(vars(variable), scales = "fixed") +
#   labs(title = "Most Households Use One or Fewer Discounts",
#        x = "Share of Purchases", y = "Household Income",
#        caption = paste0("Source: Author calulations from Nielsen Consumer Panel. \n",
#                         "Note: Demographic adjustments control for household size, \n",
#                         "age, and presence of children.")) +
#   theme_fivethirtyeight() +
#   theme(axis.title = element_text(), plot.caption = element_text(hjust = 0)) +
#   scale_color_grey()
#
# # Storable / Non-Storable items
# discBehaviorStorage[, "ageComp" := age * finalCoefs[rn == "age"]$Estimate]
# discBehaviorStorage[, "sizeComp" := household_size * finalCoefs[rn == "household_size"]$Estimate]
# discBehaviorStorage[, "childComp" := child * finalCoefs[rn == "child"]$Estimate]
# discBehaviorStorage[, "bulkAdj" := bulk - ageComp - sizeComp - childComp]
# discBehaviorStorage[, "couponAdj" := coupon - ageComp - sizeComp - childComp]
# discBehaviorStorage[, "saleAdj" := sale - ageComp - sizeComp - childComp]
# discBehaviorStorage[, "genericAdj" := generic - ageComp - sizeComp - childComp]
#
# # Graphing distributions
# graphData <- melt(discBehaviorStorage,
#                   id.vars = c("household_code", "panel_year", "household_income", "storable"),
#                   measure.vars = patterns("*Adj"))
# graphData[, "household_income" := factor(household_income,
#                                          levels = c(8, 10, 11, 13, 15, 16, 17, 18, 19, 21, 23, 26, 27),
#                                          labels = c(11, 13.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 55, 65, 85, 100),
#                                          ordered = TRUE)]
# ggplot(na.omit(graphData[storable == 0]), aes(x = value, y = as.factor(household_income))) +
#   stat_density_ridges(quantile_lines = TRUE, quantiles = 2) +
#   scale_x_continuous(limits = c(0, 1)) +
#   facet_wrap(vars(variable), scales = "fixed") +
#   labs(title = "Most Households Use One or Fewer Discounts",
#        x = "Share of Purchases", y = "Household Income",
#        caption = paste0("Source: Author calulations from Nielsen Consumer Panel. \n",
#                         "Note: Demographic adjustments control for household size, \n",
#                         "age, and presence of children.")) +
#   theme_fivethirtyeight() +
#   theme(axis.title = element_text(), plot.caption = element_text(hjust = 0)) +
#   scale_color_grey()
