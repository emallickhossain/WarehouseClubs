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
library(lfe)
library(purrr)
library(stringr)
library(stargazer)
# library(binsreg)
threads <- 8

# Getting trips and panel data for demographics and to deflate expenditures
trips <- fread("/scratch/upenn/hossaine/fullTrips.csv", nThread = threads,
               select = c("trip_code_uc", "household_code", "panel_year",
                          "purchase_date", "channel_type", "retailer_code"))
trips[, c("yearMonth") := .(substr(purchase_date, 1, 7))]

# Getting chain type for retailers (counting unique states a retailer is in)
# Using local, regional, national definition from "The Role of Retail Chains" by
# Jarmin, Klimek, and Miranda 2005 CESWP 05-30
panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("panel_year", "household_code", "fips"),
               key = c("household_code", "panel_year"))
panel[, "fips" := str_pad(fips, 5, "left", "0")]
panel[, "state" := substr(fips, 1, 2)][, "fips" := NULL]
trips <- merge(trips, panel, by = c("household_code", "panel_year"))
chainType <- trips[, .(nStates = uniqueN(state)), by = retailer_code]
chainType[nStates == 1, "chainType" := "Local"]
chainType[nStates %in% 2:10, "chainType" := "Regional"]
chainType[nStates > 10, "chainType" := "National"]
chainType[, "nStates" := NULL]
trips <- merge(trips, chainType, by = "retailer_code")

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
discBehaviorWOM <- NULL
discBehaviorChannel <- NULL
discBehaviorChain <- NULL
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
  purch[, "dayOfMonth" := as.integer(substr(purchase_date, 9, 10))]
  purch[, "WOM" := ceiling(dayOfMonth / 7)]
  purch[WOM > 4, "WOM" := 4]
  purch[, "WOM" := as.factor(WOM)]

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
  householdWOM <- purch[, lapply(.SD, weighted.mean, w = realExp),
                        .SDcols = cols,
                        by = .(household_code, panel_year, food, WOM)]
  householdChannel <- purch[, lapply(.SD, weighted.mean, w = realExp),
                            .SDcols = cols,
                            by = .(household_code, panel_year, food, channel_type)]
  householdChain <- purch[, lapply(.SD, weighted.mean, w = realExp),
                          .SDcols = cols,
                          by = .(household_code, panel_year, food, chainType)]

  # Combining
  discBehaviorAll <- rbindlist(list(discBehaviorAll, householdAvgAll),
                               use.names = TRUE)
  discBehaviorFood <- rbindlist(list(discBehaviorFood, householdAvgFood),
                                   use.names = TRUE)
  discBehaviorWOM <- rbindlist(list(discBehaviorWOM, householdWOM),
                               use.names = TRUE)
  discBehaviorChannel <- rbindlist(list(discBehaviorChannel, householdChannel),
                                   use.names = TRUE)
  discBehaviorChain <- rbindlist(list(discBehaviorChain, householdChain),
                                   use.names = TRUE)
}

# Saving
fwrite(discBehaviorAll, "/scratch/upenn/hossaine/discBehaviorAll.csv", nThread = threads)
fwrite(discBehaviorFood, "/scratch/upenn/hossaine/discBehaviorFood.csv", nThread = threads)
fwrite(discBehaviorWOM, "/scratch/upenn/hossaine/discBehaviorWOM.csv", nThread = threads)
fwrite(discBehaviorChannel, "/scratch/upenn/hossaine/discBehaviorChannel.csv", nThread = threads)
fwrite(discBehaviorChain, "/scratch/upenn/hossaine/discBehaviorChain.csv", nThread = threads)

# Adding demographics
discBehaviorAll <- fread("/scratch/upenn/hossaine/discBehaviorAll.csv", nThread = threads)
discBehaviorFood <- fread("/scratch/upenn/hossaine/discBehaviorFood.csv", nThread = threads)
discBehaviorWOM <- fread("/scratch/upenn/hossaine/discBehaviorWOM.csv", nThread = threads)
discBehaviorChannel <- fread("/scratch/upenn/hossaine/discBehaviorChannel.csv", nThread = threads)
discBehaviorChain <- fread("/scratch/upenn/hossaine/discBehaviorChain.csv", nThread = threads)

panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("panel_year", "household_code", "projection_factor",
                          "household_income", "household_size", "age", "child",
                          "fips", "household_income_coarse", "married",
                          "carShare", "law", "zip_code", "college", "men", "women",
                          "nChildren", "type_of_residence"),
               key = c("household_code", "panel_year"))
panel[, "adults" := men + women]
panel[, "household_income" := as.factor(household_income)]
panel[, "household_income_coarse" := as.factor(household_income_coarse)]
panel[, "lawInd" := (law >= 3)]

discBehaviorAll <- merge(discBehaviorAll, panel, by = c("household_code", "panel_year"))
discBehaviorFood <- merge(discBehaviorFood, panel, by = c("household_code", "panel_year"))
discBehaviorWOM <- merge(discBehaviorWOM, panel, by = c("household_code", "panel_year"))
discBehaviorChannel <- merge(discBehaviorChannel, panel, by = c("household_code", "panel_year"))
discBehaviorChain <- merge(discBehaviorChain, panel, by = c("household_code", "panel_year"))

# Identifying variation example
ex <- discBehaviorFood[men == 1 & women == 1 & nChildren == 2 & married == 1 &
                         college == 1 & type_of_residence == "Single-Family" &
                         panel_year == 2017]
ex[, "household_income" := factor(household_income,
                                  levels = c(4, 6, 8, 10, 11, 13, 15, 16, 17,
                                             18, 19, 21, 23, 26, 27),
                                  labels = c(6.5, 9, 11, 13.5, 17.5, 22.5, 27.5,
                                             32.5, 37.5, 42.5, 47.5, 55, 65, 85, 100),
                                  ordered = TRUE)]
ex[, "foodChar" := ifelse(food == 0, "Non-Food", "Food")]
ex[, "household_income" := as.numeric(as.character(household_income))]
ggplot(data = ex, aes(x = household_income, y = bulk, color = foodChar)) +
  geom_point(size = 1) +
  geom_jitter(width = 1) +
  geom_smooth(method = "lm") +
  facet_wrap(vars(foodChar)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  labs(x = "Annual Household Income ($000s)",
       y = "Annual Bulk Buying Share",
       color = "Product Type") +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom") +
  scale_color_grey()
  # scale_color_fivethirtyeight()

ggsave("./figures/savingsBehaviorVariation.pdf", height = 4, width = 6)
# ggsave("./figures/savingsBehaviorVariationColor.pdf", height = 4, width = 6)

# Histogram of bulk buying by income group
ggplot(data = discBehaviorFood, aes(x = bulk)) +
  geom_density() +
  facet_grid(rows = vars(household_income_coarse))

# Avg bulk buying by income
avgBulk <- discBehaviorFood[, .(bulk = weighted.mean(bulk, w = projection_factor)),
                            by = .(household_income, food)]
avgBulk[, "household_income" := factor(household_income,
                                       levels = c(4, 6, 8, 10, 11, 13, 15, 16, 17,
                                                  18, 19, 21, 23, 26, 27),
                                       labels = c(6.5, 9, 11, 13.5, 17.5, 22.5,
                                                  27.5, 32.5, 37.5, 42.5, 47.5,
                                                  55, 65, 85, 100),
                                       ordered = TRUE)]
avgBulk[, "foodChar" := ifelse(food == 0, "Non-Food", "Food")]
avgBulk[, "household_income" := as.numeric(as.character(household_income))]

fwrite(avgBulk, "./figures/savingsBehaviorAvgBulkRaw.csv")
avgBulk <- fread("./figures/savingsBehaviorAvgBulkRaw.csv")

ggplot(data = avgBulk, aes(x = household_income, y = bulk * 100, color = foodChar,
                           shape = foodChar)) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 35) +
  labs(x = "Annual Household Income ($000s)",
       y = "Annual Bulk Buying Share (%)",
       color = "Product Type",
       shape = "Product Type") +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom",
        text = element_text(size = 14),
        axis.ticks.length = unit(0.25, "cm")) +
  scale_color_fivethirtyeight()

ggsave("./figures/savingsBehaviorAvgBulkRaw.pdf", height = 4, width = 6)

# Bin-scatter'd version
discBehaviorFood[, "household_income_cts" :=
                   as.numeric(as.character(factor(household_income,
                          levels = c(4, 6, 8, 10, 11, 13, 15, 16, 17,
                                     18, 19, 21, 23, 26, 27),
                          labels = c(6.5, 9, 11, 13.5, 17.5, 22.5, 27.5, 32.5,
                                     37.5, 42.5, 47.5, 55, 65, 85, 100),
                          ordered = TRUE)))]
bin1 <- with(discBehaviorFood[food == 1],
             binsreg(y = bulk, x = household_income_cts,
                     w = cbind(married, age, adults, nChildren, carShare, college,
                               as.factor(panel_year), as.factor(type_of_residence),
                               as.factor(fips)),
                     weights = projection_factor))
bin1DT <- as.data.table(bin1$data.plot$`Group Full Sample`$data.dots)
bin1DT[, "food" := "Food"]
bin2 <- with(discBehaviorFood[food == 0],
             binsreg(y = bulk, x = household_income_cts,
                     w = cbind(married, age, adults, nChildren, carShare, college,
                               as.factor(panel_year), as.factor(type_of_residence),
                               as.factor(fips)),
                     weights = projection_factor))
bin2DT <- as.data.table(bin2$data.plot$`Group Full Sample`$data.dots)
bin2DT[, "food" := "Non-Food"]
graphData <- rbindlist(list(bin1DT, bin2DT), use.names = TRUE)

fwrite(graphData, "./figures/savingsBehaviorBinScatter.csv")
graphData <- fread("./figures/savingsBehaviorBinScatter.csv")

ggplot(data = graphData, aes(x = x, y = fit * 100, color = food, shape = food)) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 25) +
  labs(x = "Annual Household Income ($000s)",
       y = "Annual Bulk Buying Share (%)",
       color = "Product Type",
       shape = "Product Type") +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom",
        text = element_text(size = 14),
        axis.ticks.length = unit(0.25, "cm")) +
  scale_color_fivethirtyeight()

ggsave("./figures/savingsBehaviorBinScatter.pdf", height = 4, width = 6)

# Binscatter with only adults, kids, marital status
bin1 <- with(discBehaviorFood[food == 1],
             binsreg(y = bulk, x = household_income_cts,
                     w = cbind(married, adults, nChildren),
                     weights = projection_factor))
bin1DT <- as.data.table(bin1$data.plot$`Group Full Sample`$data.dots)
bin1DT[, "food" := "Food"]
bin2 <- with(discBehaviorFood[food == 0],
             binsreg(y = bulk, x = household_income_cts,
                     w = cbind(married, age, adults, nChildren),
                     weights = projection_factor))
bin2DT <- as.data.table(bin2$data.plot$`Group Full Sample`$data.dots)
bin2DT[, "food" := "Non-Food"]
graphData <- rbindlist(list(bin1DT, bin2DT), use.names = TRUE)

fwrite(graphData, "./figures/savingsBehaviorBinScatterSizeKids.csv")
graphData <- fread("./figures/savingsBehaviorBinScatterSizeKids.csv")

ggplot(data = graphData, aes(x = x, y = fit * 100, color = food, shape = food)) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 35) +
  labs(x = "Annual Household Income ($000s)",
       y = "Annual Bulk Buying Share (%)",
       color = "Product Type",
       shape = "Product Type") +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom",
        text = element_text(size = 14),
        axis.ticks.length = unit(0.25, "cm")) +
  scale_color_fivethirtyeight()

ggsave("./figures/savingsBehaviorBinScatterSizeKids.pdf", height = 4, width = 6)

# Running regressions
# If you add an interaction with panel year, you can test if there have been
# different trends over time, but nothing seemed significant or big enough
# to change the overall picture of increasing bulk with income.
print(discBehaviorAll[, weighted.mean(bulk, w = projection_factor),
                      keyby = household_income_coarse])
print(discBehaviorFood[food == 1, weighted.mean(bulk, w = projection_factor),
                       keyby = household_income_coarse])
print(discBehaviorFood[food == 0, weighted.mean(bulk, w = projection_factor),
                       keyby = household_income_coarse])

runRegAll <- function(y) {
  regForm <- as.formula(paste0(y, "~", "household_income + married + ",
                               "age + adults + nChildren + type_of_residence + ",
                               "carShare + college | fips + panel_year"))

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
fwrite(graphData, "./figures/savingsBehavior.csv")
graphData <- fread("./figures/savingsBehavior.csv")

panels <- c("Coupon", "Generic", "Bulk")
ggplot(data = graphData[disc %in% "Bulk" & `Product Type` != "All"],
       aes(x = rn, y = beta * 100, color = `Product Type`)) +
  geom_errorbar(aes(ymin = 100 * LCL,
                    ymax = 100 * UCL), width = 0.2) +
  geom_point(aes(shape = `Product Type`), size = 3) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  labs(x = "Annual Household Income ($000s)",
       y = "Difference in Bulk Purchasing\n(Percentage Points)") +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom",
        text = element_text(size = 14),
        axis.ticks.length = unit(0.25, "cm")) +
  scale_color_fivethirtyeight()

ggsave("./figures/savingsBehavior.pdf", height = 4, width = 6)

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
reg4 <- felm(formula = bulk ~ household_income + married + age + adults +
               nChildren,
             data = discBehaviorAll, weights = discBehaviorAll$projection_factor)
reg5 <- felm(formula = bulk ~ household_income + married + age + adults +
               nChildren + type_of_residence,
             data = discBehaviorAll, weights = discBehaviorAll$projection_factor)
reg6 <- felm(formula = bulk ~ household_income + married + age + adults +
               nChildren + type_of_residence + carShare,
             data = discBehaviorAll, weights = discBehaviorAll$projection_factor)
reg7 <- felm(formula = bulk ~ household_income + married + age + adults +
               nChildren + type_of_residence + carShare + college,
             data = discBehaviorAll, weights = discBehaviorAll$projection_factor)
reg8 <- felm(formula = bulk ~ household_income + married + age + adults +
               nChildren + type_of_residence + carShare + college | fips,
             data = discBehaviorAll, weights = discBehaviorAll$projection_factor)
reg9 <- felm(formula = bulk ~ household_income + married + age + adults +
               nChildren + type_of_residence + carShare + college | fips + panel_year,
             data = discBehaviorAll, weights = discBehaviorAll$projection_factor)
stargazer(reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8, reg9, type = "text", omit.stat = "ser")
# Average bulk purchasing, residual of marital status, age, composition,
# housing type, car share, and education
# (i.e. reg 7) is 28.6% for all products. Coarsening income bins gives 28.5%
# High income are an additional 2.2 pp above that.

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
reg4 <- felm(formula = bulk ~ household_income + married + age + adults +
               nChildren,
             data = discBehaviorFood[food == 1],
             weights = discBehaviorFood[food == 1]$projection_factor)
reg5 <- felm(formula = bulk ~ household_income + married + age + adults +
               nChildren + type_of_residence,
             data = discBehaviorFood[food == 1],
             weights = discBehaviorFood[food == 1]$projection_factor)
reg6 <- felm(formula = bulk ~ household_income + married + age + adults +
               nChildren + type_of_residence + carShare,
             data = discBehaviorFood[food == 1],
             weights = discBehaviorFood[food == 1]$projection_factor)
reg7f <- felm(formula = bulk ~ household_income + married + age + adults +
               nChildren + type_of_residence + carShare + college,
             data = discBehaviorFood[food == 1],
             weights = discBehaviorFood[food == 1]$projection_factor)
reg8 <- felm(formula = bulk ~ household_income + married + age + adults +
               nChildren + type_of_residence + carShare + college | fips,
             data = discBehaviorFood[food == 1],
             weights = discBehaviorFood[food == 1]$projection_factor)
reg9 <- felm(formula = bulk ~ household_income + married + age + adults +
               nChildren + type_of_residence + carShare + college | fips + panel_year,
             data = discBehaviorFood[food == 1],
             weights = discBehaviorFood[food == 1]$projection_factor)
stargazer(reg1, reg2, reg3, reg4, reg5, reg6, reg7f, reg8, reg9, type = "text",
          add.lines = list(c("Market FE's", "N", "N", "N", "N", "N", "N", "N", "Y", "Y"),
                           c("Year FE's",   "N", "N", "N", "N", "N", "N", "N", "N", "Y")),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          dep.var.caption = "", dep.var.labels.include = FALSE,
          omit = c("Constant"),
          covariate.labels = c("8-10k", "10-12k", "12-15k", "15-20k", "20-25k",
                               "25-30k", "30-35k", "35-40k", "40-45k", "45-50k",
                               "50-60k", "60-70k", "70-100k", ">100k", "Married",
                               "Age", "Adults", "Children", "Single-Family Home",
                               "% Car Access", "College"),
          notes.align = "l",
          notes.append = TRUE,
          digits = 3,
          notes = c("Source: Author calulations from Nielsen Consumer Panel."),
          label = "tab:discountingBehaviorFood",
          title = "Correlation of Bulk Buying and Demographics (Food Products)",
          out = "tables/AppendixDiscountingBehaviorFood.tex")
# Average bulk purchasing, residual of marital status, age, composition,
# housing, car share, and education
# (i.e. reg 7) is 27% for food products. High income are an additional 0.6 pp
# above that. Coarsening income bins gives 26.6% with 1pp above.


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
reg4 <- felm(formula = bulk ~ household_income + married + age + adults +
               nChildren,
             data = discBehaviorFood[food == 0],
             weights = discBehaviorFood[food == 0]$projection_factor)
reg5 <- felm(formula = bulk ~ household_income + married + age + adults +
               nChildren + type_of_residence,
             data = discBehaviorFood[food == 0],
             weights = discBehaviorFood[food == 0]$projection_factor)
reg6 <- felm(formula = bulk ~ household_income + married + age + adults +
               nChildren + type_of_residence + carShare,
             data = discBehaviorFood[food == 0],
             weights = discBehaviorFood[food == 0]$projection_factor)
reg7nf <- felm(formula = bulk ~ household_income + married + age + adults +
                nChildren + type_of_residence + carShare + college,
              data = discBehaviorFood[food == 0],
              weights = discBehaviorFood[food == 0]$projection_factor)
reg8 <- felm(formula = bulk ~ household_income + married + age + adults +
               nChildren + type_of_residence + carShare + college | fips,
             data = discBehaviorFood[food == 0],
             weights = discBehaviorFood[food == 0]$projection_factor)
reg9 <- felm(formula = bulk ~ household_income + married + age + adults +
               nChildren + type_of_residence + carShare + college | fips + panel_year,
             data = discBehaviorFood[food == 0],
             weights = discBehaviorFood[food == 0]$projection_factor)
stargazer(reg1, reg2, reg3, reg4, reg5, reg6, reg7nf, reg8, reg9, type = "text",
          add.lines = list(c("Market FE's", "N", "N", "N", "N", "N", "N", "N", "Y", "Y"),
                           c("Year FE's",   "N", "N", "N", "N", "N", "N", "N", "N", "Y")),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          dep.var.caption = "", dep.var.labels.include = FALSE,
          omit = c("Constant"),
          covariate.labels = c("8-10k", "10-12k", "12-15k", "15-20k", "20-25k",
                               "25-30k", "30-35k", "35-40k", "40-45k", "45-50k",
                               "50-60k", "60-70k", "70-100k", ">100k", "Married",
                               "Age", "Adults", "Children", "Single-Family Home",
                               "% Car Access", "College"),
          notes.align = "l",
          notes.append = TRUE,
          digits = 3,
          notes = c("Source: Author calulations from Nielsen Consumer Panel."),
          label = "tab:discountingBehaviorNonFood",
          title = "Correlation of Bulk Buying and Demographics (Non-Food Products)",
          out = "tables/AppendixDiscountingBehaviorNonFood.tex")
# Average bulk purchasing, residual of marital status, age, composition,
# house type, car share, and education
# (i.e. reg 7) is 33.4% for non-food products. High income are an additional 13 pp
# above that. Coarsening income bins gives 35.7% with 10.1pp above.

# Adding interactions just to check. Overall, there are some significant interactions,
# but the story is similar. The effects of other demographics are attenuated
# for richer households, which is likely because they are already buying
# more in bulk without the push of another household member to force that.
reg10 <- felm(formula = bulk ~ household_income * married + age +
                adults + nChildren +
                type_of_residence + carShare + college | fips + panel_year,
             data = discBehaviorFood[food == 0],
             weights = discBehaviorFood[food == 0]$projection_factor)
reg11 <- felm(formula = bulk ~ household_income * married + age +
                household_income * adults + household_income * nChildren +
                type_of_residence + carShare + college | fips + panel_year,
              data = discBehaviorFood[food == 0],
              weights = discBehaviorFood[food == 0]$projection_factor)
reg12 <- felm(formula = bulk ~ household_income * married + age +
                household_income * adults + household_income * nChildren +
                household_income * type_of_residence + carShare + college |
                fips + panel_year,
              data = discBehaviorFood[food == 0],
              weights = discBehaviorFood[food == 0]$projection_factor)
reg13 <- felm(formula = bulk ~ household_income * married + age +
                household_income * adults + household_income * nChildren +
                household_income * type_of_residence + household_income * carShare +
                college | fips + panel_year,
              data = discBehaviorFood[food == 0],
              weights = discBehaviorFood[food == 0]$projection_factor)
reg14 <- felm(formula = bulk ~ household_income * married + household_income * age +
                household_income * adults + household_income * nChildren +
                household_income * type_of_residence + household_income * carShare +
                college | fips + panel_year,
              data = discBehaviorFood[food == 0],
              weights = discBehaviorFood[food == 0]$projection_factor)
stargazer(reg9, reg10, reg11, reg12, reg13, reg14, type = "text")

# Plotting food and non food averages without market and year FE's, just for illustration
coefs7f <- as.data.table(summary(reg7f)$coefficients, keep.rownames = TRUE)
confInt7f <- as.data.table(confint(reg7f), keep.rownames = TRUE)
coefs7f <- merge(coefs7f, confInt7f, by = "rn")
coefs7f[, "reg" := "Food"]

coefs7nf <- as.data.table(summary(reg7nf)$coefficients, keep.rownames = TRUE)
confInt7nf <- as.data.table(confint(reg7nf), keep.rownames = TRUE)
coefs7nf <- merge(coefs7nf, confInt7nf, by = "rn")
coefs7nf[, "reg" := "Non-Food"]

# Organizing graph data
finalCoefs <- rbindlist(list(coefs7f, coefs7nf), use.names = TRUE)
graphData <- finalCoefs[grepl("(household_income*)|(Intercept)", rn)]
graphData[, "rn" := gsub("household_income", "", rn)]
graphData[, "rn" := gsub("\\(Intercept\\)", "4", rn)]
graphData[, "rn" := factor(rn, levels = c(4, 6, 8, 10, 11, 13, 15, 16, 17, 18, 19, 21, 23, 26, 27),
                           labels = c(6.5, 9, 11, 13.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 55, 65, 85, 100),
                           ordered = TRUE)]
graphData[, "rn" := as.numeric(as.character(rn))]
setnames(graphData, c("rn", "beta", "se", "t", "p", "LCL", "UCL", "Product Type"))
intFood <- graphData[rn == 6.5 & `Product Type` == "Food"]$beta
intNonFood <- graphData[rn == 6.5 & `Product Type` == "Non-Food"]$beta
graphData[rn > 6.5 & `Product Type` == "Food", "beta" := beta + intFood]
graphData[rn > 6.5 & `Product Type` == "Non-Food", "beta" := beta + intNonFood]

# Graphing
fwrite(graphData, "./figures/savingsBehaviorAvg.csv")
graphData <- fread("./figures/savingsBehaviorAvg.csv")

ggplot(data = graphData,
       aes(x = rn, y = beta * 100, color = `Product Type`)) +
  geom_point(aes(shape = `Product Type`), size = 3) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 25) +
  labs(x = "Annual Household Income ($000s)",
       y = "Average Bulk Share of Purchases (Percent)") +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom",
        text = element_text(size = 14),
        axis.ticks.length = unit(0.25, "cm")) +
  scale_color_fivethirtyeight()
ggsave(filename = "./figures/savingsBehaviorAvg.pdf", height = 4, width = 6)

# Robusness for over-the-month liquidity changes
discBehaviorWOM[, "WOM" := as.factor(WOM)]
discBehaviorWOM[, "firstWeek" := (WOM == 1)]
reg1 <- felm(bulk ~ household_income_coarse * WOM,
             data = discBehaviorWOM[food == 0], weights = discBehaviorWOM[food == 0]$projection_factor)
save(reg1, file = "/scratch/upenn/hossaine/reg1.rda", compress = TRUE)
rm(reg1)

reg2 <- felm(formula = bulk ~ household_income_coarse * WOM + married + age +
               adults + nChildren + type_of_residence + carShare + college,
             data = discBehaviorWOM[food == 0], weights = discBehaviorWOM[food == 0]$projection_factor)
save(reg2, file = "/scratch/upenn/hossaine/reg2.rda", compress = TRUE)
rm(reg2)

reg3 <- felm(formula = bulk ~ household_income_coarse * WOM + married + age +
               adults + nChildren + type_of_residence + carShare + college | fips,
             data = discBehaviorWOM[food == 0], weights = discBehaviorWOM[food == 0]$projection_factor)
save(reg3, file = "/scratch/upenn/hossaine/reg3.rda", compress = TRUE)
rm(reg3)

reg4 <- felm(formula = bulk ~ household_income_coarse * WOM + married + age + adults +
               nChildren + type_of_residence + carShare + college | fips + panel_year,
             data = discBehaviorWOM[food == 0], weights = discBehaviorWOM[food == 0]$projection_factor)
save(reg4, file = "/scratch/upenn/hossaine/reg4.rda", compress = TRUE)
rm(reg4)

reg5 <- felm(formula = bulk ~ WOM | household_code,
             data = discBehaviorWOM[food == 0], weights = discBehaviorWOM[food == 0]$projection_factor)
save(reg5, file = "/scratch/upenn/hossaine/reg5.rda", compress = TRUE)
rm(reg5)

reg6 <- felm(formula = bulk ~ household_income_coarse * WOM | household_code,
             data = discBehaviorWOM[food == 0], weights = discBehaviorWOM[food == 0]$projection_factor)
save(reg6, file = "/scratch/upenn/hossaine/reg6.rda", compress = TRUE)
rm(reg6)

reg7 <- felm(formula = bulk ~ household_income_coarse * WOM + married + age +
               adults + nChildren + type_of_residence + carShare +
               college | household_code,
             data = discBehaviorWOM[food == 0], weights = discBehaviorWOM[food == 0]$projection_factor)
save(reg7, file = "/scratch/upenn/hossaine/reg7.rda", compress = TRUE)
rm(reg7)

load("/scratch/upenn/hossaine/reg1.rda")
load("/scratch/upenn/hossaine/reg2.rda")
load("/scratch/upenn/hossaine/reg3.rda")
load("/scratch/upenn/hossaine/reg4.rda")
load("/scratch/upenn/hossaine/reg5.rda")
load("/scratch/upenn/hossaine/reg6.rda")
load("/scratch/upenn/hossaine/reg7.rda")
stargazer(reg1, reg2, reg3, reg4, reg5, reg6, reg7, type = "text", omit.stat = "ser")

# Average weekly bulk buying
meanBulk <- round(discBehaviorWOM[, weighted.mean(bulk, w = projection_factor)], 2)

stargazer(reg5, reg6, reg7, type = "text",
          add.lines = list(c("Mean Bulk", rep(meanBulk, 3)),
                           c("Household FE's", "Y", "Y", "Y"),
                           c("Demographics", "N", "N", "Y")),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          dep.var.caption = "", dep.var.labels.include = FALSE,
          keep = c("WOM", "household_income_coarse"),
          order = c(4, 5, 6, 2, 15, 18, 21, 3, 16, 19, 22, 1, 14, 17, 20),
          covariate.labels = c("Week 2", "Week 3", "Week 4",
                               "25-50k", "Week 2 : 25-50k", "Week 3 : 25-50k", "Week 4 : 25-50k",
                               "50-100k", "Week 2 : 50-100k", "Week 3 : 50-100k", "Week 4 : 50-100k",
                               ">100k", "Week 2 : >100k", "Week 3 : >100k", "Week 4 : >100k"),
          notes.align = "l",
          notes.append = TRUE,
          digits = 3,
          label = "tab:discountingBehaviorLiquidity",
          title = "Over-the-Month Changes in Bulk Buying",
          out = "tables/AppendixDiscountingBehaviorLiquidity.tex")


################################################################################
########### END ROBUSTNESS #####################################################
################################################################################
