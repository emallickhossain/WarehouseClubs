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
discBehaviorModule <- NULL
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
  householdAvgModule <- purch[, lapply(.SD, weighted.mean, w = realExp),
                              .SDcols = cols,
                              by = .(household_code, panel_year,
                                     product_module_code, food)]

  # Combining
  discBehaviorModule <- rbindlist(list(discBehaviorModule, householdAvgModule),
                                  use.names = TRUE)
}

# Saving
fwrite(discBehaviorModule, "/scratch/upenn/hossaine/discBehaviorModule.csv",
       nThread = threads)

# Adding demographics
discBehaviorModule <- fread("/scratch/upenn/hossaine/discBehaviorModule.csv",
                            nThread = threads)
panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("panel_year", "household_code", "projection_factor",
                          "household_income", "age", "men", "women", "nChildren",
                          "dma_cd", "household_income_coarse", "married", "college"),
               key = c("household_code", "panel_year"))
panel[, "household_income" := as.factor(household_income)]

discBehaviorModule <- merge(discBehaviorModule, panel,
                            by = c("household_code", "panel_year"))

# Running regressions
# If you add an interaction with panel year, you can test if there have been
# different trends over time, but nothing seemed significant or big enough
# to change the overall picture of increasing bulk with income.
runRegAll <- function(y, module) {
  regForm <- as.formula(paste0(y, "~", "household_income + age + men + women + ",
                               "nChildren + married + college | dma_cd + panel_year"))

  # All products
  regData <- discBehaviorModule[product_module_code == module]
  regAll <- felm(data = regData, formula = regForm, weights = regData$projection_factor)
  coefsAll <- as.data.table(summary(regAll)$coefficients, keep.rownames = TRUE)
  confIntAll <- as.data.table(confint(regAll), keep.rownames = TRUE)
  coefsAll <- merge(coefsAll, confIntAll, by = "rn")
  coefsAll[, c("discount", "module") := .(y, module)]
  return(coefsAll)
}

discounts <- c("bulk")
modules <- c(7260, 7734, 8444, 3625, 4100)
toRun <- expand.grid(y = discounts, module = modules)
finalCoefs <- rbindlist(map2(toRun$y, toRun$module, runRegAll), use.names = TRUE)

# Adding in module names
finalCoefs[module == 7260, "Product" := "Toilet Paper"]
finalCoefs[module == 7734, "Product" := "Paper Towels"]
finalCoefs[module == 8444, "Product" := "Diapers"]
finalCoefs[module == 7270, "Product" := "Tampons"]
finalCoefs[module == 3625, "Product" := "Milk"]
finalCoefs[module == 4100, "Product" := "Eggs"]

# Organizing graph data
graphData <- finalCoefs[grepl("household_income", rn)]
graphData[, "rn" := gsub("household_income", "", rn)]
graphData[, "rn" := factor(rn, levels = c(4, 6, 8, 10, 11, 13, 15, 16, 17, 18, 19, 21, 23, 26, 27),
                           labels = c(6.5, 9, 11, 13.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 55, 65, 85, 100),
                           ordered = TRUE)]
graphData[, "rn" := as.numeric(as.character(rn))]
setnames(graphData, c("rn", "beta", "se", "t", "p", "LCL", "UCL", "disc",
                      "module", "Product"))

# Housekeeping
graphData[, "food" := ifelse(Product %in% c("Milk", "Eggs"), "Food", "Non-Food")]

# Graphing
ggplot(data = graphData,
       aes(x = rn, y = beta * 100, color = Product)) +
  geom_errorbar(aes(ymin = 100 * LCL,
                    ymax = 100 * UCL), width = 0.2) +
  geom_point(aes(shape = Product), size = 3) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  facet_wrap(vars(food), scales = "fixed") +
  labs(x = "Household Income",
       y = "Difference in Bulk Purchasing (Percentage Points)") +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom") +
  scale_shape_manual(values = c(7, 15:19)) +
  scale_color_grey()

ggsave("./figures/savingsBehaviorModules.png", height = 4, width = 6)

################################################################################
########### ROBUSTNESS #########################################################
################################################################################
#Redoing regressions slowly dropping in covariates and looking at coefficient changes
# 7260: Toilet Paper
reg1 <- felm(formula = bulk ~ household_income,
             data = discBehaviorModule[product_module_code == 7260],
             weights = discBehaviorModule[product_module_code == 7260]$projection_factor)
reg2 <- felm(formula = bulk ~ household_income + married,
             data = discBehaviorModule[product_module_code == 7260],
             weights = discBehaviorModule[product_module_code == 7260]$projection_factor)
reg3 <- felm(formula = bulk ~ household_income + married + age,
             data = discBehaviorModule[product_module_code == 7260],
             weights = discBehaviorModule[product_module_code == 7260]$projection_factor)
reg4 <- felm(formula = bulk ~ household_income + married + age + men + women +
               nChildren,
             data = discBehaviorModule[product_module_code == 7260],
             weights = discBehaviorModule[product_module_code == 7260]$projection_factor)
reg5 <- felm(formula = bulk ~ household_income + married + age + men + women +
               nChildren + college,
             data = discBehaviorModule[product_module_code == 7260],
             weights = discBehaviorModule[product_module_code == 7260]$projection_factor)
reg6 <- felm(formula = bulk ~ household_income + married + age + men + women +
               nChildren + college | dma_cd | 0 | dma_cd,
             data = discBehaviorModule[product_module_code == 7260],
             weights = discBehaviorModule[product_module_code == 7260]$projection_factor)
reg7 <- felm(formula = bulk ~ household_income + married + age + men + women +
               nChildren + college | dma_cd + panel_year | 0 | dma_cd,
             data = discBehaviorModule[product_module_code == 7260],
             weights = discBehaviorModule[product_module_code == 7260]$projection_factor)
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
          label = "tab:discountingBehavior7260",
          title = "Correlation of Bulk Buying and Demographics (Toilet Paper)",
          out = "tables/AppendixDiscountingBehavior7260.tex")

# 7734: Paper Towels
reg1 <- felm(formula = bulk ~ household_income,
             data = discBehaviorModule[product_module_code == 7734],
             weights = discBehaviorModule[product_module_code == 7734]$projection_factor)
reg2 <- felm(formula = bulk ~ household_income + married,
             data = discBehaviorModule[product_module_code == 7734],
             weights = discBehaviorModule[product_module_code == 7734]$projection_factor)
reg3 <- felm(formula = bulk ~ household_income + married + age,
             data = discBehaviorModule[product_module_code == 7734],
             weights = discBehaviorModule[product_module_code == 7734]$projection_factor)
reg4 <- felm(formula = bulk ~ household_income + married + age + men + women +
               nChildren,
             data = discBehaviorModule[product_module_code == 7734],
             weights = discBehaviorModule[product_module_code == 7734]$projection_factor)
reg5 <- felm(formula = bulk ~ household_income + married + age + men + women +
               nChildren + college,
             data = discBehaviorModule[product_module_code == 7734],
             weights = discBehaviorModule[product_module_code == 7734]$projection_factor)
reg6 <- felm(formula = bulk ~ household_income + married + age + men + women +
               nChildren + college | dma_cd | 0 | dma_cd,
             data = discBehaviorModule[product_module_code == 7734],
             weights = discBehaviorModule[product_module_code == 7734]$projection_factor)
reg7 <- felm(formula = bulk ~ household_income + married + age + men + women +
               nChildren + college | dma_cd + panel_year | 0 | dma_cd,
             data = discBehaviorModule[product_module_code == 7734],
             weights = discBehaviorModule[product_module_code == 7734]$projection_factor)
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
          label = "tab:discountingBehavior7734",
          title = "Correlation of Bulk Buying and Demographics (Paper Towels)",
          out = "tables/AppendixDiscountingBehavior7734.tex")


# 8444: Diapers
reg1 <- felm(formula = bulk ~ household_income,
             data = discBehaviorModule[product_module_code == 8444],
             weights = discBehaviorModule[product_module_code == 8444]$projection_factor)
reg2 <- felm(formula = bulk ~ household_income + married,
             data = discBehaviorModule[product_module_code == 8444],
             weights = discBehaviorModule[product_module_code == 8444]$projection_factor)
reg3 <- felm(formula = bulk ~ household_income + married + age,
             data = discBehaviorModule[product_module_code == 8444],
             weights = discBehaviorModule[product_module_code == 8444]$projection_factor)
reg4 <- felm(formula = bulk ~ household_income + married + age + men + women +
               nChildren,
             data = discBehaviorModule[product_module_code == 8444],
             weights = discBehaviorModule[product_module_code == 8444]$projection_factor)
reg5 <- felm(formula = bulk ~ household_income + married + age + men + women +
               nChildren + college,
             data = discBehaviorModule[product_module_code == 8444],
             weights = discBehaviorModule[product_module_code == 8444]$projection_factor)
reg6 <- felm(formula = bulk ~ household_income + married + age + men + women +
               nChildren + college | dma_cd | 0 | dma_cd,
             data = discBehaviorModule[product_module_code == 8444],
             weights = discBehaviorModule[product_module_code == 8444]$projection_factor)
reg7 <- felm(formula = bulk ~ household_income + married + age + men + women +
               nChildren + college | dma_cd + panel_year | 0 | dma_cd,
             data = discBehaviorModule[product_module_code == 8444],
             weights = discBehaviorModule[product_module_code == 8444]$projection_factor)
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
          label = "tab:discountingBehavior8444",
          title = "Correlation of Bulk Buying and Demographics (Diapers)",
          out = "tables/AppendixDiscountingBehavior8444.tex")


# 7270: Tampons
reg1 <- felm(formula = bulk ~ household_income,
             data = discBehaviorModule[product_module_code == 7270],
             weights = discBehaviorModule[product_module_code == 7270]$projection_factor)
reg2 <- felm(formula = bulk ~ household_income + married,
             data = discBehaviorModule[product_module_code == 7270],
             weights = discBehaviorModule[product_module_code == 7270]$projection_factor)
reg3 <- felm(formula = bulk ~ household_income + married + age,
             data = discBehaviorModule[product_module_code == 7270],
             weights = discBehaviorModule[product_module_code == 7270]$projection_factor)
reg4 <- felm(formula = bulk ~ household_income + married + age + men + women +
               nChildren,
             data = discBehaviorModule[product_module_code == 7270],
             weights = discBehaviorModule[product_module_code == 7270]$projection_factor)
reg5 <- felm(formula = bulk ~ household_income + married + age + men + women +
               nChildren + college,
             data = discBehaviorModule[product_module_code == 7270],
             weights = discBehaviorModule[product_module_code == 7270]$projection_factor)
reg6 <- felm(formula = bulk ~ household_income + married + age + men + women +
               nChildren + college | dma_cd | 0 | dma_cd,
             data = discBehaviorModule[product_module_code == 7270],
             weights = discBehaviorModule[product_module_code == 7270]$projection_factor)
reg7 <- felm(formula = bulk ~ household_income + married + age + men + women +
               nChildren + college | dma_cd + panel_year | 0 | dma_cd,
             data = discBehaviorModule[product_module_code == 7270],
             weights = discBehaviorModule[product_module_code == 7270]$projection_factor)
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
          label = "tab:discountingBehavior7270",
          title = "Correlation of Bulk Buying and Demographics (Tampons)",
          out = "tables/AppendixDiscountingBehavior7270.tex")


# 3625: Milk
reg1 <- felm(formula = bulk ~ household_income,
             data = discBehaviorModule[product_module_code == 3625],
             weights = discBehaviorModule[product_module_code == 3625]$projection_factor)
reg2 <- felm(formula = bulk ~ household_income + married,
             data = discBehaviorModule[product_module_code == 3625],
             weights = discBehaviorModule[product_module_code == 3625]$projection_factor)
reg3 <- felm(formula = bulk ~ household_income + married + age,
             data = discBehaviorModule[product_module_code == 3625],
             weights = discBehaviorModule[product_module_code == 3625]$projection_factor)
reg4 <- felm(formula = bulk ~ household_income + married + age + men + women +
               nChildren,
             data = discBehaviorModule[product_module_code == 3625],
             weights = discBehaviorModule[product_module_code == 3625]$projection_factor)
reg5 <- felm(formula = bulk ~ household_income + married + age + men + women +
               nChildren + college,
             data = discBehaviorModule[product_module_code == 3625],
             weights = discBehaviorModule[product_module_code == 3625]$projection_factor)
reg6 <- felm(formula = bulk ~ household_income + married + age + men + women +
               nChildren + college | dma_cd | 0 | dma_cd,
             data = discBehaviorModule[product_module_code == 3625],
             weights = discBehaviorModule[product_module_code == 3625]$projection_factor)
reg7 <- felm(formula = bulk ~ household_income + married + age + men + women +
               nChildren + college | dma_cd + panel_year | 0 | dma_cd,
             data = discBehaviorModule[product_module_code == 3625],
             weights = discBehaviorModule[product_module_code == 3625]$projection_factor)
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
          label = "tab:discountingBehavior3625",
          title = "Correlation of Bulk Buying and Demographics (Milk)",
          out = "tables/AppendixDiscountingBehavior3625.tex")


# 4100: Eggs
reg1 <- felm(formula = bulk ~ household_income,
             data = discBehaviorModule[product_module_code == 4100],
             weights = discBehaviorModule[product_module_code == 4100]$projection_factor)
reg2 <- felm(formula = bulk ~ household_income + married,
             data = discBehaviorModule[product_module_code == 4100],
             weights = discBehaviorModule[product_module_code == 4100]$projection_factor)
reg3 <- felm(formula = bulk ~ household_income + married + age,
             data = discBehaviorModule[product_module_code == 4100],
             weights = discBehaviorModule[product_module_code == 4100]$projection_factor)
reg4 <- felm(formula = bulk ~ household_income + married + age + men + women +
               nChildren,
             data = discBehaviorModule[product_module_code == 4100],
             weights = discBehaviorModule[product_module_code == 4100]$projection_factor)
reg5 <- felm(formula = bulk ~ household_income + married + age + men + women +
               nChildren + college,
             data = discBehaviorModule[product_module_code == 4100],
             weights = discBehaviorModule[product_module_code == 4100]$projection_factor)
reg6 <- felm(formula = bulk ~ household_income + married + age + men + women +
               nChildren + college | dma_cd | 0 | dma_cd,
             data = discBehaviorModule[product_module_code == 4100],
             weights = discBehaviorModule[product_module_code == 4100]$projection_factor)
reg7 <- felm(formula = bulk ~ household_income + married + age + men + women +
               nChildren + college | dma_cd + panel_year | 0 | dma_cd,
             data = discBehaviorModule[product_module_code == 4100],
             weights = discBehaviorModule[product_module_code == 4100]$projection_factor)
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
          label = "tab:discountingBehavior4100",
          title = "Correlation of Bulk Buying and Demographics (Eggs)",
          out = "tables/AppendixDiscountingBehavior4100.tex")
