# Computes household propensity for bulk discounting within store channels
library(data.table)
library(ggplot2)
library(ggthemes)
library(ggridges)
library(lfe)
library(purrr)
library(stringr)
library(stargazer)
threads <- 8
stores <- c("Grocery", "Discount Store", "Warehouse Club", "Dollar Store")

# Getting trips and panel data for demographics and to deflate expenditures
trips <- fread("/scratch/upenn/hossaine/fullTrips.csv", nThread = threads,
               select = c("trip_code_uc", "household_code", "panel_year",
                          "purchase_date", "retailer_code"))
trips[, c("yearMonth", "purchase_date") := .(substr(purchase_date, 1, 7), NULL)]
retailers <- fread("/scratch/upenn/hossaine/fullRetailers.csv")
trips <- merge(trips, retailers, by = "retailer_code")

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
                 select = c("trip_code_uc", "brand_code_uc", "product_module_code",
                            "food", "packagePrice", "quintile", "quantity"),
                 key = "trip_code_uc")
  purch[, ':=' (bulk = as.integer(quintile >= 4),
                totalExpenditure = packagePrice * quantity)]
  purch[, c("packagePrice", "quantity", "brand_code_uc") := NULL]

  # Deflating expenditures
  setkey(purch, trip_code_uc)
  purch <- merge(purch, fullData, by = "trip_code_uc")[, "trip_code_uc" := NULL]
  purch[, "realExp" := totalExpenditure / newIndex * 100]
  purch[, c("totalExpenditure", "newIndex") := NULL]

  # Getting household propensities by product type
  householdAvgAll <- purch[, .(bulk = weighted.mean(bulk, w = realExp)),
                           by = .(household_code, panel_year, channel_type)]
  householdAvgFood <- purch[, .(bulk = weighted.mean(bulk, w = realExp)),
                            by = .(household_code, panel_year, channel_type, food)]

  # Combining
  discBehaviorAll <- rbindlist(list(discBehaviorAll, householdAvgAll),
                               use.names = TRUE)
  discBehaviorFood <- rbindlist(list(discBehaviorFood, householdAvgFood),
                                use.names = TRUE)
}

# Saving
fwrite(discBehaviorAll, "/scratch/upenn/hossaine/discBehaviorAllChannel.csv", nThread = threads)
fwrite(discBehaviorFood, "/scratch/upenn/hossaine/discBehaviorFoodChannel.csv", nThread = threads)

# Adding demographics
discBehaviorAll <- fread("/scratch/upenn/hossaine/discBehaviorAllChannel.csv", nThread = threads)
discBehaviorFood <- fread("/scratch/upenn/hossaine/discBehaviorFoodChannel.csv", nThread = threads)

panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("panel_year", "household_code", "projection_factor",
                          "household_income", "household_size", "age", "child",
                          "dma_cd", "household_income_coarse", "married"),
               key = c("household_code", "panel_year"))
panel[, "household_income" := as.factor(household_income)]
panel[, "household_income_coarse" := as.factor(household_income_coarse)]

discBehaviorAll <- merge(discBehaviorAll, panel, by = c("household_code", "panel_year"))
discBehaviorFood <- merge(discBehaviorFood, panel, by = c("household_code", "panel_year"))

# Running regressions
# If you add an interaction with panel year, you can test if there have been
# different trends over time, but nothing seemed significant or big enough
# to change the overall picture of increasing bulk with income.
regData <- discBehaviorFood[food == 0]

reg1 <- felm(data = regData[channel_type == "Grocery"],
             bulk ~ household_income_coarse + married + age + household_size +
               child | dma_cd + panel_year)
reg2 <- felm(data = regData[channel_type == "Discount Store"],
             bulk ~ household_income_coarse + married + age + household_size +
               child | dma_cd + panel_year)
reg3 <- felm(data = regData[channel_type == "Warehouse Club"],
             bulk ~ household_income_coarse + married + age + household_size +
               child | dma_cd + panel_year)
reg4 <- felm(data = regData[channel_type == "Dollar Store"],
             bulk ~ household_income_coarse + married + age + household_size +
               child | dma_cd + panel_year)
stargazer(reg1, reg2, reg3, reg4, type = "text",
          add.lines = list(c("Market FE", "Y", "Y", "Y", "Y"),
                           c("Year FE", "Y", "Y", "Y", "Y")),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("Grocery", "Discount", "Warehouse", "Dollar"),
          column.separate = c(1, 1, 1, 1),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          keep = c("household_income*"),
          order = c(2, 3, 1),
          covariate.labels = c("25-50k", "50-100k", ">100k"),
          notes.align = "l",
          digits = 3,
          label = "tab:discountingWithinChannel",
          title = "Bulk Buying Gap Persists Within Channels")
,
          out = "tables/discountingWithinChannel.tex")

################################################################################
############# DROPPING WAREHOUSE CLUB SHOPPERS #################################
################################################################################
# Redoing discountingBehavior, but without warehouse club shoppers.
discBehaviorFood <- NULL
for (yr in 2004:2017) {
  print(yr)
  purch <- fread(paste0("/scratch/upenn/hossaine/fullPurch", yr, ".csv"),
                 nThread = threads,
                 select = c("trip_code_uc",
                            "brand_code_uc", "product_module_code", "food",
                            "packagePrice", "quintile", "quantity"),
                 key = "trip_code_uc")
  purch[, ':=' (bulk = as.integer(quintile >= 4),
                totalExpenditure = packagePrice * quantity)]
  purch[, c("packagePrice", "quantity", "brand_code_uc") := NULL]

  # Deflating expenditures
  setkey(purch, trip_code_uc)
  purch <- merge(purch, fullData,
                 by = "trip_code_uc")[, "trip_code_uc" := NULL]
  purch[, "realExp" := totalExpenditure / newIndex * 100]
  purch[, c("totalExpenditure", "newIndex") := NULL]

  # Dropping warehouse club shoppers
  purch[, "clubShopper" := (sum(channel_type == "Warehouse Club") > 0),
        by = .(household_code, panel_year)]
  #purch <- purch[channel_type != "Warehouse Club"]
  purch <- purch[clubShopper == FALSE]

  # Getting household propensities by product type
  householdAvgFood <- purch[, .(bulk = weighted.mean(bulk, w = realExp)),
                            by = .(household_code, panel_year, food)]

  # Combining
  discBehaviorFood <- rbindlist(list(discBehaviorFood, householdAvgFood),
                                use.names = TRUE)
}

# Saving
fwrite(discBehaviorFood, "/scratch/upenn/hossaine/discBehaviorFoodNoWarehouse.csv", nThread = threads)

# Adding demographics
discBehaviorFood <- fread("/scratch/upenn/hossaine/discBehaviorFoodNoWarehouse.csv", nThread = threads)

panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("panel_year", "household_code", "projection_factor",
                          "household_income", "household_size", "age", "child",
                          "dma_cd", "household_income_coarse", "married",
                          "carShare"),
               key = c("household_code", "panel_year"))
panel[, "household_income" := as.factor(household_income)]
panel[, "household_income_coarse" := as.factor(household_income_coarse)]

discBehaviorFood <- merge(discBehaviorFood, panel, by = c("household_code", "panel_year"))

# Running regressions
# If you add an interaction with panel year, you can test if there have been
# different trends over time, but nothing seemed significant or big enough
# to change the overall picture of increasing bulk with income.
runRegAll <- function(y) {
  regForm <- as.formula(paste0(y, "~", "household_income + married + ",
                               "age + household_size + child | dma_cd + panel_year"))

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
  coefs <- rbindlist(list(coefsFood, coefsNonFood), use.names = TRUE)
  return(coefs)
}

discounts <- c("bulk")
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
graphData[disc == "bulk", "disc" := "Bulk"]

# Graphing
ggplot(data = graphData[`Product Type` == "Non-Food"],
       aes(x = rn, y = beta * 100)) +
  geom_errorbar(aes(ymin = 100 * LCL,
                    ymax = 100 * UCL), width = 0.2) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  labs(x = "Household Income",
       y = "Difference in Bulk Purchasing (Percentage Points)") +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom") +
  scale_color_grey()
ggsave(filename = "./figures/discountingBehaviorNoWarehouse.png", height = 4, width = 6)
