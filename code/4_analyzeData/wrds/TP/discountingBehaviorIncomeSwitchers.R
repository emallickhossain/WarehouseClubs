# Gets discounting behavior for income switchers
# The main effects are really just from whatever bin the household ends up in,
# but not from whether they experienced an increase or decrease in their
# income. Basically, if you end up in the 50-100k bin, you are more likely
# to buy in bulk than if you are in the <25k or 25-50k bin.
# Running a regression of change in bulk share on change in income is not
# statistically significant. If anything, there might be a slight positive relationship
# but even then, it would be very small.
# Uses output from discountingBehavior.R
library(data.table)
library(ggplot2)
library(ggthemes)
library(ggridges)
library(lfe)
library(purrr)
library(stargazer)
threads <- 8

# Adding demographics
discBehaviorAll <- fread("/scratch/upenn/hossaine/discBehaviorAll.csv", nThread = threads)
discBehaviorFood <- fread("/scratch/upenn/hossaine/discBehaviorFood.csv", nThread = threads)

panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("panel_year", "household_code", "married",
                          "household_income", "household_size", "age", "child",
                          "dma_cd", "household_income_coarse", "zip_code"),
               key = c("household_code", "panel_year"))
panel[, "household_income" := as.factor(household_income)]

discBehaviorAll <- merge(discBehaviorAll, panel, by = c("household_code", "panel_year"))
discBehaviorFood <- merge(discBehaviorFood, panel, by = c("household_code", "panel_year"))

# Summary stats of switchers
switchers <- panel[, uniqueN(household_income), by = household_code]
round(prop.table(table(switchers$V1)), digits = 2)

# Making income continuous
discBehaviorAll[, "household_income_cts" :=
                  factor(household_income,
                         levels = c(3, 4, 6, 8, 10, 11, 13, 15, 16, 17, 18, 19,
                                    21, 23, 26, 27),
                         labels = c(2.5, 6.5, 9, 11, 13.5, 17.5, 22.5, 27.5, 32.5,
                                    37.5, 42.5, 47.5, 55, 65, 85, 100),
                         ordered = TRUE)]
discBehaviorAll[, "household_income_cts" := as.numeric(as.character(household_income_cts))]

# Generating fixed effects and income shift indicators
setorder(discBehaviorAll, household_code, panel_year)
discBehaviorAll[, "incChange" := household_income_cts -
                  shift(household_income_cts, 1, type = "lag"),
                by = household_code]
discBehaviorAll[incChange > 0, "incShift" := "up"]
discBehaviorAll[incChange < 0, "incShift" := "down"]

discBehaviorAll[, "bulkChange" := bulk -
                  shift(bulk, 1, type = "lag"),
                by = household_code]
discBehaviorAll[, "sizeChange" := household_size -
                  shift(household_size, 1, type = "lag"),
                by = household_code]
discBehaviorAll[, "ageChange" := age -
                  shift(age, 1, type = "lag"),
                by = household_code]
discBehaviorAll[, "marriedChange" := married -
                  shift(married, 1, type = "lag"),
                by = household_code]
discBehaviorAll[, "childChange" := child -
                  shift(child, 1, type = "lag"),
                by = household_code]
discBehaviorAll[, "logIncChange" := sign(incChange) * log(abs(incChange))]

# Running regressions
reg0 <- felm(bulkChange ~ household_income_coarse + sizeChange + ageChange + marriedChange + childChange,
             data = discBehaviorAll)
reg1 <- felm(bulkChange ~ incShift + sizeChange + ageChange + marriedChange + childChange,
             data = discBehaviorAll)
reg2 <- felm(bulkChange ~ incChange + sizeChange + ageChange + marriedChange + childChange,
             data = discBehaviorAll)
stargazer(reg1, reg2, type = "text")




# Trying for food and non-food items
# Making income continuous
discBehaviorFood[, "household_income_cts" :=
                  factor(household_income,
                         levels = c(3, 4, 6, 8, 10, 11, 13, 15, 16, 17, 18, 19,
                                    21, 23, 26, 27),
                         labels = c(2.5, 6.5, 9, 11, 13.5, 17.5, 22.5, 27.5, 32.5,
                                    37.5, 42.5, 47.5, 55, 65, 85, 100),
                         ordered = TRUE)]
discBehaviorFood[, "household_income_cts" := as.numeric(as.character(household_income_cts))]

# Generating fixed effects and income shift indicators
setorder(discBehaviorFood, household_code, food, panel_year)
discBehaviorFood[, "incChange" := household_income_cts -
                  shift(household_income_cts, 1, type = "lag"),
                by = .(household_code, food)]
discBehaviorFood[incChange > 0, "incShift" := "up"]
discBehaviorFood[incChange < 0, "incShift" := "down"]

discBehaviorFood[, "bulkChange" := bulk -
                  shift(bulk, 1, type = "lag"),
                by = .(household_code, food)]
discBehaviorFood[, "sizeChange" := household_size -
                  shift(household_size, 1, type = "lag"),
                by = .(household_code, food)]
discBehaviorFood[, "ageChange" := age -
                  shift(age, 1, type = "lag"),
                by = .(household_code, food)]
discBehaviorFood[, "marriedChange" := married -
                  shift(married, 1, type = "lag"),
                by = .(household_code, food)]
discBehaviorFood[, "childChange" := child -
                  shift(child, 1, type = "lag"),
                by = .(household_code, food)]
discBehaviorFood[, "logIncChange" := sign(incChange) * log(abs(incChange))]

# Running regressions
reg1 <- felm(bulkChange ~ incShift + sizeChange + ageChange + marriedChange + childChange,
             data = discBehaviorFood[food == 1])
reg2 <- felm(bulkChange ~ incChange + sizeChange + ageChange + marriedChange + childChange,
             data = discBehaviorFood[food == 1])
reg3 <- felm(bulkChange ~ incShift + sizeChange + ageChange + marriedChange + childChange,
             data = discBehaviorFood[food == 0])
reg4 <- felm(bulkChange ~ incChange + sizeChange + ageChange + marriedChange + childChange,
             data = discBehaviorFood[food == 0])
stargazer(reg1, reg2, reg3, reg4, type = "text")

