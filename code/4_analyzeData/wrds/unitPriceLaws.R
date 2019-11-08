# Examines effects of unit price regulations on bulk buying
library(data.table)
library(ggplot2)
library(ggthemes)
library(ggridges)
library(lfe)
library(purrr)
library(stringr)
library(stargazer)
library(forcats)
library(Hmisc)
library(knitr)
library(zoo)
threads <- 8

# Making map of unit price laws in 2017
states_map <- setDT(map_data("state"))
graphData <- fread("./code/0_data/nist130.csv",
                   select = c("state", "type", "displayRequirements"))
graphData[type == "", "reg" := "No Reg"]
graphData[type == "voluntary", "reg" := "Vol. Disp"]
graphData[type == "mandatory" & displayRequirements == "No", "reg" := "Mand. Disp"]
graphData[type == "mandatory" & displayRequirements == "Yes", "reg" := "Mand. Disp, Strict"]
graphData[, "reg" := factor(reg, levels = c("Mand. Disp, Strict", "Mand. Disp",
                                            "Vol. Disp", "No Reg"),
                            ordered = TRUE)]
stateRef <- data.table(state = state.abb, region = tolower(state.name))
graphData <- merge(graphData, stateRef, by = "state")

states_map <- merge(states_map, graphData, by = "region")

ggplot(states_map, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = reg), color = "gray") +
  labs(fill = "Regulation Status") +
  theme_tufte() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom") +
  # scale_fill_grey(start = 0.5, end = 0.95)
  scale_fill_brewer(direction = -1)
ggsave(filename = "./code/5_figures/unitPriceLaws.pdf", height = 4, width = 6)

# Loading panel data
panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("panel_year", "household_code", "household_income",
                          "men", "women", "age", "nChildren", "fips", "projection_factor",
                          "household_income_coarse", "married", "carShare",
                          "law", "college", "type_of_residence"),
               key = c("household_code", "panel_year"))
panel[, "fips" := str_pad(fips, width = 5, side = "left", pad = "0")]
panel[, "state" := as.integer(substr(fips, 1, 2))]
panel[, "household_income" := factor(household_income,
                                     levels = c(3, 4, 6, 8, 10, 11, 13, 15, 16,
                                                17, 18, 19, 21, 23, 26, 27),
                                     labels = c(2.5, 6.5, 9, 11, 13.5, 17.5,
                                                22.5, 27.5, 32.5, 37.5, 42.5,
                                                47.5, 55, 65, 85, 100),
                                     ordered = TRUE)]
panel[, "household_size" := men + women + nChildren]
panel[, "child" := as.integer(nChildren > 0)]
panel[, "household_income" := as.numeric(as.character(household_income))]
panel[, "household_income_coarse" := as.factor(household_income_coarse)]
panel[, "adult" := men + women]

# Coding law types
panel[, "lawInd" := (law >= 3)]
panel[lawInd == FALSE, "mandatory" := "None"]
panel[lawInd == TRUE, "mandatory" := ifelse(state %in%
                                              c(9, 11, 24, 25, 33, 34, 36, 41, 44, 50),
                                            "Mandatory", "Voluntary")]
panel[lawInd == FALSE, "display" := "None"]
panel[lawInd == TRUE, "display" := mandatory]
panel[lawInd == TRUE & state %in% c(6, 9, 25, 34, 36, 44), "display" := "Display"]
panel[, "mandatory" := relevel(as.factor(mandatory), ref = "None")]
panel[, "display" := relevel(as.factor(display), ref = "None")]

# Getting summary stats for laws with and without regulations
cols <- c("household_income", "household_size", "child", "married", "college", "age")
summaryMeans <- panel[, lapply(.SD, weighted.mean, w = projection_factor),
                      .SDcols = cols, by = lawInd]
summaryMeans[, "type" := "Mean"]
summarySD <- panel[, lapply(.SD, function(x, w) sqrt(wtd.var(x, weights = w)),
                            w = projection_factor),
                   .SDcols = cols, by = lawInd]
summarySD[, "type" := "SD"]

# Assembling final table
finalTable <- rbindlist(list(summaryMeans, summarySD), use.names = TRUE)
finalTableLong <- melt(finalTable, id.vars = c("type", "lawInd"))
finalTableWide <- dcast(finalTableLong, variable ~ type + lawInd)
setnames(finalTableWide, "variable", "Variable")
setcolorder(finalTableWide, c("Variable", "Mean_FALSE", "SD_FALSE",
                              "Mean_TRUE", "SD_TRUE"))
finalTableWide[Variable == "household_income", "Variable" := "Household income ($000s)"]
finalTableWide[Variable == "household_size", "Variable" := "Household size"]
finalTableWide[Variable == "age", "Variable" := "Age"]
finalTableWide[Variable == "college", "Variable" := "College Educated"]
finalTableWide[Variable == "child", "Variable" := "Child Present"]
finalTableWide[Variable == "married", "Variable" := "Married"]

kable(finalTableWide, digits = 2, format = "markdown")
# Testing for differences
t.test(household_income ~ lawInd, data = panel, conf.level = 0.95)
t.test(household_size ~ lawInd, data = panel, conf.level = 0.95)
t.test(age ~ lawInd, data = panel, conf.level = 0.95)
t.test(college ~ lawInd, data = panel, conf.level = 0.95)
t.test(child ~ lawInd, data = panel, conf.level = 0.95)
t.test(married ~ lawInd, data = panel, conf.level = 0.95)
# Saved in summaryStatsUnitLaw.tex

# Getting data from discountingBehavior.R
# Adding demographics
discBehaviorNonFood <- fread("/scratch/upenn/hossaine/discBehaviorFood.csv",
                             nThread = threads)[food == 0]
discBehaviorNonFood <- merge(discBehaviorNonFood, panel,
                             by = c("household_code", "panel_year"))
discBehaviorNonFood[state == 6, "display" := "Voluntary"]
discBehaviorNonFood[, c("food", "coupon", "generic", "none", "one", "two", "three",
                        "men", "women", "fips") := NULL]

# Cross section
reg1 <- felm(bulk ~ lawInd | 0 | 0 | state,
             data = discBehaviorNonFood)
reg2 <- felm(bulk ~ lawInd + household_income_coarse + adult + nChildren +
               age + carShare + type_of_residence + college + married | 0 | 0 | state,
             data = discBehaviorNonFood)
# reg3 <- felm(bulk ~ lawInd * household_income_coarse + adult + nChildren +
#                age + carShare + type_of_residence + college + married | 0 | 0 | state,
#              data = discBehaviorNonFood)
reg4 <- felm(bulk ~ display + adult + nChildren +
               age + carShare + type_of_residence + college + married | 0 | 0 | state,
             data = discBehaviorNonFood)
reg5 <- felm(bulk ~ display + adult + nChildren +
               age + carShare + type_of_residence + college + married | 0 | 0 | state,
             data = discBehaviorNonFood[state != 6])
stargazer(reg1, reg2, reg4, reg5, type = "text",
          add.lines = list(c("Demographics", "N", "Y", "Y", "Y"),
                           c("Omit California", "N", "N", "N", "Y")),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          dep.var.caption = "", dep.var.labels.include = FALSE,
          keep = c("lawInd*", "mandatory*", "display*", "voluntary*"),
          order = c(1, 16, 17, 15, 7, 6, 5),
          covariate.labels = c("Regulation", "Vol. Disp",
                               "Mand. Disp", "Mand. Disp, Strict"),
          notes.align = "l",
          notes.append = TRUE,
          digits = 3,
          out = "tables/unitPriceLaw.tex")

# Summary stats of movers
panel[, "hhLaw" := paste(household_code, lawInd, sep = "_")]
moveTally <- panel[, uniqueN(hhLaw), by = household_code]
table(moveTally$V1)
ids <- moveTally[V1 > 1]$household_code
panel[, "mover" := ifelse(household_code %in% ids, TRUE, FALSE)]

# All movers transition matrix
setorder(panel, household_code, panel_year)
panel[, "lagLaw" := shift(lawInd, n = 1, type = "lag"), by = household_code]
panel[, "lagZip" := shift(zip_code, n = 1, type = "lag"), by = household_code]
panel[, "moveDirection" := paste(lagLaw, lawInd, sep = "_")]
table(panel[!is.na(lagLaw) & zip_code != lagZip]$moveDirection)
# Saving in moverTransition.tex

# Getting summary stats for laws with and without regulations
cols <- c("household_income", "household_size", "child", "married", "college", "age")
summaryMeans <- panel[, lapply(.SD, mean), .SDcols = cols, by = mover]
summaryMeans[, "type" := "Mean"]
summarySD <- panel[, lapply(.SD, sd), .SDcols = cols, by = mover]
summarySD[, "type" := "SD"]

# Assembling final table
finalTable <- rbindlist(list(summaryMeans, summarySD), use.names = TRUE)
finalTableLong <- melt(finalTable, id.vars = c("mover", "type"))
finalTableWide <- dcast(finalTableLong, variable ~ mover + type)
setnames(finalTableWide, "variable", "Variable")
finalTableWide[Variable == "household_income", "Variable" := "Household income ($000s)"]
finalTableWide[Variable == "household_size", "Variable" := "Household size"]
finalTableWide[Variable == "age", "Variable" := "Age"]
finalTableWide[Variable == "college", "Variable" := "College Educated"]
finalTableWide[Variable == "child", "Variable" := "Child Present"]
finalTableWide[Variable == "married", "Variable" := "Married"]

kable(finalTableWide, digits = 2, format = "markdown")
# Saved in summaryStatsUnitLawMovers.tex

# Determining move direction (to regulations or away from regulations)
discBehaviorNonFood[, "lawCount" := uniqueN(lawInd), by = household_code]
discBehaviorNonFood[, "mover" := (lawCount == 2)]
setorder(discBehaviorNonFood, household_code, panel_year)
discBehaviorNonFood[, "lagLaw" := shift(lawInd, 1, type = "lag"),
                    by = .(household_code)]
discBehaviorNonFood[, "move" := lawInd - lagLaw]

# Classifying movers
movers <- discBehaviorNonFood[mover == TRUE]
movers[is.na(move), "move" := 0]
movers <- movers[, .(type = max(move)), by = household_code]
discBehaviorNonFood <- merge(discBehaviorNonFood, movers, by = "household_code", all = TRUE)
discBehaviorNonFood[is.na(type), "moverType" := "No Move"]
discBehaviorNonFood[type == 0, "moverType" := "Move To No Reg"]
discBehaviorNonFood[type == 1, "moverType" := "Move To Reg"]
discBehaviorNonFood[, "move" := relevel(as.factor(move), ref = "0")]

# Getting regression table
# Movers (symmetric effects, no difference between move to and move away from regs)
reg1 <- felm(bulk ~ lawInd | household_code + panel_year | 0 | household_code,
             data = discBehaviorNonFood)
reg2 <- felm(bulk ~ lawInd + household_income_coarse + adult + nChildren +
               married + carShare + type_of_residence + college + age |
               household_code + panel_year | 0 | household_code,
             data = discBehaviorNonFood)
reg3 <- felm(bulk ~ lawInd + household_income_coarse + adult + nChildren +
               married + carShare + type_of_residence + college + age |
               household_code + panel_year | 0 | household_code,
             data = discBehaviorNonFood[moverType != "Move To No Reg"])
reg4 <- felm(bulk ~ lawInd + household_income_coarse + adult + nChildren +
               married + carShare + type_of_residence + college + age |
               household_code + panel_year | 0 | household_code,
             data = discBehaviorNonFood[moverType != "Move To Reg"])
stargazer(reg1, reg2, reg3, reg4, type = "text",
          add.lines = list(c("Household FE", "Y", "Y", "Y", "Y"),
                           c("Year FE", "Y", "Y", "Y", "Y"),
                           c("Demographics", "N", "Y", "Y", "Y")),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          dep.var.caption = "", dep.var.labels.include = FALSE,
          column.separate = c(2, 1, 1),
          column.labels = c("All Households", "No Law To Law", "Law To No Law"),
          keep = c("lawInd"),
          covariate.labels = c("Regulation"),
          notes.align = "l",
          digits = 3,
          out = "tables/unitPriceLawMovers.tex")

# Generating event study.
moveYear <- discBehaviorNonFood[move != "0", .(moveYear = min(panel_year)), by = household_code]
discBehaviorNonFood <- merge(discBehaviorNonFood, moveYear, by = "household_code", all = TRUE)
discBehaviorNonFood[, "Y" := panel_year - moveYear]
discBehaviorNonFood[is.na(Y), "Y" := -1]
discBehaviorNonFood[, "Y" := relevel(as.factor(Y), ref = "-1")]
discBehaviorNonFood[, "moveLags" := paste(moverType, Y, sep = "_")]
discBehaviorNonFood[moveLags == "Move To Reg_-1", "moveLags" := "No Move_-1"]
discBehaviorNonFood[moveLags == "Move To No Reg_-1", "moveLags" := "No Move_-1"]
discBehaviorNonFood[, "moveLags" := relevel(as.factor(moveLags), ref = "No Move_-1")]

regEvent1 <- felm(bulk ~ moveLags | household_code + panel_year |
                    0 | household_code,
                  data = discBehaviorNonFood)
regEvent2 <- felm(bulk ~ moveLags + household_income_coarse + adult +
                    nChildren + age + married + college |
                    household_code + panel_year | 0 | household_code,
                  data = discBehaviorNonFood)
stargazer(regEvent1, regEvent2, type = "text")

finalCoefs <- as.data.table(summary(regEvent2)$coefficients, keep.rownames = TRUE)
finalInt <- as.data.table(confint(regEvent2), keep.rownames = TRUE)
finalCoefs <- merge(finalCoefs, finalInt, by = "rn")
finalCoefs <- finalCoefs[grepl("move", rn)]
finalCoefs[, c("direction", "Year") := tstrsplit(rn, "_", fixed = TRUE)]
finalCoefs[direction == "moveLagsMove To Reg", direction := "To Law"]
finalCoefs[direction == "moveLagsMove To No Reg", direction := "To No Law"]
finalCoefs[, "Year" := as.integer(Year)]
finalCoefs[, "rn" := NULL]

setnames(finalCoefs, c("beta", "se", "t", "p", "LCL", "UCL", "direction", "Year"))
finalCoefs <- rbindlist(list(finalCoefs,
                             list(0, 0, 0, 0, 0, 0, "To Law", -1),
                             list(0, 0, 0, 0, 0, 0, "To No Law", -1)))

pd <- position_dodge(width = 0.4)
ggplot(data = finalCoefs[Year %in% -3:2],
       aes(x = Year, y = beta, color = direction, shape = direction)) +
  geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.2, position = pd) +
  geom_point(position = pd, size = 3) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  labs(x = "Years After Moving",
       y = "Difference in Bulk Purchasing\n(Percentage Points)",
       color = "Law Status After Household Move",
       shape = "Law Status After Household Move") +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom",
        text = element_text(size = 14),
        axis.ticks.length = unit(0.25, "cm")) +
  scale_color_fivethirtyeight()

ggsave(filename = "./figures/unitPriceEventStudyColor.pdf", height = 4, width = 6)
