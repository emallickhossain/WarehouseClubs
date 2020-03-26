# Examines effects of unit price regulations on bulk buying
library(data.table)
library(ggplot2)
library(ggthemes)
library(lfe)
library(purrr)
library(stringr)
library(stargazer)
library(forcats)
library(Hmisc)
library(knitr)
library(zoo)
threads <- 8

# Loading panel data
panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("panel_year", "household_code", "household_income",
                          "men", "women", "age", "nChildren", "fips", "projection_factor",
                          "household_income_coarse", "married", "carShare",
                          "law", "college", "type_of_residence", "zip_code"),
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

# Getting years when households move for those that move once
panel[, "nMoves" := uniqueN(zip_code) - 1, by = household_code]
nMoves <- panel[, .(moves = uniqueN(zip_code) - 1), by = household_code]
prop.table(table(nMoves$moves))
moveID <- nMoves[moves == 1]$household_code
setorder(panel, household_code, panel_year)
moveYears <- panel[household_code %in% moveID, .(moveYear = head(panel_year, 1)),
                   by = .(household_code, zip_code)]
moveYears <- moveYears[, .(moveYear = tail(moveYear, 1)), by = household_code]
panel <- merge(panel, moveYears, by = "household_code", all.x = TRUE)

# Classifying households by mover type
panel[, "YearsAfterMove" := panel_year - moveYear]
lawPre1 <- panel[YearsAfterMove == -1, .(household_code, lawPre = lawInd)]
lawPre2 <- panel[YearsAfterMove == -2, .(household_code, lawPre = lawInd)]
lawPre3 <- panel[YearsAfterMove == -3, .(household_code, lawPre = lawInd)]
lawPre4 <- panel[YearsAfterMove == -4, .(household_code, lawPre = lawInd)]
lawPre5 <- panel[YearsAfterMove == -5, .(household_code, lawPre = lawInd)]
lawPre6 <- panel[YearsAfterMove == -6, .(household_code, lawPre = lawInd)]
lawPre7 <- panel[YearsAfterMove == -7, .(household_code, lawPre = lawInd)]
lawPre8 <- panel[YearsAfterMove == -8, .(household_code, lawPre = lawInd)]
lawPre9 <- panel[YearsAfterMove == -9, .(household_code, lawPre = lawInd)]
lawPre10 <- panel[YearsAfterMove == -10, .(household_code, lawPre = lawInd)]
lawPre11 <- panel[YearsAfterMove == -11, .(household_code, lawPre = lawInd)]
lawPre12 <- panel[YearsAfterMove == -12, .(household_code, lawPre = lawInd)]
lawPre13 <- panel[YearsAfterMove == -13, .(household_code, lawPre = lawInd)]

lawPost1 <- panel[YearsAfterMove == 0, .(household_code, lawPost = lawInd)]
lawPost2 <- panel[YearsAfterMove == 1, .(household_code, lawPost = lawInd)]
lawPost3 <- panel[YearsAfterMove == 2, .(household_code, lawPost = lawInd)]
lawPre <- unique(rbindlist(list(lawPre1, lawPre2, lawPre3, lawPre4, lawPre5,
                                lawPre6, lawPre7, lawPre8, lawPre9, lawPre10,
                                lawPre11, lawPre12, lawPre13),
                           use.names = TRUE))
lawPost <- unique(rbindlist(list(lawPost1, lawPost2, lawPost3), use.names = TRUE))
panel <- merge(panel, lawPre, by = "household_code", all.x = TRUE)
panel <- merge(panel, lawPost, by = "household_code", all.x = TRUE)
panel[nMoves == 0 & lawInd == TRUE, "moveType" := "StayReg"]
panel[nMoves == 0 & lawInd == FALSE, "moveType" := "StayNoReg"]
panel[nMoves > 1, "moveType" := "MultiMove"]
panel[lawPre == FALSE & lawPost == TRUE, "moveType" := "Move To"]
panel[lawPre == TRUE & lawPost == FALSE, "moveType" := "Move Away"]
panel[lawPre == TRUE & lawPost == TRUE, "moveType" := "Move Same Reg"]
panel[lawPre == FALSE & lawPost == FALSE, "moveType" := "Move Same No Reg"]

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
reg2 <- felm(bulk ~ lawInd + household_income_coarse + age + adult + nChildren +
               married + college + type_of_residence + carShare | 0 | 0 | state,
             data = discBehaviorNonFood)
# reg3 <- felm(bulk ~ lawInd * household_income_coarse + adult + nChildren +
#                age + carShare + type_of_residence + college + married | 0 | 0 | state,
#              data = discBehaviorNonFood)
reg4 <- felm(bulk ~ display + age + adult + nChildren +
               married + college + type_of_residence + carShare | 0 | 0 | state,
             data = discBehaviorNonFood)
reg5 <- felm(bulk ~ display + age + adult + nChildren +
               married + college + type_of_residence + carShare | 0 | 0 | state,
             data = discBehaviorNonFood[state != 6])

meanBulk <- round(discBehaviorNonFood[, weighted.mean(bulk, w = projection_factor)], 2)
meanBulkNoCA <- round(discBehaviorNonFood[state != 6, weighted.mean(bulk, w = projection_factor)], 2)
stargazer(reg1, reg2, reg4, reg5, type = "text",
          add.lines = list(c("Avg Bulk", rep(meanBulk, 3), rep(meanBulkNoCA, 2)),
                           c("Demographics", "N", "Y", "Y", "Y"),
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

# Getting summary stats for laws with and without regulations
cols <- c("household_income", "household_size", "child", "married", "college", "age")
summaryMeans <- discBehaviorNonFood[!moveType %in% c("MultiMove", "StayNoReg", "StayReg"),
                                    lapply(.SD, mean), .SDcols = cols, by = moveType]
summaryMeans[, "type" := "Mean"]
summarySD <- discBehaviorNonFood[!moveType %in% c("MultiMove", "StayNoReg", "StayReg"),
                                 lapply(.SD, sd), .SDcols = cols, by = moveType]
summarySD[, "type" := "SD"]

# Assembling final table
finalTable <- rbindlist(list(summaryMeans, summarySD), use.names = TRUE)
finalTableLong <- melt(finalTable, id.vars = c("moveType", "type"))
finalTableWide <- dcast(finalTableLong, variable ~ moveType + type)
setnames(finalTableWide, "variable", "Variable")
finalTableWide[Variable == "household_income", "Variable" := "Household income ($000s)"]
finalTableWide[Variable == "household_size", "Variable" := "Household size"]
finalTableWide[Variable == "age", "Variable" := "Age"]
finalTableWide[Variable == "college", "Variable" := "College Educated"]
finalTableWide[Variable == "child", "Variable" := "Child Present"]
finalTableWide[Variable == "married", "Variable" := "Married"]

kable(finalTableWide, digits = 2, format = "markdown")
discBehaviorNonFood[, .N, by = moveType]
# Saved in summaryStatsUnitLawMovers.tex

# Getting regression table
# Movers (symmetric effects, no difference between move to and move away from regs)
movers <- c("Move Same No Reg", "Move Same Reg", "Move To", "Move Away")
reg1 <- felm(bulk ~ lawInd | household_code + panel_year | 0 | household_code,
              data = discBehaviorNonFood[moveType %in% movers])
reg2 <- felm(bulk ~ lawInd + household_income_coarse + age + adult + nChildren +
                married + college + type_of_residence + carShare |
                household_code + panel_year | 0 | household_code,
              data = discBehaviorNonFood[moveType %in% movers])
reg3 <- felm(bulk ~ lawInd + household_income_coarse + age + adult + nChildren +
                married + college + type_of_residence + carShare |
                household_code + panel_year | 0 | household_code,
              data = discBehaviorNonFood[moveType %in% c("Move Same No Reg", "Move Same Reg", "Move To")])
reg4 <- felm(bulk ~ lawInd + household_income_coarse + age + adult + nChildren +
                married + college + type_of_residence + carShare |
                household_code + panel_year | 0 | household_code,
              data = discBehaviorNonFood[moveType %in% c("Move Same Reg", "Move Same No Reg", "Move Away")])

meanMove1 <- round(discBehaviorNonFood[moveType %in% movers, mean(bulk)], 2)
meanMove2 <- round(discBehaviorNonFood[moveType %in% c("Move Same No Reg", "Move Same Reg", "Move To"),
                                       mean(bulk)], 2)
meanMove3 <- round(discBehaviorNonFood[moveType %in% c("Move Same Reg", "Move Same No Reg", "Move Away"),
                                       mean(bulk)], 2)

stargazer(reg1, reg2, reg3, reg4, type = "text",
          add.lines = list(c("Avg Bulk", rep(meanMove1, 2), meanMove2, meanMove3),
                           c("Household FE", "Y", "Y", "Y", "Y"),
                           c("Year FE", "Y", "Y", "Y", "Y"),
                           c("Demographics", "N", "Y", "Y", "Y")),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          dep.var.caption = "", dep.var.labels.include = FALSE,
          column.separate = c(2, 1, 1),
          column.labels = c("All Movers", "No Law To Law", "Law To No Law"),
          keep = c("lawInd"),
          covariate.labels = c("Regulation"),
          notes.align = "l",
          digits = 3,
          out = "tables/unitPriceLawMovers.tex")

# Generating event study.
discBehaviorNonFood[moveType %in% c("Move Same No Reg", "Move Same Reg"),
                    moveCoarse := "Move No Change"]
discBehaviorNonFood[moveType %in% c("Move To", "Move Away"), moveCoarse := moveType]
discBehaviorNonFood[moveCoarse == "Move No Change", "lawMove" := -1]
discBehaviorNonFood[moveCoarse == "Move To", "lawMove" := YearsAfterMove]
discBehaviorNonFood[moveCoarse == "Move Away", "lawMove" := YearsAfterMove]
discBehaviorNonFood[, "lawMove" := relevel(as.factor(lawMove), ref = "-1")]

regEvent1 <- felm(bulk ~ lawMove + household_income_coarse + age + adult + nChildren +
                    married + college + type_of_residence + carShare |
                    household_code + panel_year | 0 | household_code,
                  data = discBehaviorNonFood[moveCoarse %in% c("Move No Change", "Move To")])
regEvent2 <- felm(bulk ~ lawMove + household_income_coarse + age + adult + nChildren +
                    married + college + type_of_residence + carShare |
                    household_code + panel_year | 0 | household_code,
                  data = discBehaviorNonFood[moveCoarse %in% c("Move No Change", "Move Away")])
stargazer(regEvent1, regEvent2, type = "text")

# Making event study graph
refRow <- data.table(rn = "lawMove-1", Estimate = 0, `Cluster s.e.` = 0,
                     `t value` = 0, `Pr(>|t|)` = 0, `2.5 %` = 0, `97.5 %` = 0)

finalCoefs1 <- as.data.table(summary(regEvent1)$coefficients, keep.rownames = TRUE)
finalInt1 <- as.data.table(confint(regEvent1), keep.rownames = TRUE)
finalCoefs1 <- merge(finalCoefs1, finalInt1, by = "rn")
finalCoefs1 <- rbindlist(list(finalCoefs1, refRow), use.names = TRUE)
finalCoefs1 <- finalCoefs1[grepl("lawMove", rn)]
finalCoefs1[, "type" := "To Law"]

finalCoefs2 <- as.data.table(summary(regEvent2)$coefficients, keep.rownames = TRUE)
finalInt2 <- as.data.table(confint(regEvent2), keep.rownames = TRUE)
finalCoefs2 <- merge(finalCoefs2, finalInt2, by = "rn")
finalCoefs2 <- rbindlist(list(finalCoefs2, refRow), use.names = TRUE)
finalCoefs2 <- finalCoefs2[grepl("lawMove", rn)]
finalCoefs2[, "type" := "To No Law"]

finalCoefs <- rbindlist(list(finalCoefs1, finalCoefs2), use.names = TRUE)
finalCoefs[, "rn" := as.integer(gsub("lawMove", "", rn))]
setnames(finalCoefs, c("Lag", "beta", "se", "t", "p", "LCL", "UCL", "Type"))

pd <- position_dodge(width = 0.2)
ggplot(data = finalCoefs[Lag %in% -3:3], aes(x = Lag, y = beta, color = Type, shape = Type)) +
  geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.1, position = pd) +
  geom_point(size = 2, position = pd) +
  geom_hline(yintercept = 0) +
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


############ ROBUSTNESS: ESTIMATING BY STORE TYPE ##############################
# Getting data from discountingBehavior.R
# Adding demographics
discBehaviorChannel <- fread("/scratch/upenn/hossaine/discBehaviorChannel.csv",
                             nThread = threads)[food == 0]
discBehaviorChannel <- merge(discBehaviorChannel, panel,
                             by = c("household_code", "panel_year"))
discBehaviorChannel[state == 6, "display" := "Voluntary"]
discBehaviorChannel[, c("food", "coupon", "generic", "none", "one", "two", "three",
                        "men", "women", "fips") := NULL]

# Cross section (only for the final spec. Others are similar)
reg5a <- felm(bulk ~ display + age + adult + nChildren +
               married + college + type_of_residence + carShare | 0 | 0 | state,
             data = discBehaviorChannel[state != 6 & channel_type == "Grocery"])
reg5b <- felm(bulk ~ display + age + adult + nChildren +
               married + college + type_of_residence + carShare | 0 | 0 | state,
             data = discBehaviorChannel[state != 6 & channel_type == "Drug Store"])
reg5c <- felm(bulk ~ display + age + adult + nChildren +
               married + college + type_of_residence + carShare | 0 | 0 | state,
             data = discBehaviorChannel[state != 6 & channel_type == "Discount Store"])
reg5d <- felm(bulk ~ display + age + adult + nChildren +
               married + college + type_of_residence + carShare | 0 | 0 | state,
             data = discBehaviorChannel[state != 6 & channel_type == "Dollar Store"])
reg5e <- felm(bulk ~ display + age + adult + nChildren +
               married + college + type_of_residence + carShare | 0 | 0 | state,
             data = discBehaviorChannel[state != 6 & channel_type == "Warehouse Club"])

meanBulk1 <- round(discBehaviorChannel[state != 6 & channel_type == "Grocery",
                                       weighted.mean(bulk, w = projection_factor)], 2)
meanBulk2 <- round(discBehaviorChannel[state != 6 & channel_type == "Drug Store",
                                       weighted.mean(bulk, w = projection_factor)], 2)
meanBulk3 <- round(discBehaviorChannel[state != 6 & channel_type == "Discount Store",
                                       weighted.mean(bulk, w = projection_factor)], 2)
meanBulk4 <- round(discBehaviorChannel[state != 6 & channel_type == "Dollar Store",
                                       weighted.mean(bulk, w = projection_factor)], 2)
meanBulk5 <- round(discBehaviorChannel[state != 6 & channel_type == "Warehouse Club",
                                       weighted.mean(bulk, w = projection_factor)], 2)

stargazer(reg5a, reg5b, reg5c, reg5d, reg5e, type = "text",
          add.lines = list(c("Avg Bulk", meanBulk1, meanBulk2, meanBulk3, meanBulk4, meanBulk5),
                           c("Demographics", rep("Y", 5)),
                           c("Omit California", rep("Y", 5))),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          dep.var.caption = "", dep.var.labels.include = FALSE,
          keep = c("display*"),
          order = c(3, 2, 1),
          column.labels = c("Grocery", "Drug", "Discount", "Dollar", "Warehouse"),
          covariate.labels = c("Vol. Disp", "Mand. Disp", "Mand. Disp, Strict"),
          notes.align = "l",
          notes.append = TRUE,
          digits = 3,
          out = "tables/unitPriceLawChannel.tex")

# Getting regression table
# Movers (symmetric effects, no difference between move to and move away from regs)
movers <- c("Move Same No Reg", "Move Same Reg", "Move To", "Move Away")
reg2a <- felm(bulk ~ lawInd + household_income_coarse + age + adult + nChildren +
               married + college + type_of_residence + carShare |
               household_code + panel_year | 0 | household_code,
             data = discBehaviorChannel[moveType %in% movers & channel_type == "Grocery"])
reg2b <- felm(bulk ~ lawInd + household_income_coarse + age + adult + nChildren +
               married + college + type_of_residence + carShare |
               household_code + panel_year | 0 | household_code,
             data = discBehaviorChannel[moveType %in% movers & channel_type == "Drug Store"])
reg2c <- felm(bulk ~ lawInd + household_income_coarse + age + adult + nChildren +
               married + college + type_of_residence + carShare |
               household_code + panel_year | 0 | household_code,
             data = discBehaviorChannel[moveType %in% movers & channel_type == "Discount Store"])
reg2d <- felm(bulk ~ lawInd + household_income_coarse + age + adult + nChildren +
               married + college + type_of_residence + carShare |
               household_code + panel_year | 0 | household_code,
             data = discBehaviorChannel[moveType %in% movers & channel_type == "Dollar Store"])
reg2e <- felm(bulk ~ lawInd + household_income_coarse + age + adult + nChildren +
               married + college + type_of_residence + carShare |
               household_code + panel_year | 0 | household_code,
             data = discBehaviorChannel[moveType %in% movers & channel_type == "Warehouse Club"])
stargazer(reg2a, reg2b, reg2c, reg2d, reg2e, type = "text")

reg3a <- felm(bulk ~ lawInd + household_income_coarse + age + adult + nChildren +
               married + college + type_of_residence + carShare |
               household_code + panel_year | 0 | household_code,
             data = discBehaviorChannel[moveType %in% c("Move Same No Reg", "Move Same Reg", "Move To") & channel_type == "Grocery"])
reg3b <- felm(bulk ~ lawInd + household_income_coarse + age + adult + nChildren +
               married + college + type_of_residence + carShare |
               household_code + panel_year | 0 | household_code,
             data = discBehaviorChannel[moveType %in% c("Move Same No Reg", "Move Same Reg", "Move To") & channel_type == "Drug Store"])
reg3c <- felm(bulk ~ lawInd + household_income_coarse + age + adult + nChildren +
               married + college + type_of_residence + carShare |
               household_code + panel_year | 0 | household_code,
             data = discBehaviorChannel[moveType %in% c("Move Same No Reg", "Move Same Reg", "Move To") & channel_type == "Discount Store"])
reg3d <- felm(bulk ~ lawInd + household_income_coarse + age + adult + nChildren +
               married + college + type_of_residence + carShare |
               household_code + panel_year | 0 | household_code,
             data = discBehaviorChannel[moveType %in% c("Move Same No Reg", "Move Same Reg", "Move To") & channel_type == "Dollar Store"])
reg3e <- felm(bulk ~ lawInd + household_income_coarse + age + adult + nChildren +
               married + college + type_of_residence + carShare |
               household_code + panel_year | 0 | household_code,
             data = discBehaviorChannel[moveType %in% c("Move Same No Reg", "Move Same Reg", "Move To") & channel_type == "Warehouse Club"])
stargazer(reg3a, reg3b, reg3c, reg3d, reg3e, type = "text")

reg4a <- felm(bulk ~ lawInd + household_income_coarse + age + adult + nChildren +
               married + college + type_of_residence + carShare |
               household_code + panel_year | 0 | household_code,
             data = discBehaviorChannel[moveType %in% c("Move Same Reg", "Move Same No Reg", "Move Away") & channel_type == "Grocery"])
reg4b <- felm(bulk ~ lawInd + household_income_coarse + age + adult + nChildren +
               married + college + type_of_residence + carShare |
               household_code + panel_year | 0 | household_code,
             data = discBehaviorChannel[moveType %in% c("Move Same Reg", "Move Same No Reg", "Move Away") & channel_type == "Drug Store"])
reg4c <- felm(bulk ~ lawInd + household_income_coarse + age + adult + nChildren +
               married + college + type_of_residence + carShare |
               household_code + panel_year | 0 | household_code,
             data = discBehaviorChannel[moveType %in% c("Move Same Reg", "Move Same No Reg", "Move Away") & channel_type == "Discount Store"])
reg4d <- felm(bulk ~ lawInd + household_income_coarse + age + adult + nChildren +
               married + college + type_of_residence + carShare |
               household_code + panel_year | 0 | household_code,
             data = discBehaviorChannel[moveType %in% c("Move Same Reg", "Move Same No Reg", "Move Away") & channel_type == "Dollar Store"])
reg4e <- felm(bulk ~ lawInd + household_income_coarse + age + adult + nChildren +
               married + college + type_of_residence + carShare |
               household_code + panel_year | 0 | household_code,
             data = discBehaviorChannel[moveType %in% c("Move Same Reg", "Move Same No Reg", "Move Away") & channel_type == "Warehouse Club"])
stargazer(reg4a, reg4b, reg4c, reg4d, reg4e, type = "text")

############ ROBUSTNESS: ESTIMATING BY CHAON TYPE ##############################
# Getting data from discountingBehavior.R
# Adding demographics
discBehaviorChain <- fread("/scratch/upenn/hossaine/discBehaviorChain.csv",
                             nThread = threads)[food == 0]
discBehaviorChain <- merge(discBehaviorChain, panel,
                             by = c("household_code", "panel_year"))
discBehaviorChain[state == 6, "display" := "Voluntary"]
discBehaviorChain[, c("food", "coupon", "generic", "none", "one", "two", "three",
                        "men", "women", "fips") := NULL]

# Cross section (only for the final spec. Others are similar)
reg5a <- felm(bulk ~ display + age + adult + nChildren +
                married + college + type_of_residence + carShare | 0 | 0 | state,
              data = discBehaviorChain[state != 6 & chainType == "Local"])
reg5b <- felm(bulk ~ display + age + adult + nChildren +
                married + college + type_of_residence + carShare | 0 | 0 | state,
              data = discBehaviorChain[state != 6 & chainType == "Regional"])
reg5c <- felm(bulk ~ display + age + adult + nChildren +
                married + college + type_of_residence + carShare | 0 | 0 | state,
              data = discBehaviorChain[state != 6 & chainType == "National"])
stargazer(reg5a, reg5b, reg5c, type = "text")

meanBulk1 <- round(discBehaviorChain[state != 6 & chainType == "Local",
                                       weighted.mean(bulk, w = projection_factor)], 2)
meanBulk2 <- round(discBehaviorChain[state != 6 & chainType == "Regional",
                                       weighted.mean(bulk, w = projection_factor)], 2)
meanBulk3 <- round(discBehaviorChain[state != 6 & chainType == "National",
                                       weighted.mean(bulk, w = projection_factor)], 2)

stargazer(reg5a, reg5b, reg5c, type = "text",
          add.lines = list(c("Avg Bulk", meanBulk1, meanBulk2, meanBulk3),
                           c("Demographics", rep("Y", 3)),
                           c("Omit California", rep("Y", 3))),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          dep.var.caption = "", dep.var.labels.include = FALSE,
          keep = c("display*"),
          order = c(3, 2, 1),
          column.labels = c("Local", "Regional", "National"),
          covariate.labels = c("Vol. Disp", "Mand. Disp", "Mand. Disp, Strict"),
          notes.align = "l",
          notes.append = TRUE,
          digits = 3,
          out = "tables/unitPriceLawChain.tex")

