# Produces table of Homescan summary statistics
library(data.table)
library(knitr)
library(Hmisc)
library(forcats)
library(ggplot2)
library(ggthemes)
panel <- fread("/scratch/upenn/hossaine/fullPanel.csv",
               select = c("projection_factor", "household_income",
                          "household_size", "hispanic_origin", "age", "college",
                          "white", "child", "married"))

panel[, "household_income" := factor(household_income,
                                     levels = c(3, 4, 6, 8, 10, 11, 13, 15, 16,
                                                17, 18, 19, 21, 23, 26, 27),
                                     labels = c(2.5, 6.5, 9, 11, 13.5, 17.5,
                                                22.5, 27.5, 32.5, 37.5, 42.5,
                                                47.5, 55, 65, 85, 100),
                                     ordered = TRUE)]
panel[, "household_income" := as.numeric(as.character(household_income))]
panel <- panel[!is.na(household_income)]

summaryMeans <- panel[, lapply(.SD, weighted.mean, w = projection_factor)]
summaryMeans[, "type" := "Mean"]
summarySD <- panel[, lapply(.SD, wtd.var, w = projection_factor)]
summarySD <- sqrt(summarySD)
summarySD[, "type" := "SD"]
summary25 <- panel[, lapply(.SD, wtd.quantile, weights = projection_factor, 0.25)]
summary25[, "type" := "25th Pctile"]
summary75 <- panel[, lapply(.SD, wtd.quantile, weights = projection_factor, 0.75)]
summary75[, "type" := "75th Pctile"]

finalTable <- rbindlist(list(summaryMeans, summarySD, summary25, summary75), use.names = TRUE)
finalTableLong <- melt(finalTable, id.vars = "type")
finalTableWide <- dcast(finalTableLong, variable ~ type)
setnames(finalTableWide, "variable", "Variable")
setcolorder(finalTableWide, c("Variable", "Mean", "SD", "25th Pctile" ,"75th Pctile"))
finalTableWide <- finalTableWide[Variable != "projection_factor"]
finalTableWide[Variable == "household_income", "Variable" := "Household income ($000s)"]
finalTableWide[Variable == "household_size", "Variable" := "Household size"]
finalTableWide[Variable == "hispanic_origin", "Variable" := "Hispanic"]
finalTableWide[Variable == "Hispanic", "Mean" := 1 - (Mean - 1)]
finalTableWide[Variable == "Hispanic", "25th Pctile" := 1 - (`25th Pctile` - 1)]
finalTableWide[Variable == "Hispanic", "75th Pctile" := 1 - (`75th Pctile` - 1)]
finalTableWide[Variable == "age", "Variable" := "Age"]
finalTableWide[Variable == "college", "Variable" := "College Educated"]
finalTableWide[Variable == "white", "Variable" := "White"]
finalTableWide[Variable == "child", "Variable" := "Child present"]
finalTableWide[Variable == "married", "Variable" := "Married"]

kable(finalTableWide, digits = 2, format = "latex")
# saved as homescanSummaryStats.tex


# Looking at income transitions for households
panel <- fread("/scratch/upenn/hossaine/fullPanel.csv",
               select = c("projection_factor", "household_income", "household_size",
                          "hispanic_origin", "age", "college", "white",
                          "household_income_coarse", "child", "married",
                          "household_code", "panel_year"))
setorder(panel, household_code, panel_year)
panel[, "changes" := uniqueN(household_income), by = household_code]

# Tabling number of income changers and getting summary stats
changers <- unique(panel[, .(household_code, changes)])
round(prop.table(table(changers$changes)), 2)
changerStats <- panel[household_code %in% changers[changes > 1]$household_code]
changerStats[, "firstYear" := min(panel_year), by = household_code]
changerStatsMin <- changerStats[firstYear == panel_year]

# Making table of summary stats
cols <- c("household_size", "hispanic_origin", "age", "college", "white", "child", "married")
summaryMeans <- changerStatsMin[, lapply(.SD, weighted.mean, w = projection_factor),
                                .SDcols = cols]
summaryMeans[, "type" := "Mean"]
summarySD <- changerStatsMin[, lapply(.SD, wtd.var, w = projection_factor),
                             .SDcols = cols]
summarySD <- sqrt(summarySD)
summarySD[, "type" := "SD"]
summary25 <- changerStatsMin[, lapply(.SD, wtd.quantile, weights = projection_factor, 0.25),
                             .SDcols = cols]
summary25[, "type" := "25th Pctile"]
summary75 <- changerStatsMin[, lapply(.SD, wtd.quantile, weights = projection_factor, 0.75),
                             .SDcols = cols]
summary75[, "type" := "75th Pctile"]

finalTableChanger <- rbindlist(list(summaryMeans, summarySD, summary25, summary75),
                               use.names = TRUE)
finalTableChangerLong <- melt(finalTableChanger, id.vars = "type")
finalTableChangerWide <- dcast(finalTableChangerLong, variable ~ type)
setnames(finalTableChangerWide, "variable", "Variable")
setcolorder(finalTableChangerWide, c("Variable", "Mean", "SD", "25th Pctile" ,"75th Pctile"))
finalTableChangerWide <- finalTableChangerWide[Variable != "projection_factor"]
finalTableChangerWide[Variable == "household_income", "Variable" := "Household income ($000s)"]
finalTableChangerWide[Variable == "household_size", "Variable" := "Household size"]
finalTableChangerWide[Variable == "hispanic_origin", "Variable" := "Hispanic"]
finalTableChangerWide[Variable == "Hispanic", "Mean" := 1 - (Mean - 1)]
finalTableChangerWide[Variable == "Hispanic", "25th Pctile" := 1 - (`25th Pctile` - 1)]
finalTableChangerWide[Variable == "Hispanic", "75th Pctile" := 1 - (`75th Pctile` - 1)]
finalTableChangerWide[Variable == "age", "Variable" := "Age"]
finalTableChangerWide[Variable == "college", "Variable" := "College Educated"]
finalTableChangerWide[Variable == "white", "Variable" := "White"]
finalTableChangerWide[Variable == "child", "Variable" := "Child present"]
finalTableChangerWide[Variable == "married", "Variable" := "Married"]

kable(finalTableChangerWide, digits = 2, format = "latex")
# saved as homescanSummaryStatsChanger.tex


# Making correlation matrix
panel[, "income" := fct_recode(as.factor(household_income),
                               "<5" = "3",
                               "5-8" = "4",
                               "8-10" = "6",
                               "10-12" = "8",
                               "12-15" = "10",
                               "15-20" = "11",
                               "20-25" = "13",
                               "25-30" = "15",
                               "30-35" = "16",
                               "35-40" = "17",
                               "40-45" = "18",
                               "45-50" = "19",
                               "50-60" = "21",
                               "60-70" = "23",
                               "70-100" = "26",
                               ">100" = "27")]
panel[, "lagIncome" := shift(income, 1, type = "lag"), by = household_code]
transMat <- table(panel[, .(as.factor(lagIncome), as.factor(income))])
transMat <- setDT(melt(transMat / rowSums(transMat)))
setnames(transMat, c("lagIncome", "Income", "Prob"))

ggplot(data = transMat, aes(y = fct_rev(lagIncome), x = Income, fill = Prob)) +
  geom_tile(color = "white") +
  geom_text(aes(y = lagIncome, x = Income, label = round(Prob, 2)),
            color = "white", size = 4) +
  labs(title = "Income Transitions",
       x = "Future Income", y = "Current Income",
       caption = paste0("Note: Household income is in thousands of dollars. Values denote \n",
                        "the probability of transitioning from the row income to the \n",
                        "column income. Rows may not sum to 1 due to rounding.")) +
  theme_fivethirtyeight() +
  scale_x_discrete(position = "top") +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        legend.position = "none")
ggsave(filename = "./figures/incomeTransitionsAll.png", height = 6, width = 8)

# Income transitions between coarse bins
panel[, "changesCoarse" := uniqueN(household_income_coarse), by = household_code]
changersCoarse <- unique(panel[, .(household_code, changesCoarse)])
round(prop.table(table(changersCoarse$changesCoarse)), 2)
panel[, "lagIncomeCoarse" := shift(household_income_coarse, 1, type = "lag"),
      by = household_code]

transMatCoarse <- table(panel[, .(as.factor(lagIncomeCoarse),
                                  as.factor(household_income_coarse))])
transMatCoarse <- setDT(melt(transMatCoarse / rowSums(transMatCoarse)))
setnames(transMatCoarse, c("lagIncome", "Income", "Prob"))
transMatCoarse[, ':=' (lagIncome = factor(lagIncome, ordered = TRUE,
                                          levels = c(">100k", "50-100k", "25-50k", "<25k")),
                       Income = factor(Income, ordered = TRUE,
                                       levels = c("<25k", "25-50k", "50-100k", ">100k")))]

ggplot(data = transMatCoarse, aes(y = lagIncome, x = Income, fill = Prob)) +
  geom_tile(color = "white") +
  geom_text(aes(y = lagIncome, x = Income, label = round(Prob, 2)),
            color = "white", size = 4) +
  labs(title = "Income Transitions",
       x = "Future Income", y = "Current Income",
       caption = paste0("Note: Household income is in thousands of dollars. Values denote \n",
                        "the probability of transitioning from the row income to the \n",
                        "column income. Rows may not sum to 1 due to rounding.")) +
  theme_fivethirtyeight() +
  scale_x_discrete(position = "top") +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        legend.position = "none")
ggsave(filename = "./figures/incomeTransitionsCoarse.png", height = 6, width = 8)
