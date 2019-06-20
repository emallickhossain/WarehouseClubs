# Produces table of Homescan summary statistics
library(data.table)
library(knitr)
library(Hmisc)
panel <- fread("/scratch/upenn/hossaine/fullPanel.csv",
               select = c("projection_factor", "household_income",
                          "household_size", "hispanic_origin", "age", "college",
                          "white", "child", "married"))

panel[, "household_income" := factor(household_income,
                                     levels = c(8, 10, 11, 13, 15, 16, 17, 18,
                                                19, 21, 23, 26, 27),
                                     labels = c(11, 13.5, 17.5, 22.5, 27.5, 32.5,
                                                37.5, 42.5, 47.5, 55, 65, 85, 100),
                                     ordered = TRUE)]
panel[, "household_income" := as.numeric(as.character(household_income))]
panel <- panel[!is.na(household_income)]

summaryMeans <- panel[, lapply(.SD, weighted.mean, w = projection_factor)]
summaryMeans[, "type" := "Mean"]
summarySD <- panel[, lapply(.SD, wtd.var, w = projection_factor)]
summarySD <- sqrt(summarySD)
summarySD[, "type" := "Standard Deviation"]

finalTable <- rbindlist(list(summaryMeans, summarySD), use.names = TRUE)
finalTableLong <- melt(finalTable, id.vars = "type")
finalTableWide <- dcast(finalTableLong, variable ~ type)
setnames(finalTableWide, "variable", "Variable")
finalTableWide <- finalTableWide[Variable != "projection_factor"]
finalTableWide[Variable == "household_income", "Variable" := "Household income ($000s)"]
finalTableWide[Variable == "household_size", "Variable" := "Household size"]
finalTableWide[Variable == "hispanic_origin", "Variable" := "Hispanic"]
finalTableWide[Variable == "Hispanice", "Mean" := 1 - (Mean - 1)]
finalTableWide[Variable == "age", "Variable" := "Age"]
finalTableWide[Variable == "college", "Variable" := "College Educated"]
finalTableWide[Variable == "white", "Variable" := "White"]
finalTableWide[Variable == "child", "Variable" := "Child present"]
finalTableWide[Variable == "married", "Variable" := "Married"]

kable(finalTableWide, digits = 2, format = "latex")
# saved as homescanSummaryStats.tex
