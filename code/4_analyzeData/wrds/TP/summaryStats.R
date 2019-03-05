# Generates summary stats for TP purchases
library(data.table)
library(stargazer)
library(ggplot2)
threads <- 8
path <- "/scratch/upenn/hossaine/"

tpPurch <- fread(paste0(path, "7260Purch.csv"), nThread = threads)[drop == 0]
tpPurch <- na.omit(tpPurch, cols = "size")
finalTable <- NULL
for (i in unique(tpPurch$household_income_coarse)) {
  tmpDat <- tpPurch[household_income_coarse == i]
  tmpTable <- data.table(Income = i,
                         p25 = round(quantile(tmpDat$size, 0.25), digits = 2),
                         p50 = round(quantile(tmpDat$size, 0.50), digits = 2),
                         p75 = round(quantile(tmpDat$size, 0.75), digits = 2),
                         M = round(mean(tmpDat$size), digits = 2),
                         SD = round(sd(tmpDat$size), digits = 2),
                         N = nrow(tmpDat))
  finalTable <- rbind(finalTable, tmpTable)
}

setnames(finalTable, c("Income", "25th Pctl", "50th Pctl", "75th Pctl", "M", "SD", "N"))
finalTable <- finalTable[c(1, 2, 4, 3)]
stargazer(finalTable, summary = FALSE, type = "text",
          rownames = FALSE,
          title = "Package Size Statistics by Income Quantile",
          notes = "Sizes are in standardized 225-sheet, 2-ply rolls.",
          label = "tab:sizeSummary",
          out = "./tables/sizeSummary.tex")
