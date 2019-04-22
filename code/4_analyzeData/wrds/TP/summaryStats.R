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

# Making Table 1 of Khan and Jain 2005 (JMR)
sizes <- c(4, 6, 12, 24)
brands <- c("CHARMIN", "ANGEL SOFT", "KLEENEX COTTONELLE", "QUILTED NORTHERN", "SCOTT 1000", "CTL BR")

tpPurch[sizeUnadj %in% sizes & brand_descr %in% brands, "chosen" := 1L]
tpPurch[is.na(chosen), "chosen" := 0L]
tpPurch[, "brandSize" := ifelse(chosen == 1, paste0(brand_descr, sizeUnadj), "NOTCHOSEN")]
tableData <- tpPurch[chosen == 1, .(shares = .N / nrow(tpPurch) * 100,
                                    unitPrice = mean(total_price_paid / size, na.rm = TRUE)),
                     keyby = .(brand_descr, sizeUnadj)]
setnames(tableData, c("Brand", "Package Size", "% of Purchases", "Unit Price"))
stargazer(tableData, summary = FALSE, type = "text", digits = 2,
          rownames = FALSE,
          title = "Product Descriptive Statistics",
          notes = "Unit prices are in terms of standard 225-sheet, 2-ply rolls.",
          label = "tab:prodSummary",
          out = "./tables/prodSummary.tex")

