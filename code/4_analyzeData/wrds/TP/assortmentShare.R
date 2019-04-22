# Computes the share of all products sold in the Scanner data that are 4, 6, 12, or 24
# packs of Charmin, Angel Soft, Quilted Northern, Cottonelle, Scott, or store brand
library(data.table)

fullAssort <- NULL
for (i in 2006:2014) {
  assort <- fread(paste0("/home/mallick/Desktop/Nielsen/Data/Scanner/Assortment/", i, ".csv"),
                  select = c("brand_code_uc", "pkgSize", "units", "pCents", "size"))
  assort[, c("unitCost", "pCents", "size") := .(pCents / size / 100, NULL, NULL)]
  fullAssort <- rbindlist(list(fullAssort, assort), use.names = TRUE)
}

# Getting total unit sales
fullSales <- fullAssort[, .(units = sum(units),
                            unitCost = weighted.mean(unitCost, w = units, na.rm = TRUE)),
                        by = .(brand_code_uc, pkgSize)]
rm(fullAssort)

sizes <- c(4, 6, 12, 24)
brandKey <- data.table(brand = c("Angel Soft", "Charmin", "Other", "Cottonelle",
                                 "Qltd Ntn", "Scott"),
                       brand_code_uc = c(506045, 526996, 536746, 581898, 624459, 635074))

fullSales[pkgSize %in% sizes & brand_code_uc %in% brandKey$brand_code_uc, "chosen" := 1L]
fullSales[is.na(chosen), "chosen" := 0L]
fullSales <- merge(fullSales, brandKey, by = "brand_code_uc", all.x = TRUE)

tableData <- fullSales[chosen == 1, .(shares = units / sum(fullSales$units) * 100,
                                      unitPrice = mean(unitCost, na.rm = TRUE)),
                     keyby = .(brand, pkgSize)]
setnames(tableData, c("Brand", "Package Size", "% of Purchases", "Unit Price"))
stargazer(tableData, summary = FALSE, type = "text", digits = 2,
          rownames = FALSE,
          title = "Product Descriptive Statistics",
          notes = "Unit prices are in terms of standard 225-sheet, 2-ply rolls.",
          label = "tab:prodSummary",
          out = "./code/6_paper/tables/assortmentSummary.tex")
