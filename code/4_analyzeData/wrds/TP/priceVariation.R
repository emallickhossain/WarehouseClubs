# Where is the price variation?????
library(data.table)
library(stargazer)
library(ggplot2)
library(ggthemes)
library(glmnet)
library(lfe)
library(doMC)
library(knitr)
library(Hmisc)
registerDoMC(cores = 4)
threads <- 8
path <- "/scratch/upenn/hossaine/"

top5 <- c("CHARMIN", "ANGEL SOFT", "QUILTED NORTHERN", "KLEENEX COTTONELLE", "SCOTT 1000")
brandCode <- c(526996, 506045, 624459, 581898, 635074)
fullSet <- expand.grid(brand_code_uc = c(brandCode, 536746),
                       pkgSize = c(4, 6, 12, 24))
fullSet <- cbind(alt = 1:24, fullSet)

# Plotting residual histograms of prices after controlling for brands or stores
scanner <- NULL
for (i in 2006:2014) {
  print(i)
  # Getting store assortment
  assort <- fread(paste0(path, "Assortment/", i, ".csv"), nThread = 16)
  scanner <- rbind(scanner, assort)
}
rm(assort)

scanner <- merge(scanner, fullSet, by = c("brand_code_uc", "pkgSize"), all.x = TRUE)
scanner[, "unitCost" := pCents / size]
scanner[, "storeWeekMean" := mean(unitCost), by = .(store_code_uc, week_end)]
scanner[, "demeanUnitCost" := unitCost - storeWeekMean]
reg <- lm(demeanUnitCost ~ as.factor(alt), data = scanner)
summary(reg)





# Getting average between store variance
upcs <- scanner[, .(units = sum(units),
                    var = wtd.var(pCents, units),
                    mean = weighted.mean(pCents, units)), by = upc]
upcs[, .(cv = weighted.mean(sqrt(var) / mean, units, na.rm = TRUE))]

# Natural variation
var0 <- var(scanner$pCents)
row0 <- c("No Controls", 0)

# Variation captured by UPC
reg1 <- felm(data = scanner, pCents ~ 1 | upc)
var1 <- var(reg1$residuals)
row1 <- c("UPC", round((1 - (var1 / var0)) * 100, digits = 2))
rm(reg1)

# Variation captured by retailer and UPC
reg2 <- felm(data = scanner, pCents ~ 1 | upc + retailer_code)
var2 <- var(reg2$residuals)
row2 <- c("UPC + Retailer", round((1 - (var2 / var0)) * 100, digits = 2))
rm(reg2)

# Variation captured by brand and UPC
reg3 <- felm(data = scanner, pCents ~ 1 | upc + brand_code_uc)
var3 <- var(reg3$residuals)
row3 <- c("UPC + Brand", round((1 - (var3 / var0)) * 100, digits = 2))
rm(reg3)

# Variation captured by retailer and brand
reg4 <- felm(data = scanner, pCents ~ 1 | upc + brand_code_uc + retailer_code)
var4 <- var(reg4$residuals)
row4 <- c("UPC + Brand + Retailer", round((1 - (var4 / var0)) * 100, digits = 2))
rm(reg4)

tableDat <- rbind(row0, row1, row2, row3, row4)
kable(tableDat, type = "markdown", col.names = c("Controls", "Percent Explained"),
      row.names = FALSE)




fullChoice <- fread("/scratch/upenn/hossaine/TPMLogit.csv", nThread = threads)

# Collapsing products that are not uniquely defined by a brand-size
fullChoice <- na.omit(fullChoice, cols = "size")
fullChoice <- unique(fullChoice[, .(store_code_uc, week_end, upc, upc_ver_uc, brand_code_uc,
                                    pkgSize, ply, size, retailer_code, dma_code, p)])

# What explains price variations?
var0 <- var(fullChoice$p)
row0 <- c("No Controls", 0)

# Let's look at product characteristics first. Big packages are different than small
regA <- felm(data = fullChoice, p ~ pkgSize)
varA <- var(regA$residuals)
row1 <- c("Package Size", round((1 - (varA / var0)) * 100, digits = 2))

# The actual quantity of stuff also matters
regB <- felm(data = fullChoice, p ~ size)
varB <- var(regB$residuals)
row2 <- c("Std Size", round((1 - (varB / var0)) * 100, digits = 2))

# What about package size and actual stuff together?
regC <- felm(data = fullChoice, p ~ pkgSize + size)
varC <- var(regC$residuals)
row3 <- c("Package Size and Std Size", round((1 - (varC / var0)) * 100, digits = 2))

# Brand matters too!
regD <- felm(data = fullChoice, p ~ 1 | brand_code_uc)
varD <- var(regD$residuals)
row4 <- c("Brand only", round((1 - (varD / var0)) * 100, digits = 2))

# What about brand and package size?
regE <- felm(data = fullChoice, p ~ pkgSize | brand_code_uc)
varE <- var(regE$residuals)
row5 <- c("Brand and Package", round((1 - (varE / var0)) * 100, digits = 2))

# What about brand and actual stuff?
regF <- felm(data = fullChoice, p ~ size | brand_code_uc)
varF <- var(regF$residuals)
row6 <- c("Brand and Std Size", round((1 - (varF / var0)) * 100, digits = 2))

# What about brand, package size, and actual stuff?
regG <- felm(data = fullChoice, p ~ pkgSize + size | brand_code_uc)
varG <- var(regG$residuals)
row7 <- c("Brand, Package, and Std Size", round((1 - (varG / var0)) * 100, digits = 2))

# What about brand, package size, actual stuff, and ply?
regH <- felm(data = fullChoice, p ~ pkgSize + size | brand_code_uc + ply)
varH <- var(regG$residuals)
row8 <- c("Brand, Package, Std Size, and Ply", round((1 - (varH / var0)) * 100, digits = 2))

# Let's collapse all that stuff to a UPC
regI <- felm(data = fullChoice, p ~ 1 | upc)
varI <- var(regI$residuals)
row9 <- c("UPC", round((1 - (varI / var0)) * 100, digits = 2))

# The same product might vary across geographic markets
regJ <- felm(data = fullChoice, p ~ 1 | upc + dma_code)
varJ <- var(regJ$residuals)
row10 <- c("UPC and DMA", round((1 - (varJ / var0)) * 100, digits = 2))

# The same product in the same geography might vary across retailers
regK <- felm(data = fullChoice, p ~ 1 | upc + dma_code + retailer_code)
varK <- var(regK$residuals)
row11 <- c("UPC, DMA, and Retailer", round((1 - (varK / var0)) * 100, digits = 2))

# The same product in the same geography in a particular retailer might vary across stores
regL <- felm(data = fullChoice, p ~ 1 | upc + dma_code + retailer_code + store_code_uc)
varL <- var(regL$residuals)
row12 <- c("UPC, DMA, Retailer, Week", round((1 - (varL / var0)) * 100, digits = 2))

# The same product in the same geography in a particular retailer in a specific store may vary across weeks
regM <- felm(data = fullChoice, p ~ 1 | upc + dma_code + retailer_code + store_code_uc + week_end)
varM <- var(regM$residuals)
row13 <- c("UPC, DMA, Retailer, Store, Week", round((1 - (varM / var0)) * 100, digits = 2))

tableDat <- rbind(row0, row1, row2, row3, row4, row5, row6, row7, row8, row9,
                  row10, row11, row12, row13)
kable(tableDat, type = "markdown", col.names = c("Controls", "Percent Explained"),
      row.names = FALSE)
