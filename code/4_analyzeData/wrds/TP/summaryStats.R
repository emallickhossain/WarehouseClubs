# Generates summary stats for TP purchases
library(data.table)
library(knitr)

# Getting product master list
tpPurch <- fread(paste0("/scratch/upenn/hossaine/7260Purch.csv"))
brandTable <- sort(prop.table(table(tpPurch$brand_descr)), decreasing = TRUE)[1:6]
total <- round(sum(brandTable), digits = 2)
brandTable <- c(brandTable, total)
names(brandTable)[7] <- "Total"
kable(as.table(brandTable), type = "markdown", digits = 2, col.names = c("Brand", "Share"))

# Getting package size list
sizeTable <- sort(prop.table(table(tpPurch$sizeUnadj)), decreasing = TRUE)[1:8]
total <- round(sum(sizeTable), digits = 2)
sizeTable <- c(sizeTable, total)
names(sizeTable)[9] <- "Total"
kable(as.table(sizeTable), type = "markdown", digits = 2, col.names = c("Size", "Share"))

# Getting package size list
tpPurch[, "brandYes" := ifelse(brand_descr %in% names(brandTable), 1L, 0L)]
tpPurch[, "sizeYes" := ifelse(sizeUnadj %in% names(sizeTable), 1L, 0L)]
tpPurch[, "brandSize" := brandYes * sizeYes]
brandSizeTable <- sort(prop.table(table(tpPurch$brandSize)), decreasing = TRUE)
total <- round(sum(brandSizeTable), digits = 2)
brandSizeTable <- c(brandSizeTable, total)
names(brandSizeTable)[9] <- "Total"
kable(as.table(sizeTable), type = "markdown", digits = 2, col.names = c("Size", "Share"))


# Summarizing data
fullChoice <- fread("/scratch/upenn/hossaine/TPMLogit.csv")
fullChoice[, "Home" := (type_of_residence == "Home")]
fullChoice[, "Apt" := (type_of_residence == "Apt")]
fullChoice[, "Mobile" := (type_of_residence == "Mobile")]
fullChoice[, "Age < 45" := (age == "<45")]
fullChoice[, "Age 45-64" := (age == "45-64")]
fullChoice[, "Age > 65" := (age == "65+")]
setnames(fullChoice, c("p", "unitCost", "child", "household_size_cts", "household_income_cts"),
         c("Price", "Unit Cost", "Child", "Size", "Income"))
fullChoice[, "brandChoice" := ifelse(choice == 1, brandBin, "0")]
fullChoice[, "sizeChoice" := ifelse(choice == 1, pkgSizeBin, "0")]

# Making summary table by brand
fullChoiceBrand <- dcast(fullChoice, trip_code_uc + projection_factor +
                          Home + Apt + Mobile + `Age < 45` + `Age 45-64` +
                          `Age > 65` + Size + Child + Income ~ brandBin,
                        value.var = c("Price", "Unit Cost", "pkgSize"), fun = mean, na.rm = TRUE)
fullChoiceBrand <- merge(fullChoiceBrand,
                        unique(fullChoice[brandChoice != 0, .(trip_code_uc, brandChoice)]),
                        by = "trip_code_uc")

brandMeans <- fullChoiceBrand[, lapply(.SD, weighted.mean, w = projection_factor,
                                       na.rm = TRUE), by = brandChoice]
brandMeans[, c("trip_code_uc", "projection_factor") := NULL]
brandMeans <- dcast(melt(brandMeans, id.vars = "brandChoice"), variable ~ brandChoice)
kable(brandMeans, digits = 2, format = "markdown")

# Making summary table by package size
fullChoiceSize <- dcast(fullChoice, trip_code_uc + projection_factor +
                           Home + Apt + Mobile + `Age < 45` + `Age 45-64` +
                           `Age > 65` + Size + Child + Income ~ pkgSizeBin,
                         value.var = c("Price", "Unit Cost"), fun = mean, na.rm = TRUE)
fullChoiceSize <- merge(fullChoiceSize,
                         unique(fullChoice[sizeChoice != 0, .(trip_code_uc, sizeChoice)]),
                         by = "trip_code_uc")

sizeMeans <- fullChoiceSize[, lapply(.SD, weighted.mean, w = projection_factor,
                                     na.rm = TRUE), by = sizeChoice]
sizeMeans[, c("trip_code_uc", "projection_factor") := NULL]
sizeMeans <- dcast(melt(sizeMeans, id.vars = "sizeChoice"), variable ~ sizeChoice)
setcolorder(sizeMeans, c("variable", "1-4", "5-6", "7-9", "10-12", "13-20", "21-24", "25+"))
sizeMeans <- sizeMeans[c(1:9, 10, 15, 16, 11:14, 17, 22, 23, 18:21), ]
kable(sizeMeans, digits = 2, format = "markdown")
