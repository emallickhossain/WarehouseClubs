# Measures inflation by product
library(data.table)
library(purrr)
library(furrr)
library(ggplot2)
library(ggthemes)
threads <- 8
yr <- 2006:2016
path <- "/scratch/upenn/hossaine/"

prod <- fread(paste0(path, "prod.csv"),
              select = c("upc", "upc_ver_uc", "upc_descr", "product_module_code",
                         "brand_code_uc", "brand_descr", "multi", "size1_amount"))
prod <- prod[product_module_code == 7260]
charmin <- prod[brand_descr == "CHARMIN"]

fullAssort <- NULL
getAssort <- function(yr) {
  assort <- fread(paste0(path, "Assortment/", yr, ".csv"), nThread = threads)
  return(assort)
}
fullAssort <- rbindlist(future_map(yr, getAssort))
fullAssort[, "unitCost" := pCents / size]
fullAssort[, "year" := substr(week_end, 1, 4)]
fullAssort[, "month" := substr(week_end, 5, 6)]
fullAssort[, "day" := substr(week_end, 7, 8)]
fullAssort[, "date" := as.Date(paste(year, month, day, sep = "-"))]
fullAssort[, "quarter" := quarter(date)]

upcSeries <- fullAssort[, .(unitCost = weighted.mean(unitCost, w = units)),
                        by = .(upc, upc_ver_uc, year, quarter, brand_code_uc,
                               pkgSize, ply, size)]
basePrice <- upcSeries[year == "2013" & quarter == 3, .(upc, upc_ver_uc, base_price = unitCost)]
upcSeries <- merge(upcSeries, basePrice, by = c("upc", "upc_ver_uc"), all.x = TRUE)
upcSeries <- merge(upcSeries, prod, by = c("upc", "upc_ver_uc", "brand_code_uc"))

upcSeries[, "index" := unitCost / base_price * 100]

toPlot <- na.omit(upcSeries, cols = "index")
toPlot[, "date" := as.Date(paste(year, quarter * 3, "01", sep = "-"))]
toPlot[, "upcID" := .GRP, by = upc]

# Plotting inflation for a particular brand
ggplot(data = toPlot[upc %in% c(3700050907:3700030924, 3700052219:3700052223)],
       aes(x = date, y = index, color = as.factor(upcID))) +
  geom_line()
