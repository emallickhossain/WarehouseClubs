# Recreates Table 1 of Orhun and Palazzolo
# Generates distribution of purchase sizes for modules
library(data.table)
library(purrr)
library(furrr)
library(plotly)
library(lfe)
library(stringr)
library(fredr)
library(Hmisc)
library(stargazer)
library(knitr)
fredr_set_key(fredAPI)
plan(multiprocess)
path <- "/home/mallick/Desktop/Nielsen/Data/Clean/"
yr <- 2004:2016
modCode <- 7260

stores <- c("Grocery", "Warehouse Club", "Discount Store", "Dollar Store")
retailers <- fread(paste0(path, "retailers.tsv"))[channel_type %in% stores]

pce <- setDT(fredr("PCEPI", observation_start = as.Date("2004-01-01"),
                   frequency = "a", aggregation_method = "avg"))
pce[, "panel_year" := year(date)]

prod <- fread(paste0(path, "prod.csv"),
              select = c("upc", "upc_ver_uc", "upc_descr", "product_module_code", "multi",
                         "size1_amount", "size1_units", "brand_code_uc", "brand_descr"))
prod <- prod[product_module_code == modCode]

# Fixing TP brand codes
prod[grep("CHARMIN", brand_descr), "brand_code_uc" := 526996]
prod[grep("COTTONELLE", brand_descr), "brand_code_uc" := 581898]
prod[grep("^SCOTT", brand_descr), "brand_code_uc" := 635073]

# For Toilet paper, creating standardized roll
prod[, "ply" := str_extract_all(upc_descr, "\\s\\dP\\s")]
prod[, "ply" := as.integer(gsub("P", "", ply))]
prod[, "sheet" := str_extract_all(upc_descr, "\\d{2,}S\\s")]
prod[, "sheet" := as.integer(gsub("S", "", sheet))]
prod[, "stdRolls" := ply * sheet / 550]
prod[, "size" := multi * size1_amount * stdRolls]
prod[, c("ply", "sheet", "stdRolls") := NULL]

getSize <- function(yr) {
  purch <- fread(paste0(path, "Purchases/purchase", yr, ".csv"))
  purch <- merge(purch, prod, by = c("upc", "upc_ver_uc"))
  purch[, "units" := size * quantity]

  trips <- fread(paste0(path, "Trips/trips", yr, ".csv"),
                 select = c("trip_code_uc", "retailer_code"))
  fullData <- merge(purch, trips, by = "trip_code_uc")
  return(fullData)
}

fullData <- rbindlist(future_map(yr, getSize))
fullData <- merge(fullData, retailers, by = "retailer_code")
fullData[, "brand_code_uc" := as.character(brand_code_uc)]
fullData[brand_code_uc == 536746, "brand_code_uc" := paste0(brand_code_uc, channel_type)]
fullData <- merge(fullData, pce, by = "panel_year")
fullData[, "realSpend" := total_price_paid / value * 100]
fullData[, "unitPrice" := total_price_paid / units]
fullData[, "rolls" := size1_amount * multi]

tabData <- fullData[rolls %in% c(4, 12, 24, 30, 36)]

col1 <- tabData[rolls == 4, .(realPrice = mean(realSpend)), by = .(brand_code_uc, brand_descr)]
col5 <- tabData[, .(sale = mean(deal_flag_uc) * 100), by = .(brand_code_uc, brand_descr)]

unitCosts <- tabData[, .(unitCost = mean(unitPrice, na.rm = TRUE)),
                     by = .(brand_code_uc, brand_descr, rolls)]
unitCosts <- dcast(unitCosts, brand_code_uc + brand_descr ~ rolls, value.var = "unitCost")
unitCosts[, ':=' ("twelve" = (`12` / `4` - 1) * 100,
                  "twentyFour" = (`24` / `4` - 1) * 100,
                  "thirty" = (`30` / `4` - 1) * 100,
                  "thirtySix" = (`36` / `4` - 1) * 100)]
midCols <- unitCosts[, .(twelve = twelve,
                         twentyFour = twentyFour,
                         thirties = mean(c(thirty, thirtySix), na.rm = TRUE)),
                     by = .(brand_code_uc, brand_descr)]
table1 <- merge(col1, midCols, by = c("brand_code_uc", "brand_descr"))
table1 <- merge(table1, col5, by = c("brand_code_uc", "brand_descr"))
table1 <- table1[brand_code_uc %in% c(506045, 526996, 581898, 624459, 635073)]

kable(table1[, c(-1, -3)], format = "markdown",
      rownames = c("Angel Soft", "Charmin", "Cottonelle", "Quilted Northern", "Scott"),
      col.names = c("4-Roll UPCs", "12 Roll", "24 Roll", "30/36 Roll", "Purchases on Sale"))

,
          column.separate = c(1, 3, 1),
          dep.var.labels = c("Non-sale price", "Magnitude of Bulk Discount", "Percentage of"))
,
          notes = c("This table provides (1) the average non-sale price for
                    4-roll UPCs of the given brand in the data, (2), the bulk
                    discount (price per standard roll) offered by 12-, 24-, and
                    30-/36-roll products relative to the 4-roll package, and
                    (3) the percentage of each brand's purchases made on sale"))
