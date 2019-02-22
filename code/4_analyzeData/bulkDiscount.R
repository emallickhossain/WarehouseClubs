# Computes bulk discount by module
library(data.table)
library(purrr)
library(furrr)
library(plotly)
library(lfe)
library(stringr)
library(fredr)
library(Hmisc)
library(stargazer)
fredr_set_key(fredAPI)
path <- "/home/mallick/Desktop/Nielsen/Data/Clean/"
yr <- 2004:2016

# top modules
topMod <- fread("./code/0_data/topModules.csv")
setorder(topMod, -pct)
topMod <- unique(topMod[, .(product_module_code, product_module_descr)])[1:20]

stores <- c("Grocery", "Warehouse Club", "Discount Store", "Dollar Store")

pce <- setDT(fredr("PCEPI", observation_start = as.Date("2004-01-01")))[, "series_id" := NULL]
pce[, ':=' (panel_year = as.integer(year(date)),
            month = as.integer(month(date)),
            date = NULL)]
setnames(pce, "value", "pce")

prod <- fread(paste0(path, "prod.csv"), key = c("upc", "upc_ver_uc"),
              select = c("upc", "upc_ver_uc", "product_module_code", "multi",
                         "size1_amount", "brand_code_uc", "size1_units",
                         "upc_descr"))[product_module_code %in% topMod$product_module_code]
prod[, c("size", "multi", "size1_amount") := .(multi * size1_amount, NULL, NULL)]
retailers <- fread(paste0(path, "retailers.tsv"))[channel_type %in% stores]
retailers[, "channel_type" := as.factor(channel_type)]
panel <- fread(paste0(path, "fullPanel.csv"))

getSize <- function(yr) {
  print(yr)
  purch <- fread(paste0(path, "Purchases/purchase", yr, ".csv"),
                 select = c("trip_code_uc", "upc", "upc_ver_uc", "quantity", "total_price_paid"),
                 key = c("upc", "upc_ver_uc"))
  purch <- merge(purch, prod, by = c("upc", "upc_ver_uc"))
  purch[, c("units", "size") := .(size * quantity, NULL)]

  purch <- purch[, .(spend = sum(total_price_paid),
                     units = sum(units),
                     quantity = sum(quantity)),
                 by = .(trip_code_uc, product_module_code, brand_code_uc)]
  purch[, c("unitCost", "spend") := .(spend / units, NULL)]

  trips <- fread(paste0(path, "Trips/trips", yr, ".csv"),
                 select = c("panel_year", "trip_code_uc", "retailer_code", "purchase_date"))
  trips[, ':=' (month = as.integer(substr(purchase_date, 6, 7)),
                purchase_date = NULL)]
  trips <- merge(trips, pce, by = c("panel_year", "month"))

  fullData <- merge(purch, trips, by = "trip_code_uc")
  fullData[, ':=' (unitCost = unitCost / pce * 100,
                   pce = NULL,
                   month = NULL)]
  return(fullData)
}

fullData <- rbindlist(map(yr, getSize))

getDisc <- function(mod) {
  print(mod)
  reg <- felm(data = fullData[product_module_code == mod],
              unitCost ~ units |
                as.factor(brand_code_uc) + as.factor(retailer_code) + as.factor(panel_year))
  coefs <- data.table(product_module_code = mod,
                      coefs = coef(reg)[1])
  return(coefs)
}
bulkDiscounts <- rbindlist(map(topMod$product_module_code, getDisc))
bulkDiscounts <- merge(bulkDiscounts, topMod, by = "product_module_code")

# Getting bulk discounts by various popular modules
# The total cost of an item is the unit cost times the size. To get unit cost,
# this is \beta * size + FEs. Hence, unit cost difference is \beta * sizeDiff.
# Total savings is this per-unit savings times the total package.
bulkDiscounts[product_module_code == 7260, coefs] * (24 - 12) * 24
bulkDiscounts[product_module_code == 7734, coefs] * (12 - 6) * 12
bulkDiscounts[product_module_code == 7870, coefs] * (8 - 4) * 8
bulkDiscounts[product_module_code == 7880, coefs] * (4 - 2) * 4
bulkDiscounts[product_module_code == 8404, coefs] * (12.8 - 6.4) * 12.8
bulkDiscounts[product_module_code == 3625, coefs] * (128 - 64) * 128
bulkDiscounts[product_module_code == 1484, coefs] * (144 - 72) * 144
bulkDiscounts[product_module_code == 3603, coefs] * (24 - 6) * 24
bulkDiscounts[product_module_code == 4000, coefs] * (24 - 16) * 24
bulkDiscounts[product_module_code == 4100, coefs] * (18 - 12) * 18

# TP Savings by pack size
sizes <- c(4, 6, 8, 12, 18, 24, 30, 36, 48)
graphData <- data.table(size = sizes,
                        savings = bulkDiscounts[product_module_code == 7260, coefs] * (sizes - 4) * 100)
chart <- plot_ly(data = graphData, x = ~size, height = 800, width = 1200) %>%
  add_bars(y = ~savings) %>%
  layout(title = "Annual Savings by Package Size",
         titlefont = list(size = 50),
         xaxis = list(title = "Package Size", titlefont = list(size = 40),
                      tickfont = list(size = 40), dtick = 4),
         yaxis = list(title = "Savings ($)", range = c(-40, 0), dtick = 10,
                      titlefont = list(size = 40), tickfont = list(size = 40)),
         # Adjust margins so things look nice
         margin = list(l = 150, r = 100, t = 80, b = 210, pad = 20),
         annotations = list(text = "Source: Author's calculations. Nielsen. <br>Note: Annual savings based on annual purchase of 100 rolls for a 2-person household.",
                            font = list(size = 25),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.4))
export(chart, file = "./code/5_figures/tpAnnualSavings.png")

getLogDisc <- function(mod) {
  print(mod)
  reg <- felm(data = fullData[product_module_code == mod],
              log(100 * unitCost) ~ log(units) |
                as.factor(brand_code_uc) + as.factor(retailer_code) + as.factor(panel_year))
  coefs <- data.table(product_module_code = mod,
                      coefs = coef(reg)[1])
  return(coefs)
}
bulkDiscountsLog <- rbindlist(map(topMod$product_module_code, getLogDisc))
bulkDiscountsLog <- merge(bulkDiscountsLog, topMod, by = "product_module_code")

bulkDiscountsLog[, "color" := ifelse(product_module_code %in% c(1177, 2672, 3590, 3603, 3608, 3625, 4000, 4100),
                                  "red", "blue")]

chart <- plot_ly(data = bulkDiscountsLog, x = ~product_module_descr, height = 800, width = 1200) %>%
  add_bars(y = ~coefs, marker = list(color = ~color)) %>%
  layout(title = "Bulk Discount by Module",
         titlefont = list(size = 35),
         xaxis = list(title = "Product Module", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "Discount", range = c(-0.5, 0), dtick = 0.1,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.7, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 120, r = 100, t = 60, b = 500, pad = 20),
         annotations = list(text = "Source: Nielsen. <br>Note: Regression controls for brand and retailer fixed effects. Red denotes perishables.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -1.8))
export(chart, "./code/5_figures/bulkDiscount.png")
