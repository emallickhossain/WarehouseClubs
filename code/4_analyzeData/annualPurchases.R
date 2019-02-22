# Computes annual purchases by module
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
wrds <- 1
path <- ifelse(wrds == 1, "/scratch/upenn/hossaine/", "/home/mallick/Desktop/Nielsen/Data/Clean/")

yr <- 2004:2016

# top modules
topMod <- fread("./code/0_data/topModules.csv")
setorder(topMod, -pct)
topMod <- unique(topMod[, .(product_module_code, product_module_descr)])[1:20]
topMod[, "desc" := c("Bread", "Milk", "Toilet Paper", "Eggs", "Cookies", "Cereal",
                     "Soup", "Soda", "Chocolate", "Chips", "Batteries", "Paper Towels",
                     "Ice Cream", "Toothpaste", "Candy", "Cheese", "Yogurt",
                     "Butter", "Lightbulbs", "Dressing")]

stores <- c("Grocery", "Warehouse Club", "Discount Store", "Dollar Store")

pce <- setDT(fredr("PCEPI", observation_start = as.Date("2004-01-01")))[, "series_id" := NULL]
pce[, ':=' (panel_year = as.integer(year(date)),
            month = as.integer(month(date)),
            date = NULL)]
setnames(pce, "value", "pce")

prod <- fread(paste0(path, "prod.csv"), key = c("upc", "upc_ver_uc"),
              select = c("upc", "upc_ver_uc", "product_module_code", "multi",
                         "size1_amount"))[product_module_code %in% topMod$product_module_code]
prod[, c("size", "multi", "size1_amount") := .(multi * size1_amount, NULL, NULL)]
retailers <- fread(paste0(path, "retailers.tsv"))[channel_type %in% stores]
retailers[, "channel_type" := as.factor(channel_type)]
panel <- fread(paste0(path, "fullPanel.csv"))
panel[, "household_income" := factor(household_income,
                                     levels = c("<25k", "25-50k", "50-100k", ">100k"),
                                     ordered = TRUE)]

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
                 by = .(trip_code_uc, product_module_code)]

  trips <- fread(paste0(path, "Trips/trips", yr, ".csv"),
                 select = c("panel_year", "household_code", "trip_code_uc", "retailer_code", "purchase_date"))
  trips[, ':=' (month = as.integer(substr(purchase_date, 6, 7)),
                purchase_date = NULL)]
  trips <- merge(trips, pce, by = c("panel_year", "month"))

  fullData <- merge(purch, trips, by = "trip_code_uc")
  fullData[, ':=' (realSpend = spend / pce * 100,
                   pce = NULL,
                   month = NULL,
                   spend = NULL)]
  fullData <- fullData[, .(realSpend = sum(realSpend),
                           units = sum(units),
                           quantity = sum(quantity)),
                       by = .(household_code, panel_year, product_module_code, retailer_code)]
  return(fullData)
}

fullData <- rbindlist(map(yr, getSize))
fullData <- merge(fullData, panel, by = c("household_code", "panel_year"))
fullData <- merge(fullData, retailers, by = "retailer_code")
fullData[, ':=' (totalSpend = sum(realSpend),
                 totalUnits = sum(units)),
         by = .(household_code, panel_year, product_module_code)]

# Looking at annual spending
annualData <- fullData[, .(realSpend = sum(realSpend),
                           units = sum(units),
                           quantity = sum(quantity)),
                       by = .(household_code, panel_year, product_module_code)]
annualData <- merge(annualData, panel, by = c("household_code", "panel_year"))

getAnnualUnits <- function(mod) {
  reg <- felm(data = annualData[product_module_code == mod], log(units) ~ household_income |
                as.factor(panel_year) +
                household_size + as.factor(marital_status) +
                as.factor(race) + as.factor(hispanic_origin) + market + age +
                as.factor(college) + as.factor(urban) | 0 | market,
              weights = annualData[product_module_code == mod]$projection_factor)
  coefs <- data.table(income = names(coef(reg)),
                      coef = coef(reg),
                      se = reg$cse,
                      product_module_code = mod)
  return(coefs)
}

allUnits <- rbindlist(map(topMod$product_module_code, getAnnualUnits))
allUnits <- merge(allUnits, topMod, by = "product_module_code")
allUnits[, "stars" := ifelse(abs(coef / se) > 2, "*", "")]
allUnits[, "desc" := factor(desc,
                             levels = unique(allUnits[income == ">100k"]$desc)[order(allUnits[income == ">100k"]$coef, decreasing = TRUE)])]

chart <- plot_ly(data = allUnits, x = ~desc, height = 800, width = 1200) %>%
  add_bars(y = ~coef, error_y = list(array = ~1.96 * se, color = "#000000"), split = ~income) %>%
  layout(title = "Differences in Annual Units Purchased by Module",
         titlefont = list(size = 35),
         xaxis = list(title = "Product Module", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "Percent Difference", range = c(-0.5, 0.7), dtick = 0.1,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.7, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 120, r = 100, t = 60, b = 300, pad = 20),
         annotations = list(text = "Source: Nielsen. <br>Note: Regression controls for household demographics. 95% error bars are shown.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.58))
export(chart, file = "./code/5_figures/annualUnitPurchaseIncome.png")

getAnnualSpend <- function(mod) {
  reg <- felm(data = annualData[product_module_code == mod], log(realSpend) ~ household_income |
                as.factor(panel_year) +
                household_size + as.factor(marital_status) +
                as.factor(race) + as.factor(hispanic_origin) + market + age +
                as.factor(college) + as.factor(urban) | 0 | market,
              weights = annualData[product_module_code == mod]$projection_factor)
  coefs <- data.table(income = names(coef(reg)),
                      coef = coef(reg),
                      se = reg$cse,
                      product_module_code = mod)
  return(coefs)
}

allSpend <- rbindlist(map(topMod$product_module_code, getAnnualSpend))
allSpend <- merge(allSpend, topMod, by = "product_module_code")
allSpend[, "stars" := ifelse(abs(coef / se) > 2, "*", "")]
allSpend[, "desc" := factor(desc,
                            levels = unique(allUnits[income == ">100k"]$desc)[order(allUnits[income == ">100k"]$coef, decreasing = TRUE)])]

plot_ly(data = allSpend, x = ~desc, height = 800, width = 1200) %>%
  add_bars(y = ~coef, error_y = list(array = ~1.96 * se, color = "#000000"), split = ~income) %>%
  layout(title = "Differences in Annual Spending by Module",
         titlefont = list(size = 35),
         xaxis = list(title = "Product Module", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "Percent Difference", range = c(-0.5, 0.7), dtick = 0.1,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.7, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 120, r = 100, t = 60, b = 300, pad = 20),
         annotations = list(text = "Source: Nielsen. <br>Note: Regression controls for household demographics. 95% error bars are shown.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.58))

# Getting units purchased by channel
getAnnualSpend <- function(mod) {
  reg <- felm(data = annualData[product_module_code == mod], log(realSpend) ~ household_income |
                as.factor(panel_year) +
                household_size + as.factor(marital_status) +
                as.factor(race) + as.factor(hispanic_origin) + market + age +
                as.factor(college) + as.factor(urban) | 0 | market,
              weights = annualData[product_module_code == mod]$projection_factor)
  coefs <- data.table(income = names(coef(reg)),
                      coef = coef(reg),
                      se = reg$cse,
                      product_module_code = mod)
  return(coefs)
}

allSpend <- rbindlist(map(topMod$product_module_code, getAnnualSpend))
allSpend <- merge(allSpend, topMod, by = "product_module_code")
allSpend[, "stars" := ifelse(abs(coef / se) > 2, "*", "")]
