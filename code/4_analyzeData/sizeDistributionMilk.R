# Generates distribution of purchase sizes for modules

library(data.table)
library(purrr)
library(furrr)
library(plotly)
library(lfe)
library(stringr)
library(fredr)
library(Hmisc)
fredr_set_key(fredAPI)
plan(multiprocess)
path <- "/home/mallick/Desktop/Nielsen/Data/Clean/"
yr <- 2004:2016
modCode <- 4100
sizeLim <- 200
unitLim <- 256
packageLim <- 5

stores <- c("Grocery", "Warehouse Club", "Discount Store", "Dollar Store")

pce <- setDT(fredr("PCEPI", observation_start = as.Date("2004-01-01"),
                   frequency = "a", aggregation_method = "avg"))
pce[, "panel_year" := year(date)]

prod <- fread(paste0(path, "prod.csv"),
              select = c("upc", "upc_ver_uc", "upc_descr", "product_module_code", "multi",
                         "size1_amount", "size1_units", "brand_code_uc", "brand_descr"))
prod <- prod[product_module_code == modCode]
prod <- prod[size1_amount <= sizeLim]
prod[, "size" := multi * size1_amount]

retailers <- fread(paste0(path, "retailers.tsv"))[channel_type %in% stores]
panel <- fread(paste0(path, "fullPanel.csv"),
               select = c("household_code", "panel_year", "household_income",
                          "projection_factor", "household_size", "marital_status",
                          "race", "hispanic_origin", "age", "market"))
panel <- panel[household_income %in% c("<25k", ">100k")]

getSize <- function(yr) {
  purch <- fread(paste0(path, "Purchases/purchase", yr, ".csv"))
  purch <- merge(purch, prod, by = c("upc", "upc_ver_uc"))
  purch[, "units" := size * quantity]

  purch <- purch[, .(units = sum(units),
                     packages = sum(quantity),
                     spend = sum(total_price_paid),
                     withCoupon = sum(total_price_paid - coupon_value)),
                 by = .(trip_code_uc, brand_code_uc)]

  trips <- fread(paste0(path, "Trips/trips", yr, ".csv"),
                 select = c("household_code", "panel_year", "trip_code_uc", "retailer_code"))

  fullData <- merge(purch, trips, by = "trip_code_uc")
  fullData <- merge(fullData, retailers, by = "retailer_code")
  return(fullData)
}

fullData <- rbindlist(future_map(yr, getSize))
fullData <- merge(fullData, panel, by = c("household_code", "panel_year"))
fullData <- merge(fullData, pce, by = "panel_year")
fullData[, ':=' (tripCount = uniqueN(trip_code_uc),
                 annualUnits = sum(units)),
         by = .(household_code, panel_year)]

# Computing relevant statistics
fullData[, "realSpend" := spend / value * 100]
fullData[, "realSpendWCoupon" := withCoupon / value * 100]
fullData[, "unitCost" := realSpend / units]
fullData[, "unitCostWCoupon" := realSpendWCoupon / units]
fullData[, "annualRealSpend" := sum(realSpend), by = .(household_code, panel_year)]
fullData[, "annualRealSpendWCoupon" := sum(realSpendWCoupon), by = .(household_code, panel_year)]

# Cleaning
fullData <- fullData[units <= unitLim & packages <= packageLim]

# Making generic adjustment since all generics are not the same and depend by store
fullData[, "brand_code_uc" := as.character(brand_code_uc)]
fullData[brand_code_uc == 536746, "brand_code_uc" := paste0(brand_code_uc, retailer_code)]

# Packages purchased (doesn't depend on number of rolls, just how many things are thrown in the cart)
plot_ly(data = fullData, alpha = 0.6) %>%
  add_histogram(x = ~packages, split = ~household_income, histnorm = "probability")

reg <- felm(data = fullData, packages ~ household_income |
              as.factor(panel_year) +
              as.factor(market) +
              as.factor(retailer_code) +
              as.factor(brand_code_uc) +
              as.factor(household_size) + as.factor(marital_status) + as.factor(race) +
              as.factor(hispanic_origin) + as.factor(age) | 0 | market,
            weights = fullData$projection_factor)

# Getting number of units purchased
plot_ly(data = fullData, alpha = 0.6) %>%
  add_histogram(x = ~units, split = ~household_income, histnorm = "probability") %>%
  layout(barmode = "overlay")

reg <- felm(data = fullData, units ~ household_income |
              as.factor(panel_year) +
              as.factor(market) +
              as.factor(retailer_code) +
              as.factor(brand_code_uc) +
              as.factor(household_size) + as.factor(marital_status) + as.factor(race) +
              as.factor(hispanic_origin) + as.factor(age) | 0 | market,
            weights = fullData$projection_factor)

# Boxplots for spending to look at variances
chart <- plot_ly(data = fullData, height = 800, width = 1200) %>%
  add_boxplot(x = ~realSpend, split = ~household_income,
              marker = list(opacity = 0.25), jitter = 0.25) %>%
  layout(title = "Per Trip Spending (2004-2016)",
         titlefont = list(size = 35),
         annotations = list(text = "Source: Nielsen.",
                            font = list(size = 20),
                            showarrow = FALSE, align = "left", valign = "bottom",
                            xref = "paper", x = -0.03, yref = "paper", y = -0.22),
         xaxis = list(title = "Spending ($2012)", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         # Adjust margins so things look nice
         margin = list(l = 80, r = 100, t = 60, b = 150, pad = 10))
export(chart, file = "./code/5_figures/tpSpendingBoxPlot.png")
bartlett.test(fullData$realSpend, fullData$household_income)

# Trip spending density
plotDensity <- function(yr) {
  medians <- fullData[panel_year == yr, .(median = wtd.quantile(realSpend, weights = projection_factor, probs = 0.5),
                                          sd = sqrt(wtd.var(realSpend, weights = projection_factor))),
                      by = household_income]
  low <- density(fullData[household_income == "<25k" & panel_year == yr]$realSpend,
                 weights = fullData[household_income == "<25k" & panel_year == yr]$projection_factor /
                   sum(fullData[household_income == "<25k" & panel_year == yr]$projection_factor))

  high <- density(fullData[household_income == ">100k" & panel_year == yr]$realSpend,
                  weights = fullData[household_income == ">100k" & panel_year == yr]$projection_factor /
                    sum(fullData[household_income == ">100k" & panel_year == yr]$projection_factor))

  chart <- plot_ly(height = 800, width = 1200, alpha = 0.6) %>%
    add_lines(x = ~low$x, y = ~low$y, name = "<25k", fill = "tozeroy") %>%
    add_lines(x = ~high$x, y = ~high$y, name = ">100k", fill = "tozeroy") %>%
    add_annotations(x = low$x[33], y = low$y[33], font = list(size = 20),
                    text = paste0("Median: ", round(medians[household_income == "<25k"]$median, 1),
                                  "<br>SD: ", round(medians[household_income == "<25k"]$sd, 1)),
                    xref = "x", yref = "y",
                    arrowhead = 0,
                    ax = 40, ay = -50) %>%
    add_annotations(x = high$x[70], y = high$y[70], font = list(size = 20),
                    text = paste0("Median: ", round(medians[household_income == ">100k"]$median, 1),
                                  "<br>SD: ", round(medians[household_income == ">100k"]$sd, 1)),
                    xref = "x", yref = "y",
                    arrowhead = 0,
                    ax = 40, ay = -80) %>%
    layout(title = paste0("Per Trip Spending Density by Income (", yr, ")"),
           titlefont = list(size = 35),
           annotations = list(text = "Source: Nielsen.",
                              font = list(size = 20),
                              showarrow = FALSE, align = "left", valign = "bottom",
                              xref = "paper", x = -0.03, yref = "paper", y = -0.22),
           xaxis = list(title = "Spending ($2012)", range = c(0, 25),
                        titlefont = list(size = 30), tickfont = list(size = 25)),
           yaxis = list(title = "Density", titlefont = list(size = 30), range = c(0, 0.5),
                        tickfont = list(size = 25)),
           # Adjust margins so things look nice
           margin = list(l = 100, r = 100, t = 60, b = 150, pad = 10),
           legend = list(font = list(size = 20)))
  export(chart, file = paste0("./code/5_figures/tripSpending/milk", yr, ".png"))
}
for (i in 2004:2016) plotDensity(i)

# Getting number of trips
tripCount <- unique(fullData[, .(household_code, panel_year, tripCount,
                                 household_income, market, household_size,
                                 marital_status, race, hispanic_origin, age, projection_factor)])
getTrips <- function(yr) {
  chart <- plot_ly(data = tripCount[panel_year == yr & tripCount < 100], alpha = 0.6) %>%
    add_histogram(x = ~tripCount, split = ~household_income, histnorm = "probability") %>%
    layout(barmode = "overlay", yaxis = list(range = c(0, 0.1)))
  export(chart, file = paste0("./code/5_figures/tripDensity/milk", yr, ".png"))
}
for (i in 2004:2016) getTrips(i)
reg <- felm(data = tripCount, tripCount ~ household_income |
              as.factor(panel_year) +
              as.factor(market) +
              as.factor(household_size) + as.factor(marital_status) + as.factor(race) +
              as.factor(hispanic_origin) + as.factor(age) | 0 | market,
            weights = tripCount$projection_factor)

# Getting total units purchased
# Doing hh size adjustment
annualUnits <- unique(fullData[, .(household_code, panel_year, annualUnits,
                                   household_income, market, household_size,
                                   marital_status, race, hispanic_origin, age, projection_factor)])
reg <- felm(data = annualUnits, annualUnits ~ household_income |
              as.factor(panel_year) +
              as.factor(market) +
              as.factor(household_size) + as.factor(marital_status) + as.factor(race) +
              as.factor(hispanic_origin) + as.factor(age) | 0 | market,
            weights = annualUnits$projection_factor)

# Getting annual spending
annualSpend <- unique(fullData[, .(household_code, panel_year, annualRealSpend, annualRealSpendWCoupon,
                                   household_income, market, household_size,
                                   marital_status, race, hispanic_origin, age, projection_factor)])
reg <- felm(data = annualSpend, annualRealSpend ~ household_income |
              as.factor(panel_year) +
              as.factor(market) +
              as.factor(household_size) + as.factor(marital_status) + as.factor(race) +
              as.factor(hispanic_origin) + as.factor(age) | 0 | market,
            weights = annualSpend$projection_factor)

# Unit costs
reg <- felm(data = fullData, unitCost ~ household_income + annualUnits |
              as.factor(panel_year) +
              as.factor(market) +
              as.factor(retailer_code) +
              as.factor(brand_code_uc) +
              as.factor(household_size) + as.factor(marital_status) + as.factor(race) +
              as.factor(hispanic_origin) + as.factor(age) | 0 | market,
            weights = fullData$projection_factor)
