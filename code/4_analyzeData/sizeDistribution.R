# Generates distribution of purchase sizes for modules

#### Cleaning products file
# Removing "to-go" toilet paper packs
# Removing any UPCs for which there are more than 96 rolls in a single package

#### Cleaning purchases
# I keep households that buy at least 6 rolls and less than 365 rolls in a year
# I only look at single package purchases (~85% of trips)
# I only keep unit costs that are between $0.2 and $2 per roll

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
plan(multiprocess)
path <- "/home/mallick/Desktop/Nielsen/Data/Clean/"
yr <- 2004:2016
modCode <- 7260
sizeUnadjLim <- 96

upperPackages <- 2
annualUnitLim <- 365
annualUnitLow <- 6
unitCostLim <- 2
unitCostLow <- 0.2

stores <- c("Grocery", "Warehouse Club", "Discount Store", "Dollar Store")

pce <- setDT(fredr("PCEPI", observation_start = as.Date("2004-01-01"),
                   frequency = "a", aggregation_method = "avg"))
pce[, "panel_year" := year(date)]

prod <- fread(paste0(path, "prod.csv"),
              select = c("upc", "upc_ver_uc", "upc_descr", "product_module_code", "multi",
                         "size1_amount", "size1_units", "brand_code_uc", "brand_descr"))
prod <- prod[product_module_code == modCode]
prod <- prod[!grep("TO GO", brand_descr)]

# Fixing TP brand codes
prod[grep("CHARMIN", brand_descr), "brand_code_uc" := 526996]
prod[grep("COTTONELLE", brand_descr), "brand_code_uc" := 581898]
prod[grep("MARCAL", brand_descr), "brand_code_uc" := 593633]
prod[grep("LILY'S", brand_descr), "brand_code_uc" := 588581]
prod[grep("PERT", brand_descr), "brand_code_uc" := 616644]
prod[grep("^SCOTT", brand_descr), "brand_code_uc" := 635073]
prod[grep("SIMPLY", brand_descr), "brand_code_uc" := 638408]

# For Toilet paper, creating standardized roll
prod[, "ply" := str_extract_all(upc_descr, "\\s\\dP\\s")]
prod[, "ply" := as.integer(gsub("P", "", ply))]
prod[, "sheet" := str_extract_all(upc_descr, "\\d{2,}S\\s")]
prod[, "sheet" := as.integer(gsub("S", "", sheet))]
prod[, "stdRolls" := ply * sheet / 500]
prod[, "size" := multi * size1_amount * stdRolls]
prod[, "sizeUnadj" := multi * size1_amount]
prod <- prod[sizeUnadj <= sizeUnadjLim]

retailers <- fread(paste0(path, "retailers.tsv"))[channel_type %in% stores]
panel <- fread(paste0(path, "fullPanel.csv"))[household_income %in% c("<25k", ">100k")]

runReg <- function(spec, brand, store, title, data) {
  fullData <- data
  reg <- list()
  for (i in 1:length(spec)) {
    reg[[i]] <- felm(data = fullData, as.formula(spec[i]), weights = fullData$projection_factor)
  }
  stargazer(reg, type = "text",
            add.lines = list(c("Brand FE", brand),
                             c("Store FE", store)),
            single.row = FALSE, no.space = FALSE,
            dep.var.labels.include = FALSE,
            dep.var.caption = paste(title),
            covariate.labels = c("units"),
            notes.align = "l",
            omit.stat = c("ser", "rsq"),
            digits = 3)
}

getSize <- function(yr) {
  purch <- fread(paste0(path, "Purchases/purchase", yr, ".csv"))
  purch <- merge(purch, prod, by = c("upc", "upc_ver_uc"))
  purch[, "units" := size * quantity]
  purch[, "unitsUnadj" := sizeUnadj * quantity]

  purch <- purch[, .(units = sum(units),
                     unitsUnadj = sum(unitsUnadj),
                     packages = sum(quantity),
                     spend = sum(total_price_paid)),
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
                 annualUnits = sum(units),
                 annualUnitsUnadj = sum(unitsUnadj)),
         by = .(household_code, panel_year)]

# Computing relevant statistics
fullData[, "realSpend" := spend / value * 100]
fullData[, "unitCost" := realSpend / units]
fullData[, "annualRealSpend" := sum(realSpend), by = .(household_code, panel_year)]

# Cleaning
fullData <- fullData[packages <= upperPackages &
                     annualUnits >= annualUnitLow & annualUnits <= annualUnitLim &
                       unitCost >= unitCostLow & unitCost <= unitCostLim]
cols <- c("panel_year", "retailer_code", "brand_code_uc", "household_income",
          "household_size", "marital_status", "race", "hispanic_origin", "market",
          "age", "college")
fullData[, (cols) := lapply(.SD, as.factor), .SDcols = cols]

# Making generic adjustment since all generics are not the same and depend by store type
fullData[, "brand_code_uc" := as.character(brand_code_uc)]
fullData[brand_code_uc == 536746, "brand_code_uc" := paste0(brand_code_uc, channel_type)]

# Packages purchased (doesn't depend on number of rolls, just how many things are thrown in the cart)
plot_ly(data = fullData, alpha = 0.6) %>%
  add_histogram(x = ~packages, split = ~household_income, histnorm = "probability")

reg <- felm(data = fullData, packages ~ household_income |
              panel_year + market +
              retailer_code +
              brand_code_uc +
              household_size + marital_status + race + hispanic_origin + age + college | 0 | market,
            weights = fullData$projection_factor)

# Getting number of rolls purchased (standardized rolls)
medians <- fullData[, .(median = wtd.quantile(units, weights = projection_factor, probs = 0.5),
                        sd = sqrt(wtd.var(units, weights = projection_factor))),
                    by = household_income]
low <- density(fullData[household_income == "<25k"]$units,
               weights = fullData[household_income == "<25k"]$projection_factor /
                         sum(fullData[household_income == "<25k"]$projection_factor))

high <- density(fullData[household_income == ">100k"]$units,
                weights = fullData[household_income == ">100k"]$projection_factor /
                  sum(fullData[household_income == ">100k"]$projection_factor))

chart <- plot_ly(height = 800, width = 1200, alpha = 0.6) %>%
  add_lines(x = ~low$x, y = ~low$y, name = "<25k", fill = "tozeroy") %>%
  add_lines(x = ~high$x, y = ~high$y, name = ">100k", fill = "tozeroy") %>%
  add_annotations(x = low$x[22], y = low$y[22], font = list(size = 20),
                  text = paste0("Median: ", round(medians[household_income == "<25k"]$median, 1),
                                "<br>SD: ", round(medians[household_income == "<25k"]$sd, 1)),
                  xref = "x", yref = "y",
                  arrowhead = 0,
                  ax = 40, ay = -50) %>%
  add_annotations(x = high$x[45], y = high$y[45], font = list(size = 20),
                  text = paste0("Median: ", round(medians[household_income == ">100k"]$median, 1),
                                "<br>SD: ", round(medians[household_income == ">100k"]$sd, 1)),
                  xref = "x", yref = "y",
                  arrowhead = 0,
                  ax = 40, ay = -80) %>%
  layout(title = paste0("Per Trip Unit Purchases"),
         titlefont = list(size = 35),
         annotations = list(text = "Source: Nielsen.",
                            font = list(size = 20),
                            showarrow = FALSE, align = "left", valign = "bottom",
                            xref = "paper", x = -0.03, yref = "paper", y = -0.22),
         xaxis = list(title = "Units (Standardized)", range = c(0, 60),
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         yaxis = list(title = "Density", titlefont = list(size = 30), range = c(0, 0.1),
                      tickfont = list(size = 25)),
         # Adjust margins so things look nice
         margin = list(l = 100, r = 100, t = 60, b = 150, pad = 10),
         legend = list(font = list(size = 20)))
export(chart, file = "./code/5_figures/tpSizeDensity.png")

# Adjusting for household demographics
runReg(c("log(units) ~ household_income + college | panel_year + market +
              household_size + marital_status + race + hispanic_origin + age | 0 | market",
         "log(units) ~ household_income + college | panel_year + market +
              retailer_code +
              household_size + marital_status + race + hispanic_origin + age | 0 | market",
         "log(units) ~ household_income + college | panel_year + market +
              brand_code_uc +
              household_size + marital_status + race + hispanic_origin + age | 0 | market",
         "log(units) ~ household_income + college | panel_year + market +
              retailer_code +
              brand_code_uc +
              household_size + marital_status + race + hispanic_origin + age | 0 | market"),
       brand = c("N", "N", "Y", "Y"), store = c("N", "Y", "N", "Y"), title = "Log Units", data = fullData)

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
           yaxis = list(title = "Density", titlefont = list(size = 30), range = c(0, 0.25),
                        tickfont = list(size = 25)),
           # Adjust margins so things look nice
           margin = list(l = 100, r = 100, t = 60, b = 150, pad = 10),
           legend = list(font = list(size = 20)))
  export(chart, file = paste0("./code/5_figures/tripSpending/tp", yr, ".png"))
}
for (i in 2004:2016) plotDensity(i)

# Getting number of trips
tripCount <- unique(fullData[, .(household_code, panel_year, tripCount, college,
                                 household_income, market, household_size,
                                 marital_status, race, hispanic_origin, age, projection_factor)])
getTrips <- function(yr) {
  chart <- plot_ly(data = tripCount[tripCount < 30 & panel_year == yr], alpha = 0.6) %>%
    add_histogram(x = ~tripCount, split = ~household_income, histnorm = "probability") %>%
    layout(barmode = "overlay", yaxis = list(range = c(0, 0.16)))
  export(chart, file = paste0("./code/5_figures/tripDensity/tp", yr, ".png"))
}
for (i in 2004:2016) getTrips(i)

runReg(c("log(tripCount) ~ household_income + college | panel_year + market +
              household_size + marital_status + race + hispanic_origin + age | 0 | market"),
       brand = "NA", store = "NA", title = "Annual Trips", data = tripCount)

# Getting total units purchased
annualUnits <- unique(fullData[, .(household_code, panel_year, annualUnits,
                                   household_income, market, household_size,
                                   marital_status, race, hispanic_origin, age, college, projection_factor)])

runReg(c("log(annualUnits) ~ household_income + college | panel_year + market +
              household_size + marital_status + race + hispanic_origin + age | 0 | market"),
       store = "NA", brand = "NA", title = "Annual Units", data = annualUnits)

# Getting annual spending
annualSpend <- unique(fullData[, .(household_code, panel_year, annualRealSpend,
                                   household_income, market, household_size, college,
                                   marital_status, race, hispanic_origin, age, projection_factor)])

runReg(c("log(annualRealSpend) ~ household_income + college | panel_year + market +
              household_size + marital_status + race + hispanic_origin + age | 0 | market"),
       brand = "NA", store = "NA", title = "Log Annual Spending", data = annualSpend)

# Unit costs
runReg(c("log(unitCost) ~ household_income + college | panel_year + market +
              household_size + marital_status + race + hispanic_origin + age | 0 | market",
         "log(unitCost) ~ household_income + college | panel_year + market +
              retailer_code +
              household_size + marital_status + race + hispanic_origin + age | 0 | market",
         "log(unitCost) ~ household_income + college | panel_year + market +
              brand_code_uc +
              household_size + marital_status + race + hispanic_origin + age | 0 | market",
         "log(unitCost) ~ household_income + college | panel_year + market +
              retailer_code +
              brand_code_uc +
              household_size + marital_status + race + hispanic_origin + age | 0 | market"),
       store = c("N", "Y", "N", "Y"), brand = c("N", "N", "Y", "Y"), title = "Log Unit Cost", data = fullData)

# Bulk Discount
runReg(c("log(unitCost) ~ log(units) | panel_year + market | 0 | market",
         "log(unitCost) ~ log(units) | panel_year + market + brand_code_uc | 0 | market",
         "log(unitCost) ~ log(units) | panel_year + market + retailer_code | 0 | market",
         "log(unitCost) ~ log(units) | panel_year + market + retailer_code + brand_code_uc | 0 | market"),
       brand = c("N", "Y", "N", "Y"), store = c("N", "N", "Y", "Y"),
       title = "Log Unit Costs", data = fullData)
