library(data.table)
library(purrr)
library(furrr)
library(plotly)
library(fredr)
library(Hmisc)
library(lfe)
library(stargazer)
fredr_set_key(fredAPI)
plan(multiprocess)
path <- "/home/mallick/Desktop/Nielsen/Data/Clean/"
yr <- 2004:2016

spendLim <- 300
spendLow <- 1

# Get PCE
pce <- setDT(fredr("PCEPI", observation_start = as.Date("2004-01-01"),
                   frequency = "a", aggregation_method = "avg"))
pce[, c("panel_year", "date", "series_id") := .(year(date), NULL, NULL)]
stores <- c("Grocery", "Warehouse Club", "Discount Store", "Dollar Store")

# Getting retailers
retailers <- fread(paste0(path, "retailers.tsv"))
retailers <- retailers[channel_type %in% stores]

panel <- fread(paste0(path, "fullPanel.csv"))

# Getting panel demographic data
getData <- function(yr) {
  print(yr)
  panel <- panel[panel_year == yr]

  # Getting Nielsen purchases by trip
  purch <- fread(paste0(path, "Purchases/purchase", yr, ".csv"),
                 select = c("trip_code_uc", "total_price_paid"))
  purch <- purch[, .(total_price_paid = sum(total_price_paid)), by = .(trip_code_uc)]

  # Getting trips
  trips <- fread(paste0(path, "Trips/trips", yr, ".csv"),
                 select = c("trip_code_uc", "household_code", "retailer_code",
                            "panel_year", "purchase_date"))

  # Merging with purchases and channels
  trips <- merge(trips, purch, by = "trip_code_uc")
  trips <- merge(trips, retailers, by = "retailer_code")
  trips <- merge(trips, panel, by = c("household_code", "panel_year"))
  return(trips)
}

fullData <- rbindlist(future_map(yr, getData))
fullData <- merge(fullData, pce, by = "panel_year")
fullData[, "realSpend" := total_price_paid / value * 100]
fullData <- fullData[realSpend >= spendLow & realSpend <= spendLim]
fullData[, "annualSpend" := sum(realSpend), by = .(household_code, panel_year)]
fullData[, ':=' (month = as.integer(substr(purchase_date, 6, 7)))]

# Monthly Trip regression (aggregate)
monthlyTrips <- fullData[, .(trips = uniqueN(trip_code_uc),
                             tripSpend = mean(realSpend),
                             monthlySpend = sum(realSpend)),
                         by = .(household_code, panel_year, month, household_income,
                                projection_factor, household_size, marital_status,
                                race, hispanic_origin, market, age, college)]

reg1 <- felm(data = monthlyTrips, log(trips) ~ household_income + log(monthlySpend) | as.factor(panel_year) +
               household_size + as.factor(marital_status) + as.factor(race) +
               as.factor(hispanic_origin) + as.factor(age) + as.factor(college) +
               market | 0 | market,
             weights = monthlyTrips$projection_factor)
reg2 <- felm(data = monthlyTrips, log(tripSpend) ~ household_income + log(monthlySpend) | as.factor(panel_year) +
               household_size + as.factor(marital_status) + as.factor(race) +
               as.factor(hispanic_origin) + as.factor(age) + as.factor(college) +
               market | 0 | market,
             weights = monthlyTrips$projection_factor)

# CV from Coibion
cv <- monthlyTrips[, .(cv = sd(monthlySpend) / mean(monthlySpend)),
                   by = .(household_code, panel_year, household_income,
                          projection_factor, household_size, marital_status,
                          race, hispanic_origin, market, age, college)]
aggCV <- cv[, .(cv = weighted.mean(cv, w = projection_factor, na.rm = TRUE)),
            by = .(panel_year, household_income)]
plot_ly(data = aggCV, x = ~panel_year) %>%
  add_lines(y = ~cv, split = ~household_income)
reg3 <- felm(data = cv, log(cv + 1) ~ household_income | as.factor(panel_year) +
              household_size + as.factor(marital_status) + as.factor(race) +
              as.factor(hispanic_origin) + as.factor(age) + as.factor(college) +
              market | 0 | market,
            weights = cv$projection_factor)

stargazer(reg1, reg2, reg3, type = "latex",
          add.lines = list(c("Demographics", "Y", "Y", "Y"),
                           c("Year FE", "Y", "Y", "Y"),
                           c("MSA FE", "Y", "Y", "Y")),
          single.row = FALSE, no.space = FALSE,
          dep.var.labels.include = TRUE,
          dep.var.labels = c("Log(Trips)", "Log(Trip Spending)", "Log(CV + 1)"),
          covariate.labels = c("25-50k", "50-100k", ">100k", "Log(MonthTotal)"),
          notes.align = "l",
          omit.stat = c("ser", "rsq"),
          order = c(2, 3, 1, 4),
          digits = 3,
          out = "./code/6_paper/tables/tripChanges.tex")

# Monthly Trip regression  by Channel
monthlyTrips <- fullData[, .(trips = uniqueN(trip_code_uc)),
                         by = .(household_code, panel_year, month, household_income,
                                projection_factor, household_size, marital_status,
                                race, hispanic_origin, market, age, college, channel_type)]

getYearFE <- function(type) {
  reg <- felm(data = monthlyTrips[channel_type == type], trips ~ household_income + as.factor(panel_year) |
                household_size + as.factor(marital_status) + as.factor(race) +
                as.factor(hispanic_origin) + as.factor(age) + as.factor(college) +
                market | 0 | market,
              weights = monthlyTrips[channel_type == type]$projection_factor)
  yearCoef <- data.table(year = 2005:2016, est = reg$coefficients[2:13], se = reg$se[2:13], channel_type = type)
  return(yearCoef)
}

coefs <- rbindlist(map(stores, getYearFE))

chart <- plot_ly(data = coefs, x = ~year, height = 800, width = 1200) %>%
  add_lines(y = ~est, error_y = ~list(value = se), line = list(width = 5),
            split = ~channel_type) %>%
  layout(title = "Average Monthly Shopping Trips (Year Effects)",
         titlefont = list(size = 35),
         annotations = list(text = "Source: Nielsen.",
                            font = list(size = 20),
                            showarrow = FALSE, align = "left", valign = "bottom",
                            xref = "paper", x = -0.03, yref = "paper", y = -0.22),
         xaxis = list(title = "Year",
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         yaxis = list(title = "Change in Trips", titlefont = list(size = 30), range = c(-2, 1),
                      tickfont = list(size = 25)),
         # Adjust margins so things look nice
         margin = list(l = 100, r = 100, t = 60, b = 150, pad = 10),
         legend = list(font = list(size = 20)))
export(chart, file = "./code/5_figures/tripYearFEs.png")

# Regression to get differences in per trip expenditures
reg <- felm(data = fullData, log(realSpend) ~ household_income + as.factor(panel_year) + annualSpend |
              household_size + as.factor(marital_status) + as.factor(race) +
              as.factor(hispanic_origin) + as.factor(age) + as.factor(college) +
              market | 0 | market,
            weights = fullData$projection_factor)
summary(reg)

coefs <- data.table(year = 2005:2016, est = reg$coefficients[2:13], se = reg$se[2:13])
chart <- plot_ly(data = coefs, x = ~year, height = 800, width = 1200) %>%
  add_lines(y = ~est, error_y = ~list(value = se), line = list(width = 5)) %>%
  layout(title = "Average Per Trip Expenditures (Year Effects)",
         titlefont = list(size = 35),
         annotations = list(text = "Source: Nielsen.",
                            font = list(size = 20),
                            showarrow = FALSE, align = "left", valign = "bottom",
                            xref = "paper", x = -0.03, yref = "paper", y = -0.22),
         xaxis = list(title = "Year",
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         yaxis = list(title = "Log Spending", titlefont = list(size = 30), range = c(0, 0.3),
                      tickfont = list(size = 25)),
         # Adjust margins so things look nice
         margin = list(l = 100, r = 100, t = 60, b = 150, pad = 10),
         legend = list(font = list(size = 20)))
export(chart, file = "./code/5_figures/logSpendingYearFEs.png")


# Aggregate Spending Density by income group
plotDensity <- function(yr) {
  medians <- fullData[panel_year == yr, .(median = wtd.quantile(realSpend, weights = as.numeric(projection_factor), probs = 0.5),
                        sd = sqrt(wtd.var(realSpend, weights = as.numeric(projection_factor)))),
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
    add_annotations(x = low$x[18], y = low$y[18], font = list(size = 20),
                    text = paste0("Median: ", round(medians[household_income == "<25k"]$median, 1),
                                  "<br>SD: ", round(medians[household_income == "<25k"]$sd, 1)),
                    xref = "x", yref = "y",
                    arrowhead = 0,
                    ax = 50, ay = -50) %>%
    add_annotations(x = high$x[31], y = high$y[31], font = list(size = 20),
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
           xaxis = list(title = "Spending ($2012)", range = c(0, 200),
                        titlefont = list(size = 30), tickfont = list(size = 25)),
           yaxis = list(title = "Density", titlefont = list(size = 30), range = c(0, 0.05),
                        tickfont = list(size = 25)),
           # Adjust margins so things look nice
           margin = list(l = 100, r = 100, t = 60, b = 150, pad = 10),
           legend = list(font = list(size = 20)))
  export(chart, file = paste0("./code/5_figures/tripSpending/", yr, ".png"))
}
for (i in 2004:2016) plotDensity(i)

# Histogram of monthly shopping trips
plotTrips <- function(yr) {
  medians <- monthlyTrips[panel_year == yr, .(median = wtd.quantile(trips, weights = as.numeric(projection_factor), probs = 0.5),
                             sd = sqrt(wtd.var(trips, weights = as.numeric(projection_factor)))),
                         by = household_income]

  low <- density(monthlyTrips[household_income == "<25k" & panel_year == yr]$trips,
                 weights = monthlyTrips[household_income == "<25k" & panel_year == yr]$projection_factor /
                   sum(monthlyTrips[household_income == "<25k" & panel_year == yr]$projection_factor))

  high <- density(monthlyTrips[household_income == ">100k" & panel_year == yr]$trips,
                  weights = monthlyTrips[household_income == ">100k" & panel_year == yr]$projection_factor /
                    sum(monthlyTrips[household_income == ">100k" & panel_year == yr]$projection_factor))

  chart <- plot_ly(height = 800, width = 1200, alpha = 0.6) %>%
    add_lines(x = ~low$x, y = ~low$y, name = "<25k", fill = "tozeroy") %>%
    add_lines(x = ~high$x, y = ~high$y, name = ">100k", fill = "tozeroy") %>%
    add_annotations(x = low$x[60], y = low$y[60], font = list(size = 20),
                    text = paste0("Median: ", round(medians[household_income == "<25k"]$median, 1),
                                  "<br>SD: ", round(medians[household_income == "<25k"]$sd, 1)),
                    xref = "x", yref = "y",
                    arrowhead = 0,
                    ax = 50, ay = -50) %>%
    add_annotations(x = high$x[67], y = high$y[67], font = list(size = 20),
                    text = paste0("Median: ", round(medians[household_income == ">100k"]$median, 1),
                                  "<br>SD: ", round(medians[household_income == ">100k"]$sd, 1)),
                    xref = "x", yref = "y",
                    arrowhead = 0,
                    ax = 40, ay = -80) %>%
    layout(title = paste0("Monthly Trip Density by Income (", yr, ")"),
           titlefont = list(size = 35),
           annotations = list(text = "Source: Nielsen.",
                              font = list(size = 20),
                              showarrow = FALSE, align = "left", valign = "bottom",
                              xref = "paper", x = -0.03, yref = "paper", y = -0.22),
           xaxis = list(title = "Spending ($2012)", range = c(0, 30),
                        titlefont = list(size = 30), tickfont = list(size = 25)),
           yaxis = list(title = "Density", titlefont = list(size = 30), range = c(0, 0.14),
                        tickfont = list(size = 25)),
           # Adjust margins so things look nice
           margin = list(l = 120, r = 100, t = 60, b = 150, pad = 10),
           legend = list(font = list(size = 20)))
  export(chart, file = paste0("./code/5_figures/tripDensity/", yr, ".png"))
}
for (i in 2004:2016) plotTrips(i)
bartlett.test(monthlyTrips$trips, monthlyTrips$household_income)
















# Aggregate spending histogram
histData <- fullData[, .(realSpending = sum(realSpending),
                         count = sum(count)),
                     by = .(household_code, panel_year, month, household_income)]
histData[realSpending > 1000, "realSpending" := 1000]
plot_ly(data = histData, alpha = 0.6, height = 800, width = 1200) %>%
  add_histogram(x = ~realSpending, split = ~household_income, histnorm = "probability") %>%
  layout(barmode = "overlay",
         xaxis = list(title = "Spending ($2012) ", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "Probability", range = c(0, 0.03), dtick = 0.01,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Adjust margins so things look nice
         margin = list(l = 80, r = 100, t = 60, b = 150, pad = 10),
         title = "Distribution of Real Expenditures by Income (2004-2016)",
         titlefont = list(size = 35),
         annotations = list(text = "Source: Nielsen.",
                            font = list(size = 20),
                            showarrow = FALSE, align = "left", valign = "bottom",
                            xref = "paper", x = -0.03, yref = "paper", y = -0.22))

# Spending Distribution by Channel Type
fullData[realSpending > 1000, "realSpending" := 1000]
fullData[, "household_income" := ordered(household_income,
                                         levels = c("<25k", "25-50k", "50-100k", ">100k"))]
fullData[, "totalHouseholdMonths" := .N / 4, by = household_income]

# Grocery
zeroSpend <- unique(fullData[realSpending == 0, .(share = .N / totalHouseholdMonths * 100),
                             by = .(household_income, channel_type)])

getCharts <- function(storeType) {
  chart1 <- plot_ly(data = fullData[channel_type == storeType & realSpending > 0],
          alpha = 0.6, height = 800, width = 1200) %>%
    add_histogram(x = ~realSpending, split = ~household_income, histnorm = "probability") %>%
    layout(barmode = "overlay",
           xaxis = list(title = "Spending ($2012) ", range = c(0, 500), titlefont = list(size = 30),
                        tickfont = list(size = 25)),
           yaxis = list(title = "Probability",
                        titlefont = list(size = 30), tickfont = list(size = 25)),
           # Adjust margins so things look nice
           margin = list(l = 80, r = 100, t = 60, b = 150, pad = 10),
           title = "Distribution of Real Expenditures by Income (2004-2016)",
           titlefont = list(size = 35),
           annotations = list(text = "Source: Nielsen.",
                              font = list(size = 20),
                              showarrow = FALSE, align = "left", valign = "bottom",
                              xref = "paper", x = -0.03, yref = "paper", y = -0.22))

  chart2 <- plot_ly(data = zeroSpend[channel_type == storeType], x = ~household_income,
          height = 800, width = 1200) %>%
    add_bars(y = ~share) %>%
    layout(xaxis = list(title = "Income", titlefont = list(size = 30),
                        tickfont = list(size = 25)),
           yaxis = list(title = "Share", range = c(0, 100), dtick = 20,
                        titlefont = list(size = 30), tickfont = list(size = 25)),
           # Adjust margins so things look nice
           margin = list(l = 80, r = 100, t = 60, b = 150, pad = 10),
           title = "Zero Spending by Income (2004-2016)",
           titlefont = list(size = 35),
           annotations = list(text = "Source: Nielsen.",
                              font = list(size = 20),
                              showarrow = FALSE, align = "left", valign = "bottom",
                              xref = "paper", x = -0.03, yref = "paper", y = -0.22))
  return(list(zero = chart2, hist = chart1))
}

warehouse <- getCharts("Warehouse Club")
groc <- getCharts("Grocery")
discount <- getCharts("Discount Store")
dollar <- getCharts("Dollar Store")

chart <- subplot(warehouse, margin = 0.05)
export(chart, "./code/5_figures/warehouseLumpiness.png")

chart <- subplot(groc, margin = 0.05)
export(chart, "./code/5_figures/groceryLumpiness.png")

chart <- subplot(discount, margin = 0.05)
export(chart, "./code/5_figures/discountLumpiness.png")

chart <- subplot(dollar, margin = 0.05)
export(chart, "./code/5_figures/dollarLumpiness.png")
