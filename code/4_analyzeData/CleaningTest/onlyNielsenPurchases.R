# This gets all Nielsen purchases in aggregate and by department
library(data.table)
library(lubridate)
library(plotly)
library(fredr)
library(purrr)
library(furrr)
plan(multiprocess)
fredr_set_key(fredAPI)
yr <- 2004:2016
path <- "/home/mallick/Desktop/Nielsen/Data/Clean/"

# Get PCE
pce <- setDT(fredr("PCEPI", observation_start = as.Date("2004-01-01")))[, "series_id" := NULL]
pce[, "date" := as.POSIXct(date)]

# Get products
prod <- fread(paste0(path, "prod.csv"), select = c("upc", "upc_ver_uc", "department_code"))
panel <- fread(paste0(path, "fullPanel.csv"))

# Getting data for only Nielsen coded trips
getChart34 <- function(yr) {

  # Getting Nielsen purchases by department and trip
  purch <- fread(paste0(path, "Purchases/purchase", yr, ".csv"),
                 select = c("trip_code_uc", "upc", "upc_ver_uc", "total_price_paid"))
  purch <- merge(purch, prod, by = c("upc", "upc_ver_uc"))
  purch <- purch[, .(total_price_paid = sum(total_price_paid)), by = .(trip_code_uc, department_code)]

  # Getting trip data
  trips <- fread(paste0(path, "Trips/trips", yr, ".csv"),
                 select = c("trip_code_uc", "household_code", "panel_year", "purchase_date"))
  trips[, "purchase_date" := parse_date_time2(paste0(substr(purchase_date, 1, 8), "01"), "%Y-%m-%d")]
  setnames(trips, "purchase_date", "date")

  # Getting full data
  fullData <- merge(trips, purch, by = "trip_code_uc")

  # Getting monthly spending by department and household
  fullData <- fullData[, .(monthlySpend = sum(total_price_paid)),
                       by = .(household_code, panel_year, date, department_code)]
  return(fullData)
}
graphData34 <- rbindlist(future_map(yr, getChart34))
graphData34 <- merge(graphData34, panel, by = c("household_code", "panel_year"))

# Constructing chart 3 (only Nielsen purchases)
graphData3 <- graphData34[, .(monthlySpend = sum(monthlySpend)),
                          by = .(household_code, panel_year, date,
                                 household_income, projection_factor)]
graphData3 <- graphData3[, .(monthlySpend = weighted.mean(monthlySpend, w = projection_factor)),
                         by = .(date, household_income)]
graphData3 <- merge(graphData3, pce, by = "date")
graphData3[, "monthlySpendReal" := monthlySpend / value * 100]
setkey(graphData3, household_income, date)
graphData3[, "rollSpend" := zoo::rollmean(monthlySpendReal, 12,
                                          fill = c(NA, NULL, NA), align = "right"),
           by = .(household_income)]

# Getting changes
ann <- graphData3[date %in% parse_date_time(c("2004-12-01", "2016-12-01"), "%Y-%m-%d")]
cha <- ann[, .(change = diff(rollSpend) / head(rollSpend, 1) * 100), by = .(household_income)]

# Plotting
chart3 <- plot_ly(data = graphData3, x = ~date, width = 1200, height = 800) %>%
  add_lines(y = ~rollSpend, split = ~household_income, line = list(width = 5)) %>%
  add_annotations(x = ann[year(date) == 2016]$date,
                  y = ann[year(date) == 2016]$rollSpend,
                  text = paste0(round(cha$change), "%"),
                  font = list(size = 20),
                  xref = "x",
                  yref = "y",
                  showarrow = TRUE,
                  ax = 60,
                  ay = 0) %>%
  layout(title = "Real Expenditures by Income, Nielsen Purchases (2004-2016)",
         titlefont = list(size = 35),
         xaxis = list(title = "Year", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "Real Expenditures ($2012)", range = c(0, 1000), dtick = 200,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.9, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 100, r = 100, t = 60, b = 150, pad = 10),
         annotations = list(text = "Source: Nielsen.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.2))
export(chart3, "./code/5_figures/onlyNielsenPurchases.png")

# Modifying Chart 3 for only post-2007 observations
ann <- graphData3[date %in% parse_date_time(c("2007-12-01", "2016-12-01"), "%Y-%m-%d")]
cha <- ann[, .(change = diff(rollSpend) / head(rollSpend, 1) * 100), by = .(household_income)]

# Plotting
chart3b <- plot_ly(data = graphData3[year(date) >= 2008], x = ~date, width = 1200, height = 800) %>%
  add_lines(y = ~rollSpend, split = ~household_income, line = list(width = 5)) %>%
  add_annotations(x = ann[year(date) == 2016]$date,
                  y = ann[year(date) == 2016]$rollSpend,
                  text = paste0(round(cha$change), "%"),
                  font = list(size = 20),
                  xref = "x",
                  yref = "y",
                  showarrow = TRUE,
                  ax = 60,
                  ay = 0) %>%
  layout(title = "Real Expenditures by Income, Nielsen Purchases (2004-2016)",
         titlefont = list(size = 35),
         xaxis = list(title = "Year", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "Real Expenditures ($2012)", range = c(0, 1000), dtick = 200,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.9, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 100, r = 100, t = 60, b = 150, pad = 10),
         annotations = list(text = "Source: Nielsen.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.2))
export(chart3b, "./code/5_figures/onlyNielsenPurchasesPost2007.png")


# Constructing Chart 4 (Nielsen purchases by department)
graphData4 <- graphData34[, .(monthlySpend = weighted.mean(monthlySpend, w = projection_factor)),
                         by = .(date, household_income, department_code)]
graphData4 <- merge(graphData4, pce, by = "date")
graphData4[, "monthlySpendReal" := monthlySpend / value * 100]
setkey(graphData4, department_code, household_income, date)
graphData4[, "rollSpend" := zoo::rollmean(monthlySpendReal, 12,
                                          fill = c(NA, NULL, NA), align = "right"),
           by = .(department_code, household_income)]

# Getting changes
ann <- graphData4[date %in% parse_date_time(c("2007-12-01", "2016-12-01"), "%Y-%m-%d")]
cha <- ann[, .(change = diff(rollSpend) / head(rollSpend, 1) * 100),
           by = .(department_code, household_income)]

# Plotting
plot4 <- function(dept) {
  chart4 <- plot_ly(data = graphData4[department_code == dept & year(date) >= 2008],
                    x = ~date, width = 1200, height = 800) %>%
    add_lines(y = ~rollSpend, split = ~household_income, line = list(width = 5)) %>%
    add_annotations(x = ann[year(date) == 2016 & department_code == dept]$date,
                    y = ann[year(date) == 2016 & department_code == dept]$rollSpend,
                    text = paste0(round(cha[department_code == dept]$change), "%"),
                    font = list(size = 20),
                    xref = "x",
                    yref = "y",
                    showarrow = TRUE,
                    ax = 60,
                    ay = 0) %>%
    layout(title = paste0("Real Expenditures by Income, Nielsen Purchases (2004-2016)",
                          "<br> Department ", dept),
           titlefont = list(size = 35),
           xaxis = list(title = "Year", titlefont = list(size = 30),
                        tickfont = list(size = 25)),
           yaxis = list(title = "Real Expenditures ($2012)", range = c(0, 200), dtick = 50,
                        titlefont = list(size = 30), tickfont = list(size = 25)),
           # Pick positioning of legend so it doesn"t overlap the chart
           legend = list(yanchor = "top", x = 0.9, font = list(size = 20)),
           # Adjust margins so things look nice
           margin = list(l = 100, r = 100, t = 60, b = 150, pad = 10),
           annotations = list(text = "Source: Nielsen.",
                              font = list(size = 20),
                              showarrow = FALSE,
                              align = "left", valign = "bottom",
                              xref = "paper", x = -0.03,
                              yref = "paper", y = -0.2))
  return(chart4)
}
plot4(0)
plot4(1)
plot4(2)
plot4(3)
plot4(4)
plot4(5)
plot4(6)
plot4(7)
plot4(8)
plot4(9)

