# This gets all shopping trips
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

panel <- fread(paste0(path, "fullPanel.csv"))

# Getting data for only Nielsen coded trips
getChart2 <- function(yr) {
  # Getting unique trips with Nielsen purchases
  purch <- unique(fread(paste0(path, "Purchases/purchase", yr, ".csv"), select = "trip_code_uc"))

  # Getting trips and only getting the ones that were Nielsen coded
  trips <- fread(paste0(path, "Trips/trips", yr, ".csv"),
                 select = c("trip_code_uc", "household_code", "panel_year",
                            "purchase_date", "total_spent"))
  trips <- trips[trip_code_uc %in% purch$trip_code_uc]
  trips[, "purchase_date" := parse_date_time2(paste0(substr(purchase_date, 1, 8), "01"), "%Y-%m-%d")]
  trips[, "trip_code_uc" := NULL]
  setnames(trips, "purchase_date", "date")

  # Get monthly spending per household-month
  fullData <- trips[, .(monthlySpend = sum(total_spent)), by = .(household_code, panel_year, date)]
  return(fullData)
}
graphData2 <- rbindlist(future_map(yr, getChart2))

# Adding demographics and deflating
graphData2 <- merge(graphData2, panel, by = c("household_code", "panel_year"))
graphData2 <- graphData2[, .(monthlySpend = weighted.mean(monthlySpend, w = projection_factor)),
                         by = .(date, household_income)]
graphData2 <- merge(graphData2, pce, by = "date")
graphData2[, "monthlySpendReal" := monthlySpend / value * 100]
setkey(graphData2, household_income, date)
graphData2[, "rollSpend" := zoo::rollmean(monthlySpendReal, 12,
                                          fill = c(NA, NULL, NA), align = "right"),
           by = .(household_income)]

# Getting changes
ann <- graphData2[date %in% parse_date_time(c("2004-12-01", "2016-12-01"), "%Y-%m-%d")]
cha <- ann[, .(change = diff(rollSpend) / head(rollSpend, 1) * 100), by = .(household_income)]

# Plotting total trip spending on Nielsen only trips
chart2 <- plot_ly(data = graphData2, x = ~date, width = 1200, height = 800) %>%
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
  layout(title = "Real Expenditures by Income, Nielsen Purchase Trips (2004-2016)",
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
export(chart2, "./code/5_figures/onlyNielsenTrips.png")
