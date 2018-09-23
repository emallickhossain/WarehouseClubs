# This gets all shopping trips
library(data.table)
library(lubridate)
library(plotly)
library(fredr)
library(purrr)
fredr_set_key(fredAPI)
yr <- 2004:2016
path <- "/home/mallick/Desktop/Nielsen/Data/Consumer_Panel_Data_2004-2016/nielsen_extracts/HMS/"

# Get PCE
pce <- setDT(fredr("PCEPI", observation_start = as.Date("2004-01-01")))[, "series_id" := NULL]
pce[, "date" := as.POSIXct(date)]

################## RECREATING TRIPS CHART (Chart 1) ############################
getChart1 <- function(yr) {
  print(yr)
  # Getting shopping trips by household-month
  trips <- fread(paste0(path, yr, "/Annual_Files/trips_", yr, ".tsv"),
                 select = c("household_code", "panel_year", "purchase_date", "total_spent"))
  trips[, "purchase_date" := parse_date_time2(paste0(substr(purchase_date, 1, 8), "01"), "%Y-%m-%d")]
  setnames(trips, "purchase_date", "date")
  trips <- trips[, .(monthlySpend = sum(total_spent)),
                 by = .(household_code, panel_year, date)]
  trips <- trips[year(date) == panel_year]

  # Getting panel
  panel <- fread(paste0(path, yr, "/Annual_Files/panelists_", yr, ".tsv"),
                 select = c("Household_Cd", "Panel_Year", "Household_Income", "Projection_Factor"),
                 col.names = c("household_code", "panel_year", "household_income", "projection_factor"))
  panel[, "household_income" := cut(household_income, c(0, 13, 19, 26, 30),
                                    labels = c("<25k", "25-50k", "50-100k", ">100k"),
                                    ordered_result = TRUE)]

  # Merging to get demographics
  trips <- merge(trips, panel, by = c("household_code", "panel_year"))

  # Getting weighted average monthly spending by month-income group
  graphData1 <- trips[, .(monthlySpend = weighted.mean(monthlySpend, w = projection_factor)),
                      by = .(date, household_income)]
  graphData1 <- merge(graphData1, pce, by = "date")
  graphData1[, "monthlySpendReal" := monthlySpend / value * 100]
  return(graphData1)
}
graphData1 <- rbindlist(map(yr, getChart1))
setkey(graphData1, household_income, date)
graphData1[, "rollSpend" := zoo::rollmean(monthlySpendReal, 12,
                                          fill = c(NA, NULL, NA), align = "right"),
           by = .(household_income)]

# Getting changes
ann <- graphData1[date %in% parse_date_time(c("2004-12-01", "2016-12-01"), "%Y-%m-%d")]
cha <- ann[, .(change = diff(rollSpend) / head(rollSpend, 1) * 100), by = .(household_income)]

# 1. Plotting total trip spending
chart1 <- plot_ly(data = graphData1, x = ~date, width = 1200, height = 800) %>%
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
  layout(title = "Real Expenditures by Income, All Trips (2004-2016)",
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
export(chart1, "./code/5_figures/allNielsenTrips.png")
