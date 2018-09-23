library(data.table)
library(plotly)
library(purrr)
library(furrr)
library(fredr)
library(zoo)
plan(multiprocess)
fredr_set_key(fredAPI)
path <- "/home/mallick/Desktop/Nielsen/Data/Clean/"
yr <- 2004:2016

panel <- fread(paste0(path, "fullPanel.csv"),
               select = c("household_code", "panel_year", "projection_factor", "household_income"))

retailers <- fread(paste0(path, "retailers.tsv"))

# Getting pce
pce <- setDT(fredr("PCEPI", observation_start = as.Date("2004-01-01"),
                   frequency = "a", aggregation_method = "eop"))[, "series_id" := NULL]
pce[, "panel_year" := as.integer(substr(date, 1, 4))]

# Getting shopping trips
getTrips <- function(yr) {
  purch <- fread(paste0(path, "Purchases/purchase", yr, ".csv"))
  purch <- purch[, .(p = sum(total_price_paid)), by = .(trip_code_uc)]

  trips <- fread(paste0(path, "Trips/trips", yr, ".csv"),
                 select = c("trip_code_uc", "household_code", "panel_year", "retailer_code"))
  purch <- merge(purch, trips, by = "trip_code_uc")
  purch <- merge(purch, retailers, by = "retailer_code")
  purch <- purch[, .(trips = uniqueN(trip_code_uc),
                     p = sum(p)),
                 by = .(household_code, panel_year, channel_type)]
  return(purch)
}

trips <- rbindlist(future_map(yr, getTrips))
fullData <- merge(trips, panel, by = c("household_code", "panel_year"))

# Monthly purchase patterns
monthlyTrips <- fullData[, .(trips = sum(trips) / 12,
                             p = sum(p) / 12),
                         by = .(household_code, panel_year, projection_factor, household_income)]
graphData <- monthlyTrips[, .(trips = weighted.mean(trips, w = projection_factor),
                              p = weighted.mean(p, w = projection_factor)),
                          by = .(panel_year, household_income)]
graphData <- merge(graphData, pce, by = "panel_year")
graphData[, "realP" := p / value * 100]
graphData[, "perTrip" := realP / trips]

# Monthly trips
chart <- plot_ly(data = graphData, x = ~date, height = 800, width = 1200) %>%
  add_lines(y = ~trips, split = ~household_income, line = list(width = 5)) %>%
  layout(title = "Monthly Shopping Trips by Income (2004-2016)",
         titlefont = list(size = 35),
         xaxis = list(title = "Year", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "Number of Trips", range = c(0, 15), dtick = 5,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.75, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 80, r = 100, t = 60, b = 150, pad = 10),
         annotations = list(text = "Source: Nielsen.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.2))
export(chart, "./code/5_figures/nielsenShoppingTripsIncome.png")

# Monthly Expenditures
chart <- plot_ly(data = graphData, x = ~date, height = 800, width = 1200) %>%
  add_lines(y = ~realP, split = ~household_income, line = list(width = 5)) %>%
  layout(title = "Real Monthly Expenditures by Income (2004-2016)",
         titlefont = list(size = 35),
         xaxis = list(title = "Year", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "Real Expenditures ($2012)", range = c(0, 400), dtick = 100,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.8, y = 0.5, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 100, r = 100, t = 60, b = 150, pad = 10),
         annotations = list(text = "Source: Nielsen.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.2))
export(chart, "./code/5_figures/nielsenShoppingExpendituresIncome.png")


# Per trip spending
chart <- plot_ly(data = graphData, x = ~date, height = 800, width = 1200) %>%
  add_lines(y = ~perTrip, split = ~household_income, line = list(width = 5)) %>%
  layout(title = "Real Per-Trip Expenditures by Income (2004-2016)",
         titlefont = list(size = 35),
         xaxis = list(title = "Year", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "Real Expenditures ($2012)", range = c(0, 50), dtick = 10,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.1, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 100, r = 100, t = 60, b = 150, pad = 10),
         annotations = list(text = "Source: Nielsen.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.2))
export(chart, "./code/5_figures/nielsenShoppingPerTripIncome.png")
