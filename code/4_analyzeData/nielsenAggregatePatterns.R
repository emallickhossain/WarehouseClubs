# This plots average monthly spending and number of shopping trips
library(data.table)
library(purrr)
library(lubridate)
library(plotly)
library(zoo)
library(fredr)
fredr_set_key(fredAPI)
years <- 2004:2016

# Getting recession and pce
rec <- setDT(fredr("USREC", observation_start = as.Date("2004-01-01")))[, "series_id" := NULL]
setnames(rec, "value", "rec")
pce <- setDT(fredr("PCEPI", observation_start = as.Date("2004-01-01")))[, "series_id" := NULL]
setnames(pce, "value", "pce")

# Getting panel demographic data
getData <- function(year) {
  print(year)
  panel <- fread(paste0("/home/mallick/Desktop/Nielsen/Data/Consumer_Panel_Data_",
                        "2004-2016/nielsen_extracts/HMS/", year,
                        "/Annual_Files/panelists_", year, ".tsv"),
                 select = c("Household_Cd", "Panel_Year", "Projection_Factor"))
  setnames(panel, c("household_code", "panel_year", "projection_factor"))

  # Getting trips
  trips <- fread(paste0("/home/mallick/Desktop/Nielsen/Data/Consumer_Panel_Data_",
                        "2004-2016/nielsen_extracts/HMS/", year,"/Annual_Files/",
                        "trips_", year, ".tsv"),
                 select = c("household_code", "purchase_date", "panel_year", "total_spent"))
  trips[, "purchase_date" := parse_date_time(purchase_date, "%Y-%m-%d")]
  trips[, ':=' (year = as.integer(year(purchase_date)),
                month = as.integer(month(purchase_date)))]
  trips <- trips[panel_year == year,
                 .(totalSpend = sum(total_spent), totalCount = .N),
                 by = .(household_code, panel_year, year, month)]
  fullData <- merge(trips, panel, by = c("household_code", "panel_year"))
  fullData <- fullData[, .(totalSpend = weighted.mean(totalSpend, w = projection_factor),
                           totalCount = weighted.mean(totalCount, w = projection_factor)),
                       by = .(year, month)]
  fullData[, c("date", "year", "month") :=
             .(as.Date(paste(year, month, "01", sep = "-")), NULL, NULL)]
  return(fullData)
}

fullData <- rbindlist(map(years, getData), use.names = TRUE, fill = TRUE)
fullData <- merge(fullData, rec, by = "date")
fullData <- merge(fullData, pce, by = "date")
rm(pce, rec)
fullData[, "realSpend" := totalSpend / pce * 100]

fullData[, ':=' (rollSpend = rollmean(realSpend, 12, fill = c(NA, NULL, NA), align = "right"),
                 rollTrip = rollmean(totalCount, 12, fill = c(NA, NULL, NA), align = "right"),
                 rollPerTrip = rollmean(realSpend / totalCount, 12,
                                        fill = c(NA, NULL, NA), aligh = "right"))]

# Trips
chart <- plot_ly(data = fullData, x = ~date, height = 800, width = 1200) %>%
  add_lines(y = ~rollTrip, line = list(width = 5), name = "Trips (left)") %>%
  add_lines(y = ~rollSpend, line = list(width = 5), name = "Real Spending (right)", yaxis = "y2") %>%
  add_lines(y = ~rec * 1e5,
            line = list(width = 0),
            fill = "tozeroy",
            fillcolor = "rgba(64, 64, 64, 0.2)",
            showlegend = F,
            hoverinfo = "none") %>%
  layout(title = paste0("Monthly Shopping Trips and Expenditures (2004-2016)"),
         titlefont = list(size = 35),
         xaxis = list(title = "Year", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "Shopping Trips", range = c(0, 20), dtick = 5,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         yaxis2 = list(title = "Real Spending", range = c(0, 1000), dtick = 250,
                       side = "right", overlaying = "y", titlefont = list(size = 30),
                       tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.7, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 80, r = 100, t = 60, b = 150, pad = 10),
         annotations = list(text = "Source: Nielsen. <br> Note: 12-month rolling averages are plotted.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.2))
export(chart, "./code/5_figures/nielsenAggregatePatterns.png")
