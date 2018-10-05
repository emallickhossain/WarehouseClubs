# This plots average monthly spending and number of shopping trips
library(data.table)
library(purrr)
library(furrr)
library(lubridate)
library(plotly)
library(zoo)
library(fredr)
library(lfe)
library(Hmisc)
plan(multiprocess)
fredr_set_key(fredAPI)
year <- 2004:2016
path <- "/home/mallick/Desktop/Nielsen/Data/Clean/"
stores <- c("Grocery", "Discount Store", "Warehouse Club", "Dollar Store", "Online Shopping")

# Getting recession and pce
fred <- rbindlist(map(c("PCEPI"), fredr, observation_start = as.Date("2004-01-01"),
                      frequency = "a", aggregation_method = "avg"))[, "series_id" := NULL]
fred[, "date" := year(date)]
setnames(fred, c("panel_year", "pce"))

panel <- fread(paste0(path, "fullPanel.csv"),
               key = c("household_code", "panel_year"))[household_income %in% c("<25k", ">100k")]
retailers <- fread(paste0(path, "retailers.tsv"), key = "retailer_code")
retailers[, "channel_type" := ifelse(channel_type %in% stores, channel_type, "Other")]

getData <- function(year) {
  print(year)
  purch <- fread(paste0(path, "Purchases/purchase", year, ".csv"),
                 select = c("total_price_paid", "quantity", "trip_code_uc"),
                 key = "trip_code_uc")
  purch <- purch[, .(spend = sum(total_price_paid),
                     quantity = sum(quantity)), by = .(trip_code_uc)]

  trips <- fread(paste0(path, "Trips/trips", year, ".csv"),
                 select = c("household_code", "panel_year", "trip_code_uc", "retailer_code"),
                 key = c("trip_code_uc", "retailer_code", "household_code", "panel_year"))
  fullData <- merge(purch, trips, by = "trip_code_uc")
  fullData <- merge(fullData, retailers, by = "retailer_code")[, "retailer_code" := NULL]
  fullData <- merge(fullData, panel, by = c("household_code", "panel_year"))
  return(fullData)
}

fullData <- rbindlist(future_map(year, getData))
fullData <- merge(fullData, fred, by = "panel_year")
fullData[, "realSpend" := spend / pce * 100]

# Trips by income
trips <- fullData[, .(trips = uniqueN(trip_code_uc) / 12),
                  by = .(household_code, panel_year, projection_factor, channel_type)]
trips <- trips[, .(trips = weighted.mean(trips, w = projection_factor)),
               by = .(panel_year, channel_type)]
annual <- fullData[, .(realSpendPerTrip = weighted.mean(realSpend, w = projection_factor),
                       qPerTrip = weighted.mean(quantity, w = projection_factor)),
                   by = .(panel_year, channel_type)]
chart <- plot_ly(data = trips, x = ~panel_year, height = 800, width = 1200) %>%
  add_lines(y = ~trips, split = ~channel_type, line = list(width = 5)) %>%
  layout(title = paste0("Average Monthly Shopping Trips by Income (2004-2016)"),
         titlefont = list(size = 35),
         xaxis = list(title = "Year", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "Shopping Trips", range = c(0, 7), dtick = 1,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.7, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 80, r = 100, t = 60, b = 150, pad = 10),
         annotations = list(text = "Source: Nielsen.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.2))

# Regression for trips
trips <- fullData[, .(trips = uniqueN(trip_code_uc) / 12),
                  by = .(household_code, panel_year, projection_factor, channel_type,
                         household_income, household_size, marital_status, race,
                         hispanic_origin, market, age, college, urban)]
reg <- felm(data = trips, trips ~ household_income + as.factor(panel_year) * channel_type * as.factor(urban) |
              household_size + as.factor(marital_status) + as.factor(race) +
              as.factor(hispanic_origin) + market + age + as.factor(college) | 0 | market,
            weights = trips$projection_factor)
yearChannelFE <- data.table(est = reg$coefficients, se = reg$se, keep.rownames = TRUE)
setnames(yearChannelFE, c("id", "est", "se"))
yearChannelFE[, "id" := gsub("as\\.factor\\(panel_year\\)", "", id)]
yearChannelFE[, c("year", "channel", "urban") := tstrsplit(id, ":")]
yearChannelFE[, "year" := as.integer(year)]
yearChannelFE <- yearChannelFE[!is.na(year)]
yearChannelFE[is.na(channel), "channel" := "Discount Store"]
yearChannelFE[channel == "as.factor(urban)1", ':=' (channel = "Discount Store",
                                                    urban = "as.factor(urban)1")]
yearChannelFE[, "channel" := gsub("channel_type", "", channel)]
yearChannelFE[, "urban" := as.integer(gsub("as\\.factor\\(urban\\)", "", urban))]
yearChannelFE[is.na(urban), "urban" := 0]
yearChannelFE[, "id" := NULL]
yearChannelFE <- dcast(data = yearChannelFE, ... ~ urban, value.var = c("est", "se"))
yearChannelFE[, ':=' (urbanCombineEst = est_0 + est_1,
                      urbanCombineSE = se_0 + se_1)]

chart <- plot_ly(data = yearChannelFE, x = ~year, height = 800, width = 1200) %>%
  add_lines(y = ~est_0, error_y = ~list(value = se_0), line = list(width = 5),
            split = ~channel) %>%
  layout(title = paste0("Year Fixed Effects by Channel (Rural, 2004-2016)"),
         titlefont = list(size = 35),
         xaxis = list(title = "Year", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "Trips", range = c(-2, 1), dtick = 0.5,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.05, y = 0.4, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 110, r = 100, t = 60, b = 150, pad = 10),
         annotations = list(text = "Source: Nielsen.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.2))
export(chart, file = "./code/5_figures/yearChannelFERural.png")

chart <- plot_ly(data = yearChannelFE, x = ~year, height = 800, width = 1200) %>%
  add_lines(y = ~urbanCombineEst, error_y = ~list(value = urbanCombineSE), line = list(width = 5),
            split = ~channel) %>%
  layout(title = paste0("Year Fixed Effects by Channel (Urban, 2004-2016)"),
         titlefont = list(size = 35),
         xaxis = list(title = "Year", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "Trips", range = c(-2, 1), dtick = 0.5,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.05, y = 0.4, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 110, r = 100, t = 60, b = 150, pad = 10),
         annotations = list(text = "Source: Nielsen.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.2))
export(chart, file = "./code/5_figures/yearChannelFEUrban.png")








# Looking at Monthly spending
plot_ly(data = annual, x = ~panel_year, height = 800, width = 1200) %>%
  add_lines(y = ~realSpend, split = ~household_income, line = list(width = 5)) %>%
  layout(title = paste0("Average Monthly Expenditures by Income (2004-2016)"),
         titlefont = list(size = 35),
         xaxis = list(title = "Year", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "Monthly Expenditures ($2012)", range = c(0, 450), dtick = 50,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.7, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 100, r = 100, t = 60, b = 150, pad = 10),
         annotations = list(text = "Source: Nielsen.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.2))

# Regression
fullData[, "realSpend" := spend / pce * 100]
reg <- felm(data = fullData, log(realSpend) ~ household_income * as.factor(panel_year) |
              household_size + as.factor(marital_status) + as.factor(race) +
              as.factor(hispanic_origin) + market + age + as.factor(college) + as.factor(urban) | 0 | market,
            weights = fullData$projection_factor)

# Looking at per trip spending
plot_ly(data = annual, x = ~panel_year, height = 800, width = 1200) %>%
  add_lines(y = ~realSpendPerTrip, split = ~household_income, line = list(width = 5)) %>%
  layout(title = paste0("Average Per-Trip Expenditures by Income (2004-2016)"),
         titlefont = list(size = 35),
         xaxis = list(title = "Year", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "Per-Trip Expenditures", range = c(0, 50), dtick = 10,
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

# Regression
reg <- felm(data = fullData, log(realSpendPerTrip) ~ household_income * as.factor(panel_year) |
              household_size + as.factor(marital_status) + as.factor(race) +
              as.factor(hispanic_origin) + market + age + as.factor(college) + as.factor(urban) | 0 | market,
            weights = fullData$projection_factor)

# Looking at per trip Quantities
plot_ly(data = annual, x = ~panel_year, height = 800, width = 1200) %>%
  add_lines(y = ~qPerTrip, split = ~household_income, line = list(width = 5)) %>%
  layout(title = paste0("Average Per-Trip Quantities by Income (2004-2016)"),
         titlefont = list(size = 35),
         xaxis = list(title = "Year", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "Per-Trip Quantities", range = c(0, 15), dtick = 5,
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

# Regression
reg <- felm(data = fullData, log(qPerTrip) ~ household_income * as.factor(panel_year) |
              household_size + as.factor(marital_status) + as.factor(race) +
              as.factor(hispanic_origin) + market + age + as.factor(college) + as.factor(urban) | 0 | market,
            weights = fullData$projection_factor)
