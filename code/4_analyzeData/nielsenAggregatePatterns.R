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
library(stargazer)
plan(multiprocess)
fredr_set_key(fredAPI)
year <- 2004:2016
path <- "/home/mallick/Desktop/Nielsen/Data/Clean/"

# Getting recession and pce
fred <- rbindlist(map(c("PCEPI", "USREC"), fredr, observation_start = as.Date("2004-01-01")))
fred <- dcast(fred, date ~ series_id, value.var = "value")
setnames(fred, c("date", "pce", "rec"))
setkey(fred, "date")

panel <- fread(paste0(path, "fullPanel.csv"), key = c("household_code", "panel_year"))
panel[, "household_income" := factor(household_income,
                                     levels = c("<25k", "25-50k", "50-100k", ">100k"),
                                     ordered = TRUE)]

# Load purchases, sum by trip, merge with trip characteristics, and aggregate to
# daily purchases.
getData <- function(year) {
  purch <- fread(paste0(path, "Purchases/purchase", year, ".csv"),
                 select = c("trip_code_uc", "total_price_paid")
                 )[, .(spend = sum(total_price_paid)), keyby = .(trip_code_uc)]
  fullData <- fread(paste0(path, "Trips/trips", year, ".csv"),
                    select = c("household_code", "panel_year", "trip_code_uc",
                               "purchase_date"), key = "trip_code_uc"
                    )[purch
                      ][, .(spend = sum(spend),
                            trips = .N),
                        by = .(household_code, panel_year, purchase_date)]
  return(fullData)
}

fullData <- rbindlist(future_map(year, getData))
fullData[, "month" := as.integer(substr(purchase_date, 6, 7))]

# Collapsing to month and merging with panel data
monthData <- fullData[, .(spend = sum(spend),
                          trips = sum(trips),
                          days = .N),
                      keyby = .(household_code, panel_year, month)]
monthData <- merge(monthData, panel, by = c("household_code", "panel_year"))
monthData[, "date" := as.Date(paste(panel_year, month, "01", sep = "-"))]

# Trips, days, and spending total and merging with FRED data
monthly <- monthData[, .(trips = weighted.mean(trips, w = projection_factor),
                         days = weighted.mean(days, w = projection_factor),
                         spendPerTrip = weighted.mean(spend / trips, w = projection_factor),
                         spend = weighted.mean(spend, w = projection_factor)),
                     keyby = date]
monthly <- merge(monthly, fred, by = "date")
monthly[, ':=' (trips = rollmean(trips, k = 12, fill = c(NA, NULL, NA), align = "right"),
                days = rollmean(days, k = 12, fill = c(NA, NULL, NA), align = "right"),
                realSpendPerTrip = rollmean(spendPerTrip / pce * 100, k = 12, fill = c(NA, NULL, NA), align = "right"),
                realSpend = rollmean(spend / pce * 100, k = 12, fill = c(NA, NULL, NA), align = "right"))]

chart <- plot_ly(data = monthly, x = ~date, height = 800, width = 1200) %>%
  add_lines(y = ~trips, line = list(width = 5)) %>%
  add_lines(y = ~rec * 1e6,
            line = list(width = 0),
            fill = "tozeroy",
            fillcolor = "rgba(64, 64, 64, 0.2)",
            showlegend = F,
            hoverinfo = "none") %>%
  layout(title = paste0("Average Monthly Shopping Trips (2004-2016)"),
         titlefont = list(size = 35),
         xaxis = list(title = "Year", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "Shopping Trips", range = c(0, 15), dtick = 3,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.7, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 80, r = 100, t = 60, b = 150, pad = 10),
         annotations = list(text = "Source: Nielsen. <br>Note: 12-month rolling average is plotted.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.2))
export(chart, file = "./code/5_figures/aggregateTrips.png")

chart <- plot_ly(data = monthly, x = ~date, height = 800, width = 1200) %>%
  add_lines(y = ~days, line = list(width = 5)) %>%
  add_lines(y = ~rec * 1e6,
            line = list(width = 0),
            fill = "tozeroy",
            fillcolor = "rgba(64, 64, 64, 0.2)",
            showlegend = F,
            hoverinfo = "none") %>%
  layout(title = paste0("Average Monthly Shopping Days (2004-2016)"),
         titlefont = list(size = 35),
         xaxis = list(title = "Year", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "Shopping Days", range = c(0, 15), dtick = 3,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.7, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 80, r = 100, t = 60, b = 150, pad = 10),
         annotations = list(text = "Source: Nielsen. <br>Note: 12-month rolling average is plotted.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.2))
export(chart, file = "./code/5_figures/aggregateDays.png")

chart <- plot_ly(data = monthly, x = ~date, height = 800, width = 1200) %>%
  add_lines(y = ~realSpend, line = list(width = 5)) %>%
  add_lines(y = ~rec * 1e6,
            line = list(width = 0),
            fill = "tozeroy",
            fillcolor = "rgba(64, 64, 64, 0.2)",
            showlegend = F,
            hoverinfo = "none") %>%
  layout(title = paste0("Average Monthly Spending (2004-2016)"),
         titlefont = list(size = 35),
         xaxis = list(title = "Year", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "Spending ($2012)", range = c(0, 400), dtick = 50,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.7, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 100, r = 100, t = 60, b = 150, pad = 10),
         annotations = list(text = "Source: Nielsen. <br>Note: 12-month moving average is plotted.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.2))
export(chart, file = "./code/5_figures/aggregateMonthlySpending.png")

chart <- plot_ly(data = monthly, x = ~date, height = 800, width = 1200) %>%
  add_lines(y = ~realSpendPerTrip, line = list(width = 5)) %>%
  add_lines(y = ~rec * 1e6,
            line = list(width = 0),
            fill = "tozeroy",
            fillcolor = "rgba(64, 64, 64, 0.2)",
            showlegend = F,
            hoverinfo = "none") %>%
  layout(title = paste0("Average Per-Trip Spending (2004-2016)"),
         titlefont = list(size = 35),
         xaxis = list(title = "Year", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "Spending ($2012)", range = c(0, 50), dtick = 10,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.7, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 100, r = 100, t = 60, b = 150, pad = 10),
         annotations = list(text = "Source: Nielsen. <br>Note: 12-month moving average is plotted.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.2))
export(chart, file = "./code/5_figures/aggregatePerTripSpending.png")

# Trips and spending by Income merged with FRED
monthly <- monthData[, .(trips = weighted.mean(trips, w = projection_factor),
                         days = weighted.mean(days, w = projection_factor),
                         spendPerTrip = weighted.mean(spend / trips, w = projection_factor),
                         spend = weighted.mean(spend, w = projection_factor)),
                     keyby = .(date, household_income)]
monthly <- merge(monthly, fred, by = "date")
monthly[, ':=' (trips = rollmean(trips, k = 12, fill = c(NA, NULL, NA), align = "right"),
                days = rollmean(days, k = 12, fill = c(NA, NULL, NA), align = "right"),
                realSpendPerTrip = rollmean(spendPerTrip / pce * 100, k = 12, fill = c(NA, NULL, NA), align = "right"),
                realSpend = rollmean(spend / pce * 100, k = 12, fill = c(NA, NULL, NA), align = "right")),
        by = .(household_income)]

chart <- plot_ly(data = monthly, x = ~date, height = 800, width = 1200) %>%
  add_lines(y = ~trips, split = ~household_income, line = list(width = 5)) %>%
  add_lines(y = ~rec * 1e6,
            line = list(width = 0),
            fill = "tozeroy",
            fillcolor = "rgba(64, 64, 64, 0.2)",
            showlegend = F,
            hoverinfo = "none") %>%
  layout(title = paste0("Average Monthly Shopping Trips (2004-2016)"),
         titlefont = list(size = 50),
         xaxis = list(title = "Year", titlefont = list(size = 40),
                      tickfont = list(size = 40)),
         yaxis = list(title = "Shopping Trips", range = c(8, 12.2), dtick = 1,
                      titlefont = list(size = 40), tickfont = list(size = 40)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.7, font = list(size = 40)),
         # Adjust margins so things look nice
         margin = list(l = 120, r = 100, t = 90, b = 200, pad = 10),
         annotations = list(text = "Source: Nielsen. <br>Note: 12-month rolling average is plotted.",
                            font = list(size = 30),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.4))
export(chart, file = "./code/5_figures/aggregateTripsIncome.png")

chart <- plot_ly(data = monthly, x = ~date, height = 800, width = 1200) %>%
  add_lines(y = ~days, split = ~household_income, line = list(width = 5)) %>%
  add_lines(y = ~rec * 1e6,
            line = list(width = 0),
            fill = "tozeroy",
            fillcolor = "rgba(64, 64, 64, 0.2)",
            showlegend = F,
            hoverinfo = "none") %>%
  layout(title = paste0("Average Monthly Shopping Days (2004-2016)"),
         titlefont = list(size = 35),
         xaxis = list(title = "Year", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "Shopping Days", range = c(6, 9), dtick = 1,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.7, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 80, r = 100, t = 60, b = 150, pad = 10),
         annotations = list(text = "Source: Nielsen. <br>Note: 12-month rolling average is plotted.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.2))
export(chart, file = "./code/5_figures/aggregateDaysIncome.png")

chart <- plot_ly(data = monthly, x = ~date, height = 800, width = 1200) %>%
  add_lines(y = ~realSpend, split = ~household_income, line = list(width = 5)) %>%
  add_lines(y = ~rec * 1e6,
            line = list(width = 0),
            fill = "tozeroy",
            fillcolor = "rgba(64, 64, 64, 0.2)",
            showlegend = F,
            hoverinfo = "none") %>%
  layout(title = paste0("Average Monthly Spending (2004-2016)"),
         titlefont = list(size = 50),
         xaxis = list(title = "Year", titlefont = list(size = 40),
                      tickfont = list(size = 40)),
         yaxis = list(title = "Spending ($2012)", range = c(200, 450), dtick = 50,
                      titlefont = list(size = 40), tickfont = list(size = 40)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.8, y = 1.05, font = list(size = 40)),
         # Adjust margins so things look nice
         margin = list(l = 150, r = 100, t = 90, b = 200, pad = 10),
         annotations = list(text = "Source: Nielsen. <br>Note: 12-month moving average is plotted.",
                            font = list(size = 30),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.35))
export(chart, file = "./code/5_figures/aggregateMonthlySpendingIncome.png")

chart <- plot_ly(data = monthly, x = ~date, height = 800, width = 1200) %>%
  add_lines(y = ~realSpendPerTrip, split = ~household_income, line = list(width = 5)) %>%
  add_lines(y = ~rec * 1e6,
            line = list(width = 0),
            fill = "tozeroy",
            fillcolor = "rgba(64, 64, 64, 0.2)",
            showlegend = F,
            hoverinfo = "none") %>%
  layout(title = paste0("Average Per-Trip Spending (2004-2016)"),
         titlefont = list(size = 35),
         xaxis = list(title = "Year", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "Spending ($2012)", range = c(0, 50), dtick = 10,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.7, y = 0.4, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 100, r = 100, t = 60, b = 150, pad = 10),
         annotations = list(text = "Source: Nielsen. <br>Note: 12-month moving average is plotted.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.2))
export(chart, file = "./code/5_figures/aggregatePerTripSpendingIncome.png")

# Regressions for trips, per-trip spending, and total monthly spending with HH FE's
monthData <- merge(monthData, fred, by = "date")
monthData[, "realSpend" := spend / pce * 100]
monthData[, "perTrip" := realSpend / trips]

reg1 <- felm(data = monthData, log(trips) ~ factor(household_income, ordered = FALSE) |
               as.factor(panel_year) +
               household_size + as.factor(marital_status) + as.factor(race) +
               as.factor(hispanic_origin) + market + age + as.factor(college) +
               as.factor(urban) + as.factor(month) + as.factor(household_code) | 0 | market,
             weights = monthData$projection_factor)
reg2 <- felm(data = monthData, log(days) ~ factor(household_income, ordered = FALSE) |
               as.factor(panel_year) +
               household_size + as.factor(marital_status) + as.factor(race) +
               as.factor(hispanic_origin) + market + age + as.factor(college) +
               as.factor(urban) + as.factor(month) + as.factor(household_code) | 0 | market,
             weights = monthData$projection_factor)
reg3 <- felm(data = monthData, log(realSpend) ~ factor(household_income, ordered = FALSE) |
               as.factor(panel_year) +
               household_size + as.factor(marital_status) + as.factor(race) +
               as.factor(hispanic_origin) + market + age + as.factor(college) +
               as.factor(urban) + as.factor(month) + as.factor(household_code) | 0 | market,
             weights = monthData$projection_factor)
reg4 <- felm(data = monthData, log(perTrip) ~ factor(household_income, ordered = FALSE) |
               as.factor(panel_year) +
               household_size + as.factor(marital_status) + as.factor(race) +
               as.factor(hispanic_origin) + market + age + as.factor(college) +
               as.factor(urban) + as.factor(month) + as.factor(household_code) | 0 | market,
             weights = monthData$projection_factor)

stargazer(reg1, reg2, reg3, reg4,
          column.labels = c("Trips", "Days", "Per-Trip Spend", "Monthly Spend"),
          add.lines = list(c("Household Demographics", "Y", "Y", "Y", "Y"),
                           c("Time FE", "Y", "Y", "Y", "Y"),
                           c("Market FE", "Y", "Y", "Y", "Y"),
                           c("Household FE", "Y", "Y", "Y", "Y")),
          single.row = FALSE, no.space = FALSE, type = "latex",
          covariate.labels = c("25-50k", "50-100k", ">100k"),
          dep.var.labels.include = FALSE,
          label = "tab:aggSpend",
          notes.align = "l",
          notes = "Standard errors are clustered at the market level.",
          omit.stat = c("ser", "rsq"),
          digits = 3,
          out = "./code/6_paper/tables/aggSpendHHFEs.tex")

# Regressions for trips, per-trip spending, and total monthly spending without HH FE's
reg1 <- felm(data = monthData, log(trips) ~ factor(household_income, ordered = FALSE) |
               as.factor(panel_year) +
               household_size + as.factor(marital_status) + as.factor(race) +
               as.factor(hispanic_origin) + market + age + as.factor(college) +
               as.factor(urban) + as.factor(month) | 0 | market,
             weights = monthData$projection_factor)
reg2 <- felm(data = monthData, log(days) ~ factor(household_income, ordered = FALSE) |
               as.factor(panel_year) +
               household_size + as.factor(marital_status) + as.factor(race) +
               as.factor(hispanic_origin) + market + age + as.factor(college) +
               as.factor(urban) + as.factor(month) | 0 | market,
             weights = monthData$projection_factor)
reg3 <- felm(data = monthData, log(realSpend) ~ factor(household_income, ordered = FALSE) |
               as.factor(panel_year) +
               household_size + as.factor(marital_status) + as.factor(race) +
               as.factor(hispanic_origin) + market + age + as.factor(college) +
               as.factor(urban) + as.factor(month) | 0 | market,
             weights = monthData$projection_factor)
reg4 <- felm(data = monthData, log(perTrip) ~ factor(household_income, ordered = FALSE) +
               as.factor(panel_year) +
               household_size + as.factor(marital_status) + as.factor(race) +
               as.factor(hispanic_origin) + market + age + as.factor(college) +
               as.factor(urban) + as.factor(month) | 0 | market,
             weights = monthData$projection_factor)

stargazer(reg1, reg2, reg3, reg4,
          column.labels = c("Trips", "Days", "Per-Trip Spend", "Monthly Spend"),
          add.lines = list(c("Household Demographics", "Y", "Y", "Y", "Y"),
                           c("Time FE", "Y", "Y", "Y", "Y"),
                           c("Market FE", "Y", "Y", "Y", "Y")),
          single.row = FALSE, no.space = FALSE, type = "latex",
          covariate.labels = c("25-50k", "50-100k", ">100k"),
          dep.var.labels.include = FALSE,
          notes.align = "l",
          notes = "Standard errors are clustered at the market level.",
          omit.stat = c("ser", "rsq"),
          digits = 3,
          out = "./code/6_paper/tables/aggSpendNoHHFEs.tex")

### Year Fixed effect interaction charts
regFE <- felm(data = monthData, log(realSpend) ~ factor(household_income, ordered = FALSE) *
                as.factor(panel_year) |
                household_size + as.factor(marital_status) + as.factor(race) +
                as.factor(hispanic_origin) + market + age + as.factor(college) +
                as.factor(urban) + as.factor(month) | 0 | market,
              weights = monthData$projection_factor)

yearFE <- data.table(est = regFE$coefficients, se = regFE$cse, keep.rownames = TRUE)
setnames(yearFE, c("id", "est", "se"))
yearFE[, "id" := gsub("as\\.factor\\(panel_year\\)", "", id)]
yearFE[, "id" := gsub("factor\\(household_income, ordered = FALSE\\)", "", id)]
yearFE[, c("household_income", "year") := tstrsplit(id, ":")]
yearFE[is.na(year), "year" := id]
yearFE <- yearFE[year %in% 2005:2016]
yearFE[household_income %in% 2005:2016, "household_income" := "<25k"]
yearFE <- dcast(yearFE, year ~ household_income, value.var = c("est", "se"))
yearFE[, ':=' (`est_25-50k` = `est_<25k` + `est_25-50k`,
               `est_50-100k` = `est_<25k` + `est_50-100k`,
               `est_>100k` = `est_<25k` + `est_>100k`,
               `se_25-50k` = sqrt(`se_<25k` ^ 2 + `se_25-50k` ^ 2),
               `se_50-100k` = sqrt(`se_<25k` ^ 2 + `se_50-100k` ^ 2),
               `se_>100k` = sqrt(`se_<25k` ^ 2 + `se_>100k` ^ 2))]

chart <- plot_ly(data = yearFE, x = ~year, height = 800, width = 1200) %>%
  add_lines(y = ~`est_<25k`, line = list(width = 5, color = "red"),
            error_y = list(array = ~1.96 * `se_<25k`, color = "red"), name = "<25k") %>%
  add_lines(y = ~`est_>100k`, line = list(width = 5, color = "blue"),
            error_y = list(array = ~1.96 * `se_>100k`, color = "blue"), name = ">100k") %>%
  layout(title = paste0("Year Fixed Effects (2004-2016)"),
         titlefont = list(size = 35),
         xaxis = list(title = "Year", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "Log(Monthly Spending)", range = c(-0.25, 0.05), dtick = 0.05,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.8, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 110, r = 100, t = 60, b = 150, pad = 10),
         annotations = list(text = "Source: Nielsen.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.2))
export(chart, file = "./code/5_figures/yearFEIncome.png")
