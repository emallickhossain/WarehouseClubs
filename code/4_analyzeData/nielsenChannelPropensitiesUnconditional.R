library(data.table)
library(purrr)
library(furrr)
library(plotly)
plan(multiprocess)
path <- "/home/mallick/Desktop/Nielsen/Data/Clean/"
yr <- 2004:2016
#stores <- c("Grocery", "Warehouse Club", "Discount Store", "Drug/Dollar Store")
stores <- c("Grocery", "Warehouse Club", "Discount Store", "Dollar Store")

# Getting retailers
retailers <- fread(paste0(path, "retailers.tsv"))
#retailers[channel_type %in% c("Drug Store", "Dollar Store"), "channel_type" := "Drug/Dollar Store"]
retailers <- retailers[channel_type %in% stores]

panel <- fread(paste0(path, "fullPanel.csv"),
               select = c("household_code", "panel_year", "projection_factor", "household_income"))

# Getting panel demographic data
getData <- function(yr) {
  print(yr)
  panel <- panel[panel_year == yr]
  channels <- setDT(expand.grid(household_code = unique(panel$household_code),
                                panel_year = unique(panel$panel_year),
                                channel_type = unique(stores)))
  panel <- merge(channels, panel, by = c("household_code", "panel_year"), all.x = TRUE)

  # Getting Nielsen purchases by trip
  purch <- fread(paste0(path, "Purchases/purchase", yr, ".csv"),
                 select = c("trip_code_uc", "total_price_paid"))
  purch <- purch[, .(total_price_paid = sum(total_price_paid)), by = .(trip_code_uc)]

  # Getting trips
  trips <- fread(paste0(path, "Trips/trips", yr, ".csv"),
                 select = c("trip_code_uc", "household_code", "retailer_code", "panel_year"))

  # Merging with purchases
  trips <- merge(trips, purch, by = "trip_code_uc")

  # Getting monthly spending by household
  trips[, c("monthSpend", "monthCount") := .(sum(total_price_paid) / 12, .N / 12),
        by = .(household_code, panel_year)]

  # Getting channel type
  trips <- merge(trips, retailers, by = "retailer_code")

  # Getting monthly spending by channel and household
  trips <- trips[, .(count = .N / 12, spending = sum(total_price_paid) / 12),
                 by = .(household_code, panel_year, channel_type, monthSpend, monthCount)]
  fullData <- merge(trips, panel, all.y = TRUE,
                    by = c("household_code", "panel_year", "channel_type"))
  fullData[is.na(spending), "spending" := 0]
  fullData[is.na(count), "count" := 0]
  fullData[, "visit" := ifelse(count > 0, 1L, 0L)]
  fullData <- fullData[, .(spending = weighted.mean(spending, w = projection_factor),
                           count = weighted.mean(count, w = projection_factor),
                           totalSpend = weighted.mean(monthSpend, w = projection_factor, na.rm = TRUE),
                           totalCount = weighted.mean(monthCount, w = projection_factor, na.rm = TRUE),
                           visit = weighted.mean(visit, w = projection_factor)),
                       by = .(channel_type, household_income, panel_year)]
  return(fullData)
}

fullData <- rbindlist(future_map(yr, getData))

fullData[, ':=' (countShare = count / totalCount * 100,
                 spendShare = spending / totalSpend * 100)]
setkey(fullData, household_income, channel_type, panel_year)

# Trips (Levels)
plotTrips <- function(storeType, legend) {
  chart <- plot_ly(data = fullData[channel_type == storeType], x = ~panel_year,
                   height = 800, width = 1200) %>%
    add_lines(data = fullData[channel_type == storeType & household_income == "<25k"],
              y = ~count, line = list(width = 5, color = "red"), name = "<$25k") %>%
    add_lines(data = fullData[channel_type == storeType & household_income == "25-50k"],
              y = ~count, line = list(width = 5, color = "green"), name = "$25-$50k") %>%
    add_lines(data = fullData[channel_type == storeType & household_income == "50-100k"],
              y = ~count, line = list(width = 5, color = "orange"), name = "$50k-$100k") %>%
    add_lines(data = fullData[channel_type == storeType & household_income == ">100k"],
              y = ~count, line = list(width = 5, color = "blue"), name = ">$100k") %>%
    layout(xaxis = list(title = "Year", titlefont = list(size = 30),
                        tickfont = list(size = 25)),
           yaxis = list(title = "Trip", range = c(0, 6), dtick = 1,
                        titlefont = list(size = 30), tickfont = list(size = 25)),
           # Adjust margins so things look nice
           margin = list(l = 80, r = 100, t = 60, b = 150, pad = 10),
           showlegend = legend)
  return(chart)
}
charts <- map2(stores, c(T, F, F, F), plotTrips)
chart <- subplot(charts, nrows = 2, shareX = TRUE, shareY = TRUE, margin = 0.03) %>%
  add_annotations("Grocery", x = 0.1, y = 0.8, xref = "paper",
                  yref = "paper", showarrow = FALSE, font = list(size = 30)) %>%
  add_annotations("Warehouse Club", x = 0.75, y = 0.8, xref = "paper",
                  yref = "paper", showarrow = FALSE, font = list(size = 30)) %>%
  add_annotations("Discount Store", x = 0.1, y = 0.2, xref = "paper",
                  yref = "paper", showarrow = FALSE, font = list(size = 30)) %>%
  add_annotations("Dollar Store", x = 0.8, y = 0.2, xref = "paper",
                  yref = "paper", showarrow = FALSE, font = list(size = 30)) %>%
  layout(title = "Average Monthly Shopping Trips by Channel and Income (2004-2016)",
         titlefont = list(size = 35),
         annotations = list(text = "Source: Nielsen.",
                            font = list(size = 20),
                            showarrow = FALSE, align = "left", valign = "bottom",
                            xref = "paper", x = -0.03, yref = "paper", y = -0.22))
export(chart, paste0("./code/5_figures/nielsenChannelPropensitiesTripLevels.png"))

# Trips (Shares)
plotTrips <- function(storeType, legend) {
  chart <- plot_ly(data = fullData[channel_type == storeType], x = ~panel_year,
                   height = 800, width = 1200) %>%
    add_lines(data = fullData[channel_type == storeType & household_income == "<25k"],
              y = ~countShare, line = list(width = 5, color = "red"), name = "<$25k") %>%
    add_lines(data = fullData[channel_type == storeType & household_income == "25-50k"],
              y = ~countShare, line = list(width = 5, color = "green"), name = "$25-$50k") %>%
    add_lines(data = fullData[channel_type == storeType & household_income == "50-100k"],
              y = ~countShare, line = list(width = 5, color = "orange"), name = "$50k-$100k") %>%
    add_lines(data = fullData[channel_type == storeType & household_income == ">100k"],
              y = ~countShare, line = list(width = 5, color = "blue"), name = ">$100k") %>%
    layout(xaxis = list(title = "Year", titlefont = list(size = 30),
                        tickfont = list(size = 25)),
           yaxis = list(title = "Trip Share", range = c(0, 50), dtick = 10,
                        titlefont = list(size = 30), tickfont = list(size = 25)),
           # Adjust margins so things look nice
           margin = list(l = 80, r = 100, t = 60, b = 150, pad = 10),
           showlegend = legend)
  return(chart)
}
charts <- map2(stores, c(T, F, F, F), plotTrips)
chart <- subplot(charts, nrows = 2, shareX = TRUE, shareY = TRUE, margin = 0.03) %>%
  add_annotations("Grocery", x = 0.1, y = 0.9, xref = "paper",
                  yref = "paper", showarrow = FALSE, font = list(size = 30)) %>%
  add_annotations("Warehouse Club", x = 0.75, y = 0.9, xref = "paper",
                  yref = "paper", showarrow = FALSE, font = list(size = 30)) %>%
  add_annotations("Discount Store", x = 0.1, y = 0.2, xref = "paper",
                  yref = "paper", showarrow = FALSE, font = list(size = 30)) %>%
  add_annotations("Dollar Store", x = 0.8, y = 0.2, xref = "paper",
                  yref = "paper", showarrow = FALSE, font = list(size = 30)) %>%
  layout(title = "Average Monthly Shopping Trips by Channel and Income (2004-2016)",
         titlefont = list(size = 35),
         annotations = list(text = "Source: Nielsen.",
                            font = list(size = 20),
                            showarrow = FALSE, align = "left", valign = "bottom",
                            xref = "paper", x = -0.03, yref = "paper", y = -0.22))
export(chart, paste0("./code/5_figures/nielsenChannelPropensitiesTripShares.png"))

# Spending (Levels)
plotSpend <- function(storeType, legend) {
  chart <- plot_ly(data = fullData[channel_type == storeType], x = ~panel_year,
                   height = 800, width = 1200) %>%
    add_lines(data = fullData[channel_type == storeType & household_income == "<25k"],
              y = ~spending, line = list(width = 5, color = "red"), name = "<$25k") %>%
    add_lines(data = fullData[channel_type == storeType & household_income == "25-50k"],
              y = ~spending, line = list(width = 5, color = "green"), name = "$25-$50k") %>%
    add_lines(data = fullData[channel_type == storeType & household_income == "50-100k"],
              y = ~spending, line = list(width = 5, color = "orange"), name = "50k-100k") %>%
    add_lines(data = fullData[channel_type == storeType & household_income == ">100k"],
              y = ~spending, line = list(width = 5, color = "blue"), name = ">$100k") %>%
    layout(xaxis = list(title = "Year", titlefont = list(size = 30),
                        tickfont = list(size = 25)),
           yaxis = list(title = "Spending ($2012)", range = c(0, 200), dtick = 50,
                        titlefont = list(size = 30), tickfont = list(size = 25)),
           # Adjust margins so things look nice
           margin = list(l = 100, r = 100, t = 60, b = 150, pad = 10),
           showlegend = legend)
  return(chart)
}
charts <- map2(stores, c(T, F, F, F), plotSpend)
chart <- subplot(charts, nrows = 2, shareX = TRUE, shareY = TRUE, margin = 0.03) %>%
  add_annotations("Grocery", x = 0.1, y = 0.8, xref = "paper",
                  yref = "paper", showarrow = FALSE, font = list(size = 30)) %>%
  add_annotations("Warehouse Club", x = 0.75, y = 0.8, xref = "paper",
                  yref = "paper", showarrow = FALSE, font = list(size = 30)) %>%
  add_annotations("Discount Store", x = 0.1, y = 0.25, xref = "paper",
                  yref = "paper", showarrow = FALSE, font = list(size = 30)) %>%
  add_annotations("Dollar Store", x = 0.8, y = 0.25, xref = "paper",
                  yref = "paper", showarrow = FALSE, font = list(size = 30)) %>%
  layout(title = "Average Monthly Expenditures by Channel and Income (2004-2016)",
         titlefont = list(size = 35),
         annotations = list(text = "Source: Nielsen.",
                            font = list(size = 20),
                            showarrow = FALSE, align = "left", valign = "bottom",
                            xref = "paper", x = -0.03, yref = "paper", y = -0.22))
export(chart, paste0("./code/5_figures/nielsenChannelPropensitiesSpendLevels.png"))

# Spending (Shares)
plotSpend <- function(storeType, legend) {
  chart <- plot_ly(data = fullData[channel_type == storeType], x = ~panel_year,
                   height = 800, width = 1200) %>%
    add_lines(data = fullData[channel_type == storeType & household_income == "<25k"],
              y = ~spendShare, line = list(width = 5, color = "red"), name = "<$25k") %>%
    add_lines(data = fullData[channel_type == storeType & household_income == "25-50k"],
              y = ~spendShare, line = list(width = 5, color = "green"), name = "$25-$50k") %>%
    add_lines(data = fullData[channel_type == storeType & household_income == "50-100k"],
              y = ~spendShare, line = list(width = 5, color = "orange"), name = "50k-100k") %>%
    add_lines(data = fullData[channel_type == storeType & household_income == ">100k"],
              y = ~spendShare, line = list(width = 5, color = "blue"), name = ">$100k") %>%
    layout(xaxis = list(title = "Year", titlefont = list(size = 30),
                        tickfont = list(size = 25)),
           yaxis = list(title = "Spending Share", range = c(0, 60), dtick = 10,
                        titlefont = list(size = 30), tickfont = list(size = 25)),
           # Adjust margins so things look nice
           margin = list(l = 80, r = 100, t = 60, b = 150, pad = 10),
           showlegend = legend)
  return(chart)
}
charts <- map2(stores, c(T, F, F, F), plotSpend)
chart <- subplot(charts, nrows = 2, shareX = TRUE, shareY = TRUE, margin = 0.03) %>%
  add_annotations("Grocery", x = 0.1, y = 0.9, xref = "paper",
                  yref = "paper", showarrow = FALSE, font = list(size = 30)) %>%
  add_annotations("Warehouse Club", x = 0.75, y = 0.9, xref = "paper",
                  yref = "paper", showarrow = FALSE, font = list(size = 30)) %>%
  add_annotations("Discount Store", x = 0.1, y = 0.25, xref = "paper",
                  yref = "paper", showarrow = FALSE, font = list(size = 30)) %>%
  add_annotations("Dollar Store", x = 0.8, y = 0.25, xref = "paper",
                  yref = "paper", showarrow = FALSE, font = list(size = 30)) %>%
  layout(title = "Average Monthly Expenditures by Channel and Income (2004-2016)",
         titlefont = list(size = 35),
         annotations = list(text = "Source: Nielsen.",
                            font = list(size = 20),
                            showarrow = FALSE, align = "left", valign = "bottom",
                            xref = "paper", x = -0.03, yref = "paper", y = -0.22))
export(chart, paste0("./code/5_figures/nielsenChannelPropensitiesSpendShares.png"))

# Visit Probabilities
plotSpend <- function(storeType, legend) {
  chart <- plot_ly(data = fullData[channel_type == storeType], x = ~panel_year,
                   height = 800, width = 1200) %>%
    add_lines(data = fullData[channel_type == storeType & household_income == "<25k"],
              y = ~visit, line = list(width = 5, color = "red"), name = "<$25k") %>%
    add_lines(data = fullData[channel_type == storeType & household_income == "25-50k"],
              y = ~visit, line = list(width = 5, color = "green"), name = "$25-$50k") %>%
    add_lines(data = fullData[channel_type == storeType & household_income == "50-100k"],
              y = ~visit, line = list(width = 5, color = "orange"), name = "50k-100k") %>%
    add_lines(data = fullData[channel_type == storeType & household_income == ">100k"],
              y = ~visit, line = list(width = 5, color = "blue"), name = ">$100k") %>%
    layout(xaxis = list(title = "Year", titlefont = list(size = 30),
                        tickfont = list(size = 25)),
           yaxis = list(title = "Spending Share", range = c(0, 1), dtick = 0.2,
                        titlefont = list(size = 30), tickfont = list(size = 25)),
           # Adjust margins so things look nice
           margin = list(l = 100, r = 100, t = 60, b = 150, pad = 10),
           showlegend = legend)
  return(chart)
}
charts <- map2(stores, c(T, F, F, F), plotSpend)
chart <- subplot(charts, nrows = 2, shareX = TRUE, shareY = TRUE, margin = 0.03) %>%
  add_annotations("Grocery", x = 0.1, y = 0.9, xref = "paper",
                  yref = "paper", showarrow = FALSE, font = list(size = 30)) %>%
  add_annotations("Warehouse Club", x = 0.75, y = 0.95, xref = "paper",
                  yref = "paper", showarrow = FALSE, font = list(size = 30)) %>%
  add_annotations("Discount Store", x = 0.1, y = 0.25, xref = "paper",
                  yref = "paper", showarrow = FALSE, font = list(size = 30)) %>%
  add_annotations("Dollar Store", x = 0.8, y = 0.25, xref = "paper",
                  yref = "paper", showarrow = FALSE, font = list(size = 30)) %>%
  layout(title = "Annual Visit Probability by Channel and Income (2004-2016)",
         titlefont = list(size = 35),
         annotations = list(text = "Source: Nielsen.",
                            font = list(size = 20),
                            showarrow = FALSE, align = "left", valign = "bottom",
                            xref = "paper", x = -0.03, yref = "paper", y = -0.22))
export(chart, paste0("./code/5_figures/nielsenChannelPropensitiesVisit.png"))
