# Get popular store channels
library(data.table)
library(purrr)
library(plotly)
path <- "/home/mallick/Desktop/Nielsen/Data/Clean/"
yr <- 2004:2016

# Getting retailers
retailers <- fread(paste0(path, "retailers.tsv"))
panel <- fread(paste0(path, "fullPanel.csv"),
               select = c("household_code", "panel_year", "projection_factor", "household_income"))

# Getting panel demographic data
getData <- function(yr) {
  print(yr)
  panel <- panel[panel_year == yr]
  channels <- setDT(expand.grid(household_code = unique(panel$household_code),
                                panel_year = yr,
                                channel_type = unique(retailers$channel_type)))
  panel <- merge(channels, panel, by = c("household_code", "panel_year"), all.x = TRUE)

  # Getting Nielsen purchases by trip
  purch <- fread(paste0(path, "Purchases/purchase", yr, ".csv"),
                 select = c("trip_code_uc", "total_price_paid"))
  purch <- purch[, .(spend = sum(total_price_paid)), by = .(trip_code_uc)]

  # Getting trips
  trips <- fread(paste0(path, "Trips/trips", yr, ".csv"),
                 select = c("trip_code_uc", "household_code", "retailer_code", "panel_year"))

  # Merging with purchases
  purch <- merge(trips, purch, by = "trip_code_uc")
  fullData <- merge(purch, retailers, by = "retailer_code")
  fullData[, "totalSpend" := sum(spend), by = .(household_code, panel_year)]
  channelSpend <- fullData[, .(spend = sum(spend)),
                           by = .(household_code, panel_year, channel_type, totalSpend)]

  channelSpend <- merge(channelSpend, panel,
                        by = c("household_code", "panel_year", "channel_type"), all.y = TRUE)
  channelSpend[is.na(spend), "spend" := 0]
  channelSpend[is.na(totalSpend), "totalSpend" := -999]
  channelSpend[, "share" := spend / totalSpend * 100]

  finalData <- channelSpend[, .(share = weighted.mean(share, w = projection_factor)),
                            by = .(panel_year, channel_type)]
  return(finalData)
}

finalData <- rbindlist(map(yr, getData))
setorder(finalData, panel_year, -share)
graphData <- finalData[, .SD[1:5], by = panel_year]

chart <- plot_ly(data = graphData, x = ~panel_year, width = 1200, height = 800) %>%
  add_lines(y = ~share, split = ~channel_type, line = list(width = 5)) %>%
  layout(title = "Popular Spending Modes (2004-2016)",
         titlefont = list(size = 35),
         xaxis = list(title = "Year", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "Spending Share", range = c(0, 60), dtick = 10,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Adjust margins so things look nice
         margin = list(l = 80, r = 100, t = 60, b = 150, pad = 10),
         annotations = list(text = "Source: Nielsen.",
                            font = list(size = 20),
                            showarrow = FALSE, align = "left", valign = "bottom",
                            xref = "paper", x = -0.03, yref = "paper", y = -0.22))
export(chart, "./code/5_figures/popularChannels.png")
