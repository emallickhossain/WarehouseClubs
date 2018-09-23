# Generic HHI by channel
# Get generic brand spending by channel type
library(data.table)
library(purrr)
library(furrr)
library(plotly)
library(lubridate)
library(fredr)
library(zoo)
fredr_set_key(fredAPI)
plan(multiprocess)
path <- "/home/mallick/Desktop/Nielsen/Data/Clean/"
yr <- 2004:2016
mod <- 7260
stores <- c("Grocery", "Warehouse Club", "Discount Store", "Dollar Store")

rec <- setDT(fredr("USRECQ", observation_start = as.Date("2004-01-01")))

# Getting retailers
retailers <- fread(paste0(path, "retailers.tsv"))
retailers <- retailers[channel_type %in% stores]

# Classifying generic products (brand code = 536746)
prod <- fread(paste0(path, "prod.csv"),
              select = c("upc", "upc_ver_uc", "brand_code_uc", "product_module_code"))
prod <- prod[product_module_code == mod]

panel <- fread(paste0(path, "fullPanel.csv"),
               select = c("household_code", "panel_year", "projection_factor"))

# Getting panel demographic data
getData <- function(yr) {
  panel <- panel[panel_year == yr]

  # Getting Nielsen purchases by trip
  purch <- fread(paste0(path, "Purchases/purchase", yr, ".csv"))
  purch <- merge(purch, prod, by = c("upc", "upc_ver_uc"))
  purch[, c("upc", "upc_ver_uc", "department_code") := NULL]
  purch <- purch[, .(spend = sum(total_price_paid)), by = .(trip_code_uc, brand_code_uc)]

  # Merging with purchases and getting spending by store and generic
  trips <- fread(paste0(path, "Trips/trips", yr, ".csv"),
                 select = c("trip_code_uc", "retailer_code", "household_code",
                            "panel_year", "purchase_date"))
  purch <- merge(trips, purch, by = "trip_code_uc")
  purch[, "trip_code_uc" := NULL]
  purch[, "month" := as.integer(substr(purchase_date, 6, 7))]
  purch[, "quarter" := ifelse(month %in% 1:3, 1L,
                              ifelse(month %in% 4:6, 2L,
                                     ifelse(month %in% 7:9, 3L, 4L)))]
  fullData <- purch[, .(spend = sum(spend)),
                    by = .(retailer_code, household_code, panel_year, quarter, brand_code_uc)]
  fullData[, "date" := parse_date_time2(paste0(panel_year, quarter), "%Y%q")]
  fullData[, "quarter" := NULL]

  # Getting channel type
  fullData <- merge(fullData, retailers, by = "retailer_code")
  fullData <- fullData[, .(spend = sum(spend)),
                       by = .(household_code, panel_year, date, brand_code_uc, channel_type)]
  fullData <- merge(fullData, panel, by = c("household_code", "panel_year"))
  avgSpend <- fullData[, .(spend = weighted.mean(spend, w = projection_factor)),
                       by = .(panel_year, date, brand_code_uc, channel_type)]
  avgSpend[, "totalSpend" := sum(spend), by = .(panel_year, date, channel_type)]
  avgSpend[, "spendShare" := spend / totalSpend]
  return(avgSpend)
}

fullData <- rbindlist(future_map(yr, getData))

hhi <- fullData[, .(hhi = sum(spendShare ^ 2)), by = .(panel_year, date, channel_type)]
setorder(hhi, channel_type, date)
hhi[, "rollMean" := rollmean(hhi, 4, fill = c(NA, NULL, NA), align = "right"), by = channel_type]
chart <- plot_ly(data = hhi, x = ~date, height = 800, width = 1200) %>%
  add_lines(y = ~rollMean, split = ~channel_type, line = list(width = 5)) %>%
  add_lines(data = rec, x = ~date, y = ~value * 1000,
            line = list(width = 0),
            fill = "tozeroy",
            fillcolor = "rgba(64, 64, 64, 0.2)",
            showlegend = F,
            hoverinfo = "none") %>%
  layout(title = "Herfindahl Index by Channel (2004-2016)",
         titlefont = list(size = 35),
         annotations = list(text = "Source: Nielsen. <br> Note: Four-quarter rolling averages are plotted.",
                            font = list(size = 20),
                            showarrow = FALSE, align = "left", valign = "bottom",
                            xref = "paper", x = -0.03, yref = "paper", y = -0.22),
         xaxis = list(title = "Year", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "Spending Share", range = c(0, 0.15), dtick = 0.03,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Adjust margins so things look nice
         margin = list(l = 80, r = 100, t = 60, b = 150, pad = 10))

export(chart, "./code/5_figures/genericHHI.png")
