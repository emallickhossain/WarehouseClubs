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
legend <- c(T, F, F, F)
stores <- c("Grocery", "Warehouse Club", "Discount Store", "Dollar Store")

rec <- setDT(fredr("USRECQ", observation_start = as.Date("2004-01-01")))

# Getting retailers
retailers <- fread(paste0(path, "retailers.tsv"))
retailers <- retailers[channel_type %in% stores]

# Classifying generic products (brand code = 536746)
prod <- fread(paste0(path, "prod.csv"), select = c("upc", "upc_ver_uc", "brand_code_uc"))
prod[, "generic" := ifelse(brand_code_uc == 536746, 1L, 0L)]

panel <- fread(paste0(path, "fullPanel.csv"),
               select = c("household_code", "panel_year", "household_income", "projection_factor"))

# Getting panel demographic data
getData <- function(yr) {
  panel <- panel[panel_year == yr]

  # Getting Nielsen purchases by trip
  purch <- fread(paste0(path, "Purchases/purchase", yr, ".csv"))
  purch <- merge(purch, prod, by = c("upc", "upc_ver_uc"))
  purch[, c("upc", "upc_ver_uc", "brand_code_uc", "department_code") := NULL]
  purch <- purch[, .(spend = sum(total_price_paid)), by = .(trip_code_uc, generic)]

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
                    by = .(retailer_code, household_code, panel_year, quarter, generic)]
  fullData[, "date" := parse_date_time2(paste0(panel_year, quarter), "%Y%q")]
  fullData[, "quarter" := NULL]

  # Getting channel type
  fullData <- merge(fullData, retailers, by = "retailer_code")
  fullData <- fullData[, .(spend = sum(spend)),
                       by = .(household_code, panel_year, date, generic, channel_type)]
  fullData <- merge(fullData, panel, by = c("household_code", "panel_year"))
  avgSpend <- fullData[, .(spend = weighted.mean(spend, w = projection_factor)),
                       by = .(panel_year, date, generic, channel_type, household_income)]
  avgSpend[, "totalSpend" := sum(spend), by = .(panel_year, date, channel_type, household_income)]
  avgSpend[, "spendShare" := spend / totalSpend * 100]
  return(avgSpend)
}

fullData <- rbindlist(future_map(yr, getData))
graphData <- fullData[generic == 1]
setorder(graphData, channel_type, household_income, date)
graphData[, "rollMean" := rollmean(spendShare, 4, fill = c(NA, NULL, NA), align = "right"),
          by = .(channel_type, household_income)]
getPlots <- function(storeType, legend) {
  p <- plot_ly(data = graphData, x = ~date, width = 1200, height = 800, showlegend = legend) %>%
    add_lines(data = graphData[channel_type == storeType & household_income == "<25k"],
              y = ~rollMean, line = list(width = 5, color = "red"), name = "<$25k") %>%
    # add_lines(data = graphData[channel_type == storeType & household_income == "25-50k"],
    #           y = ~spendShare, line = list(width = 5, color = "green"), name = "$25-$50k") %>%
    # add_lines(data = graphData[channel_type == storeType & household_income == "50-100k"],
    #           y = ~spendShare, line = list(width = 5, color = "orange"), name = "50k-100k") %>%
    add_lines(data = graphData[channel_type == storeType & household_income == ">100k"],
              y = ~rollMean, line = list(width = 5, color = "blue"), name = ">$100k") %>%
    add_lines(data = rec, x = ~date, y = ~value * 1000,
              line = list(width = 0),
              fill = "tozeroy",
              fillcolor = "rgba(64, 64, 64, 0.2)",
              showlegend = F,
              hoverinfo = "none") %>%
    layout(xaxis = list(title = "Year", titlefont = list(size = 30),
                        tickfont = list(size = 25)),
           yaxis = list(title = "Generic Share", range = c(0, 30), dtick = 5,
                        titlefont = list(size = 30), tickfont = list(size = 25)),
           # Adjust margins so things look nice
           margin = list(l = 80, r = 100, t = 60, b = 150, pad = 10))
  return(p)
}

charts <- map2(stores, legend, getPlots)
chart <- subplot(charts, nrows = 2, shareX = TRUE, shareY = TRUE, margin = 0.03) %>%
  add_annotations("Grocery", x = 0.1, y = 0.7, xref = "paper",
                  yref = "paper", showarrow = FALSE, font = list(size = 30)) %>%
  add_annotations("Warehouse Club", x = 0.8, y = 0.75, xref = "paper",
                  yref = "paper", showarrow = FALSE, font = list(size = 30)) %>%
  add_annotations("Discount Store", x = 0.1, y = 0.15, xref = "paper",
                  yref = "paper", showarrow = FALSE, font = list(size = 30)) %>%
  add_annotations("Dollar Store", x = 0.8, y = 0.2, xref = "paper",
                  yref = "paper", showarrow = FALSE, font = list(size = 30)) %>%
  layout(title = "Generic Shares by Channel and Income (2004-2016)",
         titlefont = list(size = 35),
         annotations = list(text = "Source: Nielsen. <br>Note: Four-quarter moving average plotted.",
                            font = list(size = 20),
                            showarrow = FALSE, align = "left", valign = "bottom",
                            xref = "paper", x = -0.03, yref = "paper", y = -0.22))

export(chart, "./code/5_figures/genericByChannelIncome.png")
