# Payment methods
library(data.table)
library(plotly)
library(purrr)
library(lubridate)
library(forcats)
path <- "/home/mallick/Desktop/Nielsen/Data/Clean/"
yr <- 2013:2016
income <- c("<25k", "25-50k", "50-100k", ">100k")
legend <- c(T, F, F, F)
stores <- c("Grocery", "Warehouse Club", "Discount Store", "Dollar Store")

retailers <- fread(paste0(path, "retailers.tsv"))[channel_type %in% stores]
panel <- fread(paste0(path, "fullPanel.csv"),
               select = c("household_code", "panel_year", "projection_factor", "household_income"))

getData <- function(yr) {
  # Getting Nielsen purchases by trip
  purch <- fread(paste0(path, "Purchases/purchase", yr, ".csv"),
                 select = c("trip_code_uc", "total_price_paid"))
  purch <- purch[, .(spend = sum(total_price_paid)), by = .(trip_code_uc)]

  # Merging trips
  trips <- fread(paste0(path, "Trips/trips", yr, ".csv"),
                 select = c("trip_code_uc", "household_code", "retailer_code",
                            "panel_year", "purchase_date", "method_of_payment_cd"))
  trips[, "method_of_payment_cd" := fct_collapse(as.factor(method_of_payment_cd),
                                                 none = paste(0),
                                                 cashCheck = paste(c(1, 2)),
                                                 credit = paste(3:7),
                                                 debit = paste(8),
                                                 other = paste(9))]
  purch <- merge(trips, purch, by = "trip_code_uc")
  purch[, "month" := as.integer(substr(purchase_date, 6, 7))]
  purch[, "quarter" := ifelse(month %in% 1:3, 1L,
                              ifelse(month %in% 4:6, 2L,
                                     ifelse(month %in% 7:9, 3L, 4L)))]
  purch[, "date" := parse_date_time2(paste0(panel_year, quarter), "%Y%q")]
  purch[, c("trip_code_uc", "quarter", "purchase_date", "month") := NULL]
  fullData <- merge(purch, retailers, by = "retailer_code")

  finalData <- fullData[, .(spend = sum(spend)),
                        by = .(household_code, panel_year, method_of_payment_cd,
                               channel_type, date)]
}

fullData <- rbindlist(map(yr, getData))

fullData <- merge(fullData, panel, by = c("household_code", "panel_year"))

# Aggregate spending shares by payment type
graphData <- fullData[!method_of_payment_cd == "none", .(spend = sum(spend)),
                      by = .(household_code, panel_year, projection_factor,
                             household_income, method_of_payment_cd, date)]
graphData <- graphData[, .(spend = weighted.mean(spend, w = projection_factor)),
                       by = .(household_income, method_of_payment_cd, date)]
graphData[, "totalSpend" := sum(spend), by = .(household_income, date)]
graphData[, "share" := spend / totalSpend * 100]

getPlots <- function(income, legend) {
  apple <- graphData[household_income == income]
  p <- plot_ly(data = apple, x = ~date, width = 1200, height = 800, showlegend = legend) %>%
    add_lines(data = apple[method_of_payment_cd == "cashCheck"], y = ~share,
              line = list(width = 5, color = "green"), name = "Cash/Check") %>%
    add_lines(data = apple[method_of_payment_cd == "credit"], y = ~share,
              line = list(width = 5, color = "red"), name = "Credit") %>%
    add_lines(data = apple[method_of_payment_cd == "debit"], y = ~share,
              line = list(width = 5, color = "blue"), name = "Debit") %>%
    layout(xaxis = list(title = "Year", titlefont = list(size = 30),
                        tickfont = list(size = 25)),
           yaxis = list(title = "Share", range = c(0, 50), dtick = 10,
                        titlefont = list(size = 30), tickfont = list(size = 25)),
           # Adjust margins so things look nice
           margin = list(l = 80, r = 100, t = 60, b = 150, pad = 10))
  return(p)
}

charts <- map2(income, legend, getPlots)
chart <- subplot(charts, nrows = 2, shareX = TRUE, shareY = TRUE, margin = 0.03) %>%
  add_annotations("<25k", x = 0.1, y = 0.7, xref = "paper",
                  yref = "paper", showarrow = FALSE, font = list(size = 30)) %>%
  add_annotations("25-50k", x = 0.8, y = 0.75, xref = "paper",
                  yref = "paper", showarrow = FALSE, font = list(size = 30)) %>%
  add_annotations("50-100k", x = 0.1, y = 0.15, xref = "paper",
                  yref = "paper", showarrow = FALSE, font = list(size = 30)) %>%
  add_annotations(">100k", x = 0.8, y = 0.2, xref = "paper",
                  yref = "paper", showarrow = FALSE, font = list(size = 30)) %>%
  layout(title = "Payment Types by Income (2004-2016)",
         titlefont = list(size = 35),
         annotations = list(text = "Source: Nielsen.",
                            font = list(size = 20),
                            showarrow = FALSE, align = "left", valign = "bottom",
                            xref = "paper", x = -0.03, yref = "paper", y = -0.22))


# Spending shares by store type
graphData <- fullData[, .(spend = sum(spend)),
                      by = .(household_code, panel_year, projection_factor,
                             household_income, method_of_payment_cd, date, channel_type)]
graphData <- graphData[, .(spend = weighted.mean(spend, w = projection_factor)),
                       by = .(household_income, method_of_payment_cd, date, channel_type)]
graphData[, "totalSpend" := sum(spend), by = .(household_income, date, channel_type)]
graphData[, "share" := spend / totalSpend * 100]

plot_ly(data = graphData[channel_type == "Grocery" &
                           method_of_payment_cd == "debit"], x = ~date) %>%
  add_lines(y = ~share, split = ~household_income)
