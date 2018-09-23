# Gets top channels by product module
library(data.table)
library(purrr)
library(furrr)
library(plotly)
plan(multiprocess)
path <- "/home/mallick/Desktop/Nielsen/Data/Clean/"
yr <- 2004:2016
modCode <- 7734
stores <- c("Grocery", "Warehouse Club", "Discount Store", "Dollar Store")

# Getting retailers
retailers <- fread(paste0(path, "retailers.tsv"))[channel_type %in% stores]
panel <- fread(paste0(path, "fullPanel.csv"),
               select = c("household_code", "panel_year", "projection_factor", "household_income"))
prod <- fread(paste0(path, "prod.csv"),
              select = c("upc", "upc_ver_uc", "product_module_code",
                         "brand_code_uc", "multi", "size1_amount", "size1_units"))
prod[, "size" := multi * size1_amount]

# Getting panel demographic data
getData <- function(yr, modCode) {
  panel <- panel[panel_year == yr]
  channels <- setDT(expand.grid(household_code = unique(panel$household_code),
                                panel_year = yr,
                                channel_type = unique(retailers$channel_type)))
  panel <- merge(channels, panel, by = c("household_code", "panel_year"), all.x = TRUE)

  upcs <- prod[product_module_code == modCode, .(upc, upc_ver_uc, size, size1_units)]
  # Getting Nielsen purchases by trip
  purch <- fread(paste0(path, "Purchases/purchase", yr, ".csv"))
  purch <- merge(purch, upcs, by = c("upc", "upc_ver_uc"))
  purch <- purch[, .(spend = sum(total_price_paid),
                     units = sum(quantity * size)), by = .(trip_code_uc)]

  # Getting trips
  trips <- fread(paste0(path, "Trips/trips", yr, ".csv"),
                 select = c("trip_code_uc", "household_code", "retailer_code", "panel_year"))

  # Merging with purchases
  purch <- merge(trips, purch, by = "trip_code_uc")
  fullData <- merge(purch, retailers, by = "retailer_code")
  fullData[, ':=' (totalSpend = sum(spend),
                   totalUnits = sum(units)), by = .(household_code, panel_year)]
  channelSpend <- fullData[, .(spend = sum(spend),
                               units = sum(units)),
                           by = .(household_code, panel_year, channel_type,
                                  totalSpend, totalUnits)]
  channelSpend <- merge(channelSpend, panel, all.y = TRUE,
                        by = c("household_code", "panel_year", "channel_type"))
  channelSpend[is.na(spend), "spend" := 0]
  channelSpend[is.na(totalSpend), "totalSpend" := -999]
  channelSpend[is.na(units), "units" := 0]
  channelSpend[is.na(totalUnits), "totalUnits" := -999]
  channelSpend[, ':=' (spendShare = spend / totalSpend * 100,
                       unitShare = units / totalUnits * 100)]

  finalData <- channelSpend[, .(spendShare = weighted.mean(spendShare, w = projection_factor),
                                unitShare = weighted.mean(unitShare, w = projection_factor)),
                            by = .(panel_year, channel_type, household_income)]
  return(finalData)
}

finalData <- rbindlist(future_map(yr, getData, modCode = modCode))
graphData <- finalData

getPlots <- function(storeType) {
  p <- plot_ly(data = graphData, x = ~panel_year, width = 1200, height = 800) %>%
    add_lines(data = graphData[channel_type == storeType & household_income == "<25k"],
              y = ~unitShare, line = list(width = 5, color = "red"), name = "<$25k") %>%
    add_lines(data = graphData[channel_type == storeType & household_income == "25-50k"],
              y = ~unitShare, line = list(width = 5, color = "green"), name = "$25-$50k") %>%
    add_lines(data = graphData[channel_type == storeType & household_income == "50-100k"],
              y = ~unitShare, line = list(width = 5, color = "orange"), name = "50k-100k") %>%
    add_lines(data = graphData[channel_type == storeType & household_income == ">100k"],
              y = ~unitShare, line = list(width = 5, color = "blue"), name = ">$100k") %>%
    layout(xaxis = list(title = "Year", titlefont = list(size = 30),
                        tickfont = list(size = 25)),
           yaxis = list(title = "Spending Share", range = c(0, 50), dtick = 10,
                        titlefont = list(size = 30), tickfont = list(size = 25)),
           # Adjust margins so things look nice
           margin = list(l = 80, r = 100, t = 60, b = 150, pad = 10))
  return(p)
}

charts <- map(stores, getPlots)
chart <- subplot(charts, nrows = 2, shareX = TRUE, shareY = TRUE, margin = 0.03) %>%
  add_annotations("Grocery", x = 0.1, y = 0.7, xref = "paper",
                  yref = "paper", showarrow = FALSE, font = list(size = 30)) %>%
  add_annotations("Warehouse Club", x = 0.8, y = 0.9, xref = "paper",
                  yref = "paper", showarrow = FALSE, font = list(size = 30)) %>%
  add_annotations("Discount Store", x = 0.1, y = 0.3, xref = "paper",
                  yref = "paper", showarrow = FALSE, font = list(size = 30)) %>%
  add_annotations("Dollar Store", x = 0.8, y = 0.3, xref = "paper",
                  yref = "paper", showarrow = FALSE, font = list(size = 30)) %>%
  layout(title = "Monthly Expenditures by Channel and Income (2004-2016)",
         titlefont = list(size = 35),
         annotations = list(text = "Source: Nielsen.",
                            font = list(size = 20),
                            showarrow = FALSE, align = "left", valign = "bottom",
                            xref = "paper", x = -0.03, yref = "paper", y = -0.22))
