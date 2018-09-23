library(data.table)
library(purrr)
library(furrr)
library(lfe)
library(stargazer)
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
               select = c("household_code", "panel_year", "projection_factor",
                          "household_income", "household_size", "marital_status",
                          "race", "hispanic_origin", "age"))
panel[, "household_income" := ordered(household_income, levels = c(">100k", "50-100k", "25-50k", "<25k"))]

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
  trips <- merge(trips, retailers, by = "retailer_code")

  # Getting monthly spending by channel and household
  trips <- trips[, .(uniqueRet = uniqueN(retailer_code)),
                 by = .(household_code, panel_year, channel_type)]
  fullData <- merge(trips, panel, all.y = TRUE,
                    by = c("household_code", "panel_year", "channel_type"))
  fullData[is.na(uniqueRet), "uniqueRet" := 0]
  return(fullData)
}

fullData <- rbindlist(future_map(yr, getData))

graphData <- fullData[, .(uniqueRet = weighted.mean(uniqueRet, w = projection_factor)),
                      by = .(household_income, panel_year, channel_type)]

# Spending (Shares)
plotSpend <- function(storeType, legend) {
  chart <- plot_ly(data = graphData[channel_type == storeType], x = ~panel_year,
                   height = 800, width = 1200) %>%
    add_lines(data = graphData[channel_type == storeType & household_income == "<25k"],
              y = ~uniqueRet, line = list(width = 5, color = "red"), name = "<$25k") %>%
    add_lines(data = graphData[channel_type == storeType & household_income == "25-50k"],
              y = ~uniqueRet, line = list(width = 5, color = "green"), name = "$25-$50k") %>%
    add_lines(data = graphData[channel_type == storeType & household_income == "50-100k"],
              y = ~uniqueRet, line = list(width = 5, color = "orange"), name = "50k-100k") %>%
    add_lines(data = graphData[channel_type == storeType & household_income == ">100k"],
              y = ~uniqueRet, line = list(width = 5, color = "blue"), name = ">$100k") %>%
    layout(xaxis = list(title = "Year", titlefont = list(size = 30),
                        tickfont = list(size = 25)),
           yaxis = list(title = "Unique Retailers", range = c(0, 4), dtick = 1,
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
  layout(title = "Unique Retailers Visited by Channel and Income (2004-2016)",
         titlefont = list(size = 35),
         annotations = list(text = "Source: Nielsen.",
                            font = list(size = 20),
                            showarrow = FALSE, align = "left", valign = "bottom",
                            xref = "paper", x = -0.03, yref = "paper", y = -0.22))
export(chart, paste0("./code/5_figures/uniqueRetailersIncome.png"))

# Warehouse Clubs
reg1 <- felm(data = fullData[channel_type == "Warehouse Club"],
             uniqueRet ~ household_income,
             weights = fullData[channel_type == "Warehouse Club"]$projection_factor)

reg2 <- felm(data = fullData[channel_type == "Warehouse Club"],
             uniqueRet ~ household_income | as.factor(panel_year) +
               as.factor(household_size) + as.factor(marital_status) +
               as.factor(race) + as.factor(hispanic_origin) + as.factor(age),
             weights = fullData[channel_type == "Warehouse Club"]$projection_factor)

stargazer(reg1, reg2, type = "text")

# Discount Stores
reg1 <- felm(data = fullData[channel_type == "Discount Store"],
             uniqueRet ~ household_income,
             weights = fullData[channel_type == "Discount Store"]$projection_factor)

reg2 <- felm(data = fullData[channel_type == "Discount Store"],
             uniqueRet ~ household_income | as.factor(panel_year) +
               as.factor(household_size) + as.factor(marital_status) +
               as.factor(race) + as.factor(hispanic_origin) + as.factor(age),
             weights = fullData[channel_type == "Discount Store"]$projection_factor)

stargazer(reg1, reg2, type = "text")

# Dollar Store
reg1 <- felm(data = fullData[channel_type == "Dollar Store"],
             uniqueRet ~ household_income,
             weights = fullData[channel_type == "Dollar Store"]$projection_factor)

reg2 <- felm(data = fullData[channel_type == "Dollar Store"],
             uniqueRet ~ household_income | as.factor(panel_year) +
               as.factor(household_size) + as.factor(marital_status) +
               as.factor(race) + as.factor(hispanic_origin) + as.factor(age),
             weights = fullData[channel_type == "Dollar Store"]$projection_factor)

stargazer(reg1, reg2, type = "text")

# Grocery
reg1 <- felm(data = fullData[channel_type == "Grocery"],
             uniqueRet ~ household_income,
             weights = fullData[channel_type == "Grocery"]$projection_factor)

reg2 <- felm(data = fullData[channel_type == "Grocery"],
             uniqueRet ~ household_income | as.factor(panel_year) +
               as.factor(household_size) + as.factor(marital_status) +
               as.factor(race) + as.factor(hispanic_origin) + as.factor(age),
             weights = fullData[channel_type == "Grocery"]$projection_factor)

stargazer(reg1, reg2, type = "text")
