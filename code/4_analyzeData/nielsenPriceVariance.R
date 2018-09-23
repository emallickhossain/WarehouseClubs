# Gets price variance for Nielsen transactions
library(data.table)
library(plotly)
library(purrr)
library(Hmisc)

# Only getting general merchandise products
products <- fread("/home/mallick/Desktop/Nielsen/nielsen_extracts/HMS/Master_Files/Latest/products.tsv",
                  select = c("upc", "upc_ver_uc", "department_code"))
products <- products[department_code == 9]

getNielsenProducts <- function(year) {
  purchases <- fread(paste0("/home/mallick/Desktop/Nielsen/nielsen_extracts/HMS/",
                            year, "/Annual_Files/purchases_", year, ".tsv"),
                     select = c("trip_code_uc", "upc", "upc_ver_uc", "total_price_paid"))
  purchases <- merge(purchases, products, by = c("upc", "upc_ver_uc"))

  # Only getting online, discount, department, and hypermarket stores
  trips <- fread(paste0("/home/mallick/Desktop/Nielsen/nielsen_extracts/HMS/",
                        year, "/Annual_Files/trips_", year, ".tsv"),
                 select = c("trip_code_uc", "retailer_code", "household_code",
                            "panel_year", "purchase_date"))
  trips <- trips[retailer_code %in% c(4700:4849, 6901:6999, 9001:9099, 8401:8499)]
  trips[, "purchase_date" := as.Date(purchase_date)]
  trips <- trips[year(purchase_date) == year]

  full_data <- merge(purchases, trips, by = "trip_code_uc")

  # Getting panelists
  panel <- fread(paste0("/home/mallick/Desktop/Nielsen/nielsen_extracts/HMS/",
                        year, "/Annual_Files/panelists_", year, ".tsv"),
                 select = c("household_code", "panel_year", "projection_factor"))
  full_data <- merge(full_data, panel, by = c("household_code", "panel_year"))

  full_data[, c("trip_code_uc", "department_code") := NULL]
  return(full_data)
}

nielsenPurchases <- rbindlist(map(2004:2015, getNielsenProducts))
fwrite(nielsenPurchases, "/home/mallick/Desktop/Nielsen/Data/nielsenPurchases.csv")

totalSales <- nielsenPurchases[, sum(total_price_paid), by = retailer_code]
setorder(totalSales, -V1)

# Plotting chart by retailer
plotVar <- function(retailer) {
  print(retailer)

  final_data <- nielsenPurchases[retailer_code %in% retailer,
                                 .(avgPrice = weighted.mean(total_price_paid,
                                                            w = projection_factor),
                                   lowerBound = wtd.quantile(total_price_paid,
                                                             weights = projection_factor,
                                                             probs = 0.05),
                                   upperBound = wtd.quantile(total_price_paid,
                                                             weights = projection_factor,
                                                             probs = 0.95),
                                   varPrice = wtd.var(total_price_paid,
                                                      weights = projection_factor)),
                                 by = .(year(purchase_date), month(purchase_date))]
  final_data[, "date" := as.Date(paste0(year, "-", month, "-01"))]

  chart <- plot_ly(data = final_data, x = ~date, height = 800, width = 1200) %>%
    add_lines(y = ~upperBound, name = "95%", line = list(color = "rgb(211,211,211)")) %>%
    add_lines(y = ~lowerBound, name = "5%", fill = "tonexty",
              line = list(color = "rgb(211,211,211)"), fillcolor = "rgb(211,211,211)") %>%
    add_lines(y = ~avgPrice, name = "mean", line = list(color = "black")) %>%
    layout(title = "Price Variation",
           titlefont = list(size = 35),
           xaxis = list(title = "Date", titlefont = list(size = 30),
                        tickfont = list(size = 25)),
           yaxis = list(title = "Price ($)",
                        titlefont = list(size = 30), tickfont = list(size = 25)),
           showlegend = FALSE,
           # Adjust margins so things look nice
           margin = list(l = 80, r = 50, t = 60, b = 150, pad = 10),
           annotations = list(text = "Source: Nielsen.",
                              font = list(size = 20),
                              showarrow = FALSE,
                              align = "left", valign = "bottom",
                              xref = "paper", x = -0.03,
                              yref = "paper", y = -0.2))
  return(chart)
}

topSeller <- plotVar(totalSales$retailer_code[1])
export(topSeller, "./code/5_figures/NielsenTopSellerVar.png")

seller5 <- plotVar(totalSales$retailer_code[5])
export(seller5, "./code/5_figures/nielsenSeller5.png")

seller7 <- plotVar(totalSales$retailer_code[7])
export(seller7, "./code/5_figures/nielsenSeller7.png")

seller9 <- plotVar(totalSales$retailer_code[9])
export(seller9, "./code/5_figures/nielsenSeller9.png")

# Plot of just variance
plot_ly(data = final_data, x = ~date) %>%
  add_lines(y = ~varPrice, name = "SD")
