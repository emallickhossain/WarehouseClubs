# Plots Nielsen HHI for general merchandise sales
library(data.table)
library(plotly)
library(bit64)

nielsenPurchases <- fread("/home/mallick/Desktop/Nielsen/Data/nielsenPurchases.csv")
nielsenPurchases[, "purchase_date" := as.Date(purchase_date)]
nielsenPurchases <- nielsenPurchases[, .(spending = sum(total_price_paid)),
                                     by = .(year(purchase_date), month(purchase_date),
                                            household_code, panel_year,
                                            projection_factor, upc, upc_ver_uc)]

# Removing apparel category
nielsenPurchases[!upc %in% 7582804843, "monthlySpending" := sum(spending * projection_factor),
                 by = .(year, month)]
upcSpending <- unique(nielsenPurchases[, .(spendingShare = sum(spending * projection_factor) / monthlySpending),
                                by = .(year, month, upc, upc_ver_uc)])
setorder(upcSpending, -spendingShare)

# Computing HHI
hhi <- upcSpending[, .(hhi = sum(spendingShare ^ 2, na.rm = TRUE)),
                   by = .(year, month)]
hhi[, "date" := as.Date(paste0(year, "-", month, "-01"))]

plot_ly(data = hhi, x = ~date, height = 800, width = 1200) %>%
  add_lines(y = ~hhi) %>%
  layout(title = "HH Index at UPC Level",
         titlefont = list(size = 35),
         xaxis = list(title = "Year", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "HH Index", range = c(0, 0.002), dtick = 0.001,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.85, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 120, r = 50, t = 60, b = 150, pad = 10),
         annotations = list(text = "Source: Nielsen.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.2))

# Computing HHI at module level
products <- fread("/home/mallick/Desktop/Nielsen/nielsen_extracts/HMS/Master_Files/Latest/products.tsv",
                  select = c("product_group_code", "product_module_code", "upc", "upc_ver_uc"))
modulePurchases <- merge(products, nielsenPurchases, by = c("upc", "upc_ver_uc"))
modulePurchases <- modulePurchases[, .(spending = sum(spending)),
                                   by = .(year, month, household_code, panel_year,
                                          projection_factor, product_module_code)]

# Removing apparel reference card category added in 2012 and prerecorded video
modulePurchases[!product_module_code %in% c(461, 8900),
                "monthlySpending" := sum(spending * projection_factor),
                by = .(year, month)]
upcSpendingModule <- unique(modulePurchases[, .(spendingShare = sum(spending * projection_factor) /
                                                                monthlySpending),
                                             by = .(year, month, product_module_code)])
setorder(upcSpendingModule, -spendingShare)

# Computing HHI
hhiModule <- upcSpendingModule[, .(hhi = sum(spendingShare ^ 2)),
                               by = .(year, month)]
hhiModule[, "date" := as.Date(paste0(year, "-", month, "-01"))]

plot_ly(data = hhiModule, x = ~date, height = 800, width = 1200) %>%
  add_lines(y = ~hhi) %>%
  layout(title = "HH Index at Module Level",
         titlefont = list(size = 35),
         xaxis = list(title = "Year", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "HH Index", range = c(0, 0.12), dtick = 0.02,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.85, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 120, r = 50, t = 60, b = 150, pad = 10),
         annotations = list(text = "Source: Nielsen.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.2))
