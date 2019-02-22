# Quantifies "large" volume purchases as in Coibion 2017
library(data.table)
library(purrr)
library(furrr)
library(plotly)
library(lfe)
plan(multiprocess)
yr <- 2004:2016
path <- "/home/mallick/Desktop/Nielsen/Data/Clean/"

prod <- fread(paste0(path, "prod.csv"),
              select = c("upc", "upc_ver_uc", "product_module_code", "size1_units", "large"))
panel <- fread(paste0(path, "fullPanel.csv"))

getLarge <- function(yr) {
  purch <- fread(paste0(path, "Purchases/purchase", yr, ".csv"),
                 select = c("upc", "upc_ver_uc", "total_price_paid", "household_code", "panel_year"))
  purch <- merge(purch, prod, by = c("upc", "upc_ver_uc"))
  shares <- purch[, .(spend = sum(total_price_paid),
                      large = mean(large)),
                  by = .(household_code, panel_year, product_module_code, size1_units)]
  return(shares)
}

sizes <- rbindlist(future_map(yr, getLarge))

# Aggregate chart
agg <- sizes[, .(large = weighted.mean(large, w = spend)),
             by = .(household_code, panel_year, size1_units)]
agg <- merge(agg, panel, by = c("household_code", "panel_year"))
graphData <- agg[, .(large = weighted.mean(large, w = projection_factor)),
                 by = .(panel_year, size1_units)]

chart <- plot_ly(data = graphData[size1_units %in% c("OZ", "CT")], x = ~panel_year,
                 height = 800, width = 1200) %>%
  add_lines(y = ~large, line = list(width = 5), split = ~size1_units) %>%
  layout(title = paste0("Share of Large Volume Purchases (2004-2016)"),
         titlefont = list(size = 35),
         xaxis = list(title = "Year", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "Share", range = c(0, 0.15), dtick = 0.03,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.7, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 80, r = 100, t = 60, b = 150, pad = 10),
         annotations = list(text = "Source: Nielsen.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.2))
export(chart, file = "./code/5_figures/largePurchases.png")

# Redoing by income levels
graphData <- agg[, .(large = weighted.mean(large, w = projection_factor)),
                 by = .(panel_year, household_income, size1_units)]

chart <- plot_ly(data = graphData[size1_units == "OZ"], x = ~panel_year, height = 800, width = 1200) %>%
  add_lines(y = ~large, line = list(width = 5), split = ~household_income) %>%
  layout(title = paste0("Share of Large Volume Purchases (2004-2016)"),
         titlefont = list(size = 35),
         xaxis = list(title = "Year", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "Share", range = c(0, 0.15), dtick = 0.03,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.1, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 80, r = 100, t = 60, b = 150, pad = 10),
         annotations = list(text = "Source: Nielsen.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.2))
export(chart, file = "./code/5_figures/largePurchasesIncomeOZ.png")

chart <- plot_ly(data = graphData[size1_units == "CT"], x = ~panel_year, height = 800, width = 1200) %>%
  add_lines(y = ~large, line = list(width = 5), split = ~household_income) %>%
  layout(title = paste0("Share of Large Volume Purchases (2004-2016)"),
         titlefont = list(size = 35),
         xaxis = list(title = "Year", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "Share", range = c(0, 0.15), dtick = 0.03,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.1, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 80, r = 100, t = 60, b = 150, pad = 10),
         annotations = list(text = "Source: Nielsen.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.2))
export(chart, file = "./code/5_figures/largePurchasesIncomeCT.png")

# Regression (CT)
regFE <- felm(data = agg[size1_units == "CT"], large ~ household_income * as.factor(panel_year) |
                household_size + as.factor(marital_status) + as.factor(race) +
                as.factor(hispanic_origin) + market + age + as.factor(college) +
                as.factor(urban) | 0 | market,
              weights = agg[size1_units == "CT"]$projection_factor)

yearFE <- data.table(est = regFE$coefficients, se = regFE$cse, keep.rownames = TRUE)
setnames(yearFE, c("id", "est", "se"))
yearFE[, "id" := gsub("as\\.factor\\(panel_year\\)", "", id)]
yearFE[, "id" := gsub("household_income", "", id)]
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

chart <- plot_ly(data = yearFE,
                 x = ~year, height = 800, width = 1200) %>%
  add_lines(y = ~`est_<25k`, line = list(width = 5),
            error_y = list(array = ~1.96 * `se_<25k`), name = "<25k") %>%
  add_lines(y = ~`est_25-50k`, line = list(width = 5),
            error_y = list(array = ~1.96 * `se_25-50k`), name = "25-50k") %>%
  add_lines(y = ~`est_50-100k`, line = list(width = 5),
            error_y = list(array = ~1.96 * `se_50-100k`), name = "50-100k") %>%
  add_lines(y = ~`est_>100k`, line = list(width = 5),
            error_y = list(array = ~1.96 * `se_>100k`), name = ">100k") %>%
  layout(title = paste0("Year Fixed Effects, Units = Count (2004-2016)"),
         titlefont = list(size = 35),
         xaxis = list(title = "Year", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "Log(Monthly Spending)", range = c(-0, 0.1), dtick = 0.02,
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
export(chart, file = "./code/5_figures/yearFEIncomeSizeCT.png")

# Regression (CT)
regFE <- felm(data = agg[size1_units == "OZ"], large ~ household_income * as.factor(panel_year) |
                household_size + as.factor(marital_status) + as.factor(race) +
                as.factor(hispanic_origin) + market + age + as.factor(college) +
                as.factor(urban) | 0 | market,
              weights = agg[size1_units == "OZ"]$projection_factor)

yearFE <- data.table(est = regFE$coefficients, se = regFE$cse, keep.rownames = TRUE)
setnames(yearFE, c("id", "est", "se"))
yearFE[, "id" := gsub("as\\.factor\\(panel_year\\)", "", id)]
yearFE[, "id" := gsub("household_income", "", id)]
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

chart <- plot_ly(data = yearFE,
                 x = ~year, height = 800, width = 1200) %>%
  add_lines(y = ~`est_<25k`, line = list(width = 5),
            error_y = list(array = ~1.96 * `se_<25k`), name = "<25k") %>%
  add_lines(y = ~`est_25-50k`, line = list(width = 5),
            error_y = list(array = ~1.96 * `se_25-50k`), name = "25-50k") %>%
  add_lines(y = ~`est_50-100k`, line = list(width = 5),
            error_y = list(array = ~1.96 * `se_50-100k`), name = "50-100k") %>%
  add_lines(y = ~`est_>100k`, line = list(width = 5),
            error_y = list(array = ~1.96 * `se_>100k`), name = ">100k") %>%
  layout(title = paste0("Year Fixed Effects, Units = Ounces (2004-2016)"),
         titlefont = list(size = 35),
         xaxis = list(title = "Year", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "Log(Monthly Spending)", range = c(-0.02, 0.04), dtick = 0.02,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.7, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 110, r = 100, t = 60, b = 150, pad = 10),
         annotations = list(text = "Source: Nielsen.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.2))
export(chart, file = "./code/5_figures/yearFEIncomeSizeOZ.png")
