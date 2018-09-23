# This does Menzio's household price index analysis
library(data.table)
library(purrr)
library(Hmisc)
library(plotly)
library(zoo)
yr <- 2004:2016

getInd <- function(yr) {
  print(yr)
  hhIndex <- unique(fread(paste0("/home/mallick/Desktop/Nielsen/Data/Clean/",
                                 "FullData/fullData", yr, ".csv"),
                          select = c("projection_factor", "index", "purchase_date",
                                     "X", "market", "age", "household_income",
                                     "panel_year", "household_code")))
  hhIndex <- hhIndex[panel_year == floor(purchase_date)]
  return(hhIndex)
}
hhIndex <- rbindlist(map(yr, getInd))

# Plotting index over time
graphData <- hhIndex[, .(index = weighted.mean(index, w = projection_factor)),
                     by = .(purchase_date, household_income)]
graphData[, "purchase_date" := as.yearqtr(purchase_date)]
ann <- graphData[purchase_date %in% as.yearqtr(c("2007 Q1", "2016 Q4"))]

chart <- plot_ly(data = graphData[purchase_date >= 2007], x = ~purchase_date,
                 height = 800, width = 1200) %>%
  add_lines(y = ~index, split = ~household_income, line = list(width = 5)) %>%
  add_segments(x = 2007, xend = 2017, y = 1, yend = 1, showlegend = FALSE,
               line = list(color = "black")) %>%
  add_annotations(x = ann[purchase_date == "2016 Q4"]$purchase_date,
                  y = ann[purchase_date == "2016 Q4"]$index,
                  text = paste0(round((ann[purchase_date == "2016 Q4"]$index - 1) * 100, 1), "%"),
                  font = list(size = 20),
                  xref = "x",
                  yref = "y",
                  showarrow = TRUE,
                  ax = 60,
                  ay = 0) %>%
  add_annotations(x = ann[purchase_date == "2007 Q1"]$purchase_date,
                  y = ann[purchase_date == "2007 Q1"]$index,
                  text = paste0(round((ann[purchase_date == "2007 Q1"]$index - 1) * 100, 1), "%"),
                  font = list(size = 20),
                  xref = "x",
                  yref = "y",
                  showarrow = TRUE,
                  ax = 50,
                  ay = 20) %>%
  layout(title = "Household Price Index by Income (2004-2016)",
         titlefont = list(size = 35),
         xaxis = list(title = "Year", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "Price Index (Avg Mkt Price = 1)", range = c(0.9, 1.1), dtick = 0.05,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 100, r = 100, t = 60, b = 150, pad = 10),
         annotations = list(text = "Source: Nielsen.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.2))
export(chart, "./code/5_figures/hhPriceIndex.png")

# Figure 2
# <25k, 25-50k, 50-100k >100k
hhInc <- ">100k"
normMean <- mean(hhIndex[household_income == hhInc]$index)
normSD <- sd(hhIndex[household_income == hhInc]$index)
normDat <- dnorm(x = seq(0, 2, 0.01), mean = normMean, sd = normSD)
hist(hhIndex[household_income == hhInc]$index, freq = FALSE, breaks = 500, xlim = c(0, 2))
lines(x = seq(0, 2, 0.01), y = normDat)

# Table 6
tab6 <- hhIndex[, .(sd = sqrt(wtd.var(index, weights = X)),
                    q90 = wtd.quantile(index, probs = 0.9, weights = X),
                    q50 = wtd.quantile(index, probs = 0.5, weights = X),
                    q10 = wtd.quantile(index, probs = 0.1, weights = X)),
                by = .(market, purchase_date)]
tab6[, ':=' (rat9010 = q90 / q10,
             rat9050 = q90 / q50,
             rat5010 = q50 / q10)]
tab6Final <- tab6[, .(sd = mean(sd),
                      rat9010 = mean(rat9010),
                      rat9050 = mean(rat9050),
                      rat5010 = mean(rat5010))]

# Figure 9
fig9 <- hhIndex[, .(index = weighted.mean(index, w = projection_factor)), by = age]
fig9[, "base" := fig9[age == "<35"]$index]
fig9[, "normInd" := (index - base) / base]
plot_ly(data = fig9, x = ~age) %>%
  add_lines(y = ~normInd)

# Price index over time
graphData <- hhIndex[, .(index = weighted.mean(index, w = projection_factor)),
                     by = .(purchase_date, household_income)]
graphData[, "purchase_date" := as.yearqtr(purchase_date)]
plot_ly(data = graphData, x = ~purchase_date) %>%
  add_lines(y = ~index, split = ~household_income)
