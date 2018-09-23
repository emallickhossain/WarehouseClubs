library(data.table)
library(plotly)
library(fredr)
library(purrr)
fredr_set_key(fredAPI)

series <- c("MRTSSM4400AUSS", "MRTSSM4541USS", "MRTSSM45291USS", "MRTSSM45299USS", "USREC")
graphData <- rbindlist(map(series, fredr, observation_start = as.Date("1999-01-01")),
                       use.names = TRUE, fill = TRUE)
graphDataWide <- dcast(data = graphData, date ~ series_id, value.var = "value")
setnames(graphDataWide, c("date", "retExAuto", "Club", "Other Gen Merch", "Online", "rec"))

chart <- plot_ly(data = graphDataWide, x = ~date, height = 800, width = 1200) %>%
  add_lines(y = ~Club / retExAuto * 100, name = "Clubs and Supercenters",
            line = list(width = 5)) %>%
  add_lines(y = ~`Other Gen Merch` / retExAuto * 100, name = "Other General Merchandise",
            line = list(width = 5)) %>%
  add_lines(y = ~Online / retExAuto * 100, name = "Online and Mail-Order",
            line = list(width = 5)) %>%
  add_lines(y = ~rec*1e5,
            line = list(width = 0),
            fill = "tozeroy",
            fillcolor = "rgba(64, 64, 64, 0.2)",
            showlegend = F,
            hoverinfo = "none") %>%
  layout(title = "Retail Spending Shares by Subindustry",
         titlefont = list(size = 35),
         xaxis = list(title = "Year", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "Percent of Retail Spending", range = c(0, 15), dtick = 5,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.05, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 80, r = 50, t = 60, b = 150, pad = 10),
         annotations = list(text = "Source: Annual Retail Trade Survey. <br> Note: Retail spending excludes auto sales.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.2))
export(chart, "./code/5_figures/ARTSRetailChart.png")


# Index chart
base <- graphDataWide[date == "1999-01-01"]
graphDataWideIndex <- graphDataWide[, .(date = date,
                                        Club = (Club / retExAuto) / (base$Club / base$retExAuto) * 100,
                                        Online = (Online / retExAuto) / (base$Online / base$retExAuto) * 100,
                                        Other = (`Other Gen Merch` / retExAuto) / (base$`Other Gen Merch` / base$retExAuto) * 100,
                                        rec = rec)]
chart <- plot_ly(data = graphDataWideIndex, x = ~date, height = 800, width = 1200) %>%
  add_lines(y = ~Club, name = "Clubs and Supercenters",
            line = list(width = 5)) %>%
  add_lines(y = ~Other, name = "Other General Merchandise",
            line = list(width = 5)) %>%
  add_lines(y = ~Online, name = "Online and Mail-Order",
            line = list(width = 5)) %>%
  add_lines(y = ~rec*1e5,
            line = list(width = 0),
            fill = "tozeroy",
            fillcolor = "rgba(64, 64, 64, 0.2)",
            showlegend = F,
            hoverinfo = "none") %>%
  layout(title = "Retail Spending Shares by Subindustry",
         titlefont = list(size = 35),
         xaxis = list(title = "Year", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "Index of Share of Retail Sales (Jan-1999 = 100)", range = c(0, 350), dtick = 50,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.05, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 100, r = 50, t = 60, b = 150, pad = 10),
         annotations = list(text = "Source: Annual Retail Trade Survey. <br> Note: Retail spending excludes auto sales.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.2))
export(chart, "./code/5_figures/ARTSRetailChartIndex.png")
