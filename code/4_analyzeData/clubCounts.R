library(data.table)
library(plotly)
clubs <- fread("./code/0_data/clubs.csv")
chart <- plot_ly(data = clubs, x = ~year, height = 800, width = 1200) %>%
  add_lines(y = ~costco, name = "Costco", line = list(width = 5)) %>%
  add_lines(y = ~sams, name = "Sam's Club", line = list(width = 5)) %>%
  add_trace(y = ~bjs, name = "BJ's", type = "scatter", line = list(width = 5)) %>%
  layout(title = paste0("Store Counts by Warehouse Club (2004-2018)"),
         titlefont = list(size = 35),
         xaxis = list(title = "Year", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "Stores", range = c(0, 700), dtick = 100,
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.1, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 100, r = 100, t = 60, b = 150, pad = 10),
         annotations = list(text = "Source: Company Reports.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.2))
export(chart, "./code/5_figures/clubCount.png")
