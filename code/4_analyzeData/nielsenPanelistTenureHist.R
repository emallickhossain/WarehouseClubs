library(data.table)
library(plotly)

full_spending <- fread("/home/mallick/Desktop/Nielsen/Data/full_spending.csv")

# Making histogram of tenure in Nielsen sample
tenure <- full_spending[, uniqueN(panel_year), by = household_code]
chart <- plot_ly(data = tenure, height = 800, width = 1200) %>%
  add_histogram(x = ~V1) %>%
  layout(title = "Panelist Tenure",
         titlefont = list(size = 35),
         xaxis = list(title = "Years in Sample", titlefont = list(size = 30),
                      tickfont = list(size = 25)),
         yaxis = list(title = "Frequency",
                      titlefont = list(size = 30), tickfont = list(size = 25)),
         # Pick positioning of legend so it doesn"t overlap the chart
         legend = list(yanchor = "top", x = 0.85, font = list(size = 20)),
         # Adjust margins so things look nice
         margin = list(l = 100, r = 50, t = 60, b = 150, pad = 10),
         annotations = list(text = "Source: Nielsen.",
                            font = list(size = 20),
                            showarrow = FALSE,
                            align = "left", valign = "bottom",
                            xref = "paper", x = -0.03,
                            yref = "paper", y = -0.2))
export(chart, "./code/5_figures/nielsenTenure.png")
