# Plots willingness to pay for various things
library(data.table)
library(ggplot2)
library(ggthemes)

plotWTP <- function(priceCoefs, smallCoefs, city) {
  x <- seq(1, 100, by = 1)
  numSmall <- sum(smallCoefs * c(1, 0, 0.25, 0, 0, 0)) + smallCoefs[2] * x
  denom <- sum(priceCoefs * c(1, 0, 0.25, 0, 0, 0)) + priceCoefs[2] * x
  ySmall <- -numSmall / denom

  graphData <- data.table(x = x, ySmall = ySmall)
  ggplot(data = graphData, aes(x = x, y = ySmall)) +
    geom_line() +
    labs(title = paste0("Willingness to Pay for Space Decreases in Income (", city, ")"),
         x = "Household Income ($1000)",
         y = "Willingness to Pay ($)",
         caption = paste0("Source: Author calulations using Nielsen Consumer Panel.")) +
    theme_fivethirtyeight() +
    theme(axis.title = element_text()) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0)
  ggsave(filename = paste0("./code/5_figures/wtp", city, ".png"))
}

plotWTP(c(-0.36, 0.002,  -0.08, 0.12, 0,     0.05), c(1.65, -0.01,  -0.39, 0.29, 0.84, -0.18), "Boston")
plotWTP(c(-0.42, 0.002,  0,     0.09, -0.10, 0.03), c(1.41, 0,      -0.63, 0,    0.53, 0), "Charlotte")
plotWTP(c(-0.23, 0.001,  -0.1,  0,    -0.13, 0),    c(0.57, -0.005, 0.38,  0.48, 0,    0), "Chicago")
plotWTP(c(-0.34, 0.001,  0,     0.13, 0.10,  0),    c(1.62, -0.01,  0.56,  0,    0,    0), "Columbus")
plotWTP(c(-0.24, 0.001,  0.13,  0.14, 0,     0),    c(1.92, -0.01,  0.86,  0.31, 0,    0), "Dallas")
plotWTP(c(-0.35, 0.001,  0.31,  0,    0,     0),    c(0.95, 0,      0.68,  0.54, 0.6,  0), "Denver")
plotWTP(c(-0.21, 0.0004, 0.1,   0,    0.14,  0),    c(0.90, -0.01,  0.76,  0.14, 0,    0), "LA")
plotWTP(c(-0.21, 0,      0.16,  0,    0,     0.03), c(0.92, -0.01,  0,     0,    0.48, 0), "Minneapolis")
plotWTP(c(-0.22, 0.001,  -0.08, 0.07, -0.10, 0),    c(1.85, -0.01,  -1.14, 0,    0,    0), "Philly")
plotWTP(c(-0.26, 0,      0.09,  0.,   -0.07, 0),    c(1.63, -0.01,  0,     0.45, 0.57, 0), "Phoenix")
