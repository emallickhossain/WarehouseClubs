# Plots willingness to pay for various things
library(data.table)
library(ggplot2)
library(ggthemes)

plotWTP <- function(unitCost, otherParam) {
  x <- seq(1, 100, by = 1)
  num <- sum(otherParam * c(1, 0, 4, 0.25, 0, 0)) + otherParam[2] * x
  denom <- sum(unitCost * c(1, 0, 4, 0.25, 0, 0)) + unitCost[2] * x
  y <- num / denom
  graphData <- data.table(x = x, y = y)
  ggplot(data = graphData, aes(x = x, y = y)) +
    geom_line() +
    theme_fivethirtyeight() +
    scale_y_continuous(limits = c(0, 0.5))
}

plotWTP(c(3.04, -0.01, 0, 0, -0.54, -1.38), c(1.88, -0.01, -0.1, 0, 0, 1.13))
plotWTP(c(3.04, -0.01, 0, 0, -0.54, -1.38), c(0.97, 0, -0.08, 0.21, -0.23, 0.46))


x <- seq(1, 100, by = 1)
unitCost <- c(3.04, -0.01, 0, 0, -0.54, -1.38)
smallParam <- c(1.88, -0.01, -0.1, 0, 0, 1.13)
medParam <- c(0.97, 0, -0.08, 0.21, -0.23, 0.46)
numSmall <- sum(smallParam * c(1, 0, 4, 0.25, 0, 0)) + smallParam[2] * x
denomSmall <- sum(unitCost * c(1, 0, 4, 0.25, 0, 0)) + unitCost[2] * x
ySmall <- numSmall / denomSmall

numMed <- sum(medParam * c(1, 0, 4, 0.25, 0, 0)) + medParam[2] * x
denomMed <- sum(unitCost * c(1, 0, 4, 0.25, 0, 0)) + unitCost[2] * x
yMed <- numMed / denomMed
graphData <- data.table(x = x, ySmall = ySmall, yMed = yMed)
ggplot(data = graphData) +
  geom_line(aes(x = x, y = ySmall)) +
  geom_line(aes(x = x, y = yMed)) +
  geom_text(aes(x = 25, y = 0.5, label = "Small")) +
  geom_text(aes(x = 25, y = 0.2, label = "Med")) +
  theme_fivethirtyeight() +
  scale_y_continuous(limits = c(0, 0.5)) +
  labs(title = "Willingness To Pay For Package Sizes by Income",
       x = "Household Income ($1000s)",
       y = "Willingness to Pay ($ per roll)") +
  theme(axis.title = element_text())
ggsave(filename = "./code/5_figures/bostonWTP.png")
