# Price and size scatter plot colored by brand. Using the choice set data for logit estimation.
library(data.table)
library(ggplot2)
threads <- 8
tpPurch <- fread("/scratch/upenn/hossaine/TPMLogit.csv", nThread = threads)

ggplot(data = tpPurch, aes(x = pkgSize, y = unitCost)) +
  geom_jitter(aes(color = brandBin), width = 0.25)

ggplot(data = tpPurch[choice == 1], aes(x = pkgSize, y = unitCost)) +
  geom_jitter(aes(color = brandBin), width = 0.25)
