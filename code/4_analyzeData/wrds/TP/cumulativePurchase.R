# Computes cumulative purchases over the year
library(data.table)
library(ggplot2)
library(ggthemes)

path <- "/scratch/upenn/hossaine/"
tpPurch <- fread(paste0(path, "7270Purch.csv")) # WRONG PRODUCT!!!!!
tpPurch[, c("upc", "upc_ver_uc", "trip_code_uc", "quantity",
            "product_module_code", "upc_descr", "size1_units") := NULL]
tpPurch[, "days" := ifelse(is.na(IPD), 0, IPD)]
tpPurch[, ':=' (cumulative = cumsum(size),
                cumDays = cumsum(days)), by = .(household_code, panel_year)]
tpPurch[, "share" := cumulative / total * 100]
graphData <- tpPurch[, .(share = weighted.mean(share, w = projection_factor)),
                     by = .(household_income_coarse, cumDays)]

ggplot(data = graphData, aes(x = cumDays, y = share, color = household_income_coarse)) +
  geom_step() +
  theme_fivethirtyeight()
