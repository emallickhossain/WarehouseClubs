# Computes online share of purchases in 2017
library(data.table)
threads <- 8
yr <- 2017
path <- "/scratch/upenn/hossaine/nielsen_extracts/HMS/"

retailers <- fread("/scratch/upenn/hossaine/fullRetailers.csv", nThread = threads)
trips <- fread(paste0(path, yr, "/Annual_Files/trips_", yr, ".tsv"),
               nThread = threads, key = "trip_code_uc")[panel_year == yr]
trips <- merge(trips, retailers, by = "retailer_code")
purch <- fread(paste0("/scratch/upenn/hossaine/fullPurch", yr, ".csv"), nThread = threads)
fullData <- merge(trips, purch, by = "trip_code_uc")
fullData[, "online" := (channel_type == "Online Shopping")]
online <- fullData[, .(totalSpend = sum(packagePrice * quantity)),
                   by = .(household_code, panel_year, online)]
panel <- fread("/scratch/upenn/hossaine/fullPanel.csv",
               select = c("household_code", "panel_year", "projection_factor"))
online <- merge(online, panel, by = c("household_code", "panel_year"))
online[, "share" := totalSpend / sum(totalSpend), by = .(household_code)]

round(quantile(online[online == TRUE]$share, seq(0, 1, 0.1)), 2)
online[, weighted.mean(totalSpend, w = projection_factor), by = online]
