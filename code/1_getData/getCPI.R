# Gets inflation
library(data.table)
library(fredr)
fredr_set_key(fredAPI)
cpi <- setDT(fredr("CPIAUCSL", observation_start = as.Date("2004-01-01"),
                   observation_end = as.Date("2017-12-31")))
cpi[, c("series_id") := NULL]
fwrite(cpi, "./code/0_data/cpi.csv")

# scp /home/mallick/Desktop/Research/OnlineShopping/WarehouseClubs/code/0_data/cpi.csv hossaine@wrds-cloud.wharton.upenn.edu:/home/upenn/hossaine/Nielsen/Data/cpi.csv
