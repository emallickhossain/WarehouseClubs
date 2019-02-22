# Gets inflation
library(data.table)
library(fredr)
fredr_set_key(fredAPI)
pce <- setDT(fredr("PCEPI", observation_start = as.Date("2004-01-01")))
pce[, c("panel_year", "month") := .(year(date), month(date))]
pce[, c("date", "series_id") := NULL]
fwrite(pce, "./code/0_data/pce.csv")

#system("scp /home/mallick/Desktop/Research/OnlineShopping/WarehouseClubs/code/0_data/pce.csv hossaine@wrds-cloud.wharton.upenn.edu:/home/upenn/hossaine/Nielsen/pce.csv")
