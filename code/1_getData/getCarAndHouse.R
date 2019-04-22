# Getting tract-level car ownership from ACS 5-year estimate (2010-2014)
# Table B08141: MEANS OF TRANSPORTATION TO WORK BY VEHICLES AVAILABLE
library(data.table)
library(acs)
library(stringr)
tracts <- geo.make(state = state.abb, county = "*", tract = "*")
carOwn <- acs.fetch(geography = tracts, endyear = 2014, table.number = "B08141")
carDT <- data.table(carOwn@geography, carOwn@estimate)
carDT <- carDT[, .(NAME, state, county, tract, B08141_001, B08141_002, B08141_003,
                   B08141_004, B08141_005, B08141_016)]
setnames(carDT, c("name", "state", "county", "tract", "total", "Veh0",
                  "Veh1", "Veh2", "Veh3Plus", "PublicTrans"))

# Getting median number of rooms from ACS 5-year estimate (2010-2014)
# Table B25018: MEDIAN NUMBER OF ROOMS
home <- acs.fetch(geography = tracts, endyear = 2014, table.number = "B25018")
homeDT <- data.table(home@geography, home@estimate)
homeDT[B25018_001 < 0, "B25018_001" := NA]
setnames(homeDT, c("name", "state", "county", "tract", "medianRoom"))

# Combining together
fullDT <- merge(carDT, homeDT, by = c("name", "state", "county", "tract"))
fullDT[, "geoid" := as.numeric(paste0(str_pad(state, 2, "left", "0"),
                                      str_pad(county, 3, "left", "0"),
                                      str_pad(tract, 6, "left", "0")))]

# Adding in ZIP code and collapsing to ZIP
zipTract <- fread("https://www2.census.gov/geo/docs/maps-data/data/rel/zcta_tract_rel_10.txt")
setnames(zipTract, tolower(names(zipTract)))
zipTract <- zipTract[, .(zcta5, geoid, trpoppct)]
fullDT <- merge(fullDT, zipTract, by = "geoid")
fullDT <- fullDT[trpoppct > 0, .(total = sum(total),
                                 Veh0 = sum(Veh0),
                                 Veh1 = sum(Veh1),
                                 Veh2 = sum(Veh2),
                                 Veh3Plus = sum(Veh3Plus),
                                 PublicTrans = sum(PublicTrans),
                                 medianRoom = weighted.mean(medianRoom, w = trpoppct)),
                 by = .(zcta5)]

# Collapsing to car access, public transit access, and rooms
fullDT[, ':=' (carShare = (1 - (Veh0 / total)),
               publicTransShare = PublicTrans / total)]
fullDT <- fullDT[, .(zip_code = zcta5, medianRoom, carShare, publicTransShare)]
fwrite(fullDT, "./code/0_data/carAndHome.csv")

# scp /home/mallick/Desktop/Research/OnlineShopping/WarehouseClubs/code/0_data/carAndHome.csv hossaine@wrds-cloud.wharton.upenn.edu:/home/upenn/hossaine/Nielsen/Data
