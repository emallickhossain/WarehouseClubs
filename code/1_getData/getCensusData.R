# Downloads necessary Census data
library(data.table)
library(acs)
library(stringr)
library(fredr)
api.key.install(censusAPI)
fredr_set_key(fredAPI)

###################### GET CBP DATA ############################################
naicsCode <- c(445110, 446110, 452910, 452990)
getCBP <- function(yr) {
  temp <- tempfile()
  download.file(paste0("https://www2.census.gov/programs-surveys/cbp/datasets/",
                       yr, "/zbp", substr(yr, 3, 4), "detail.zip"), destfile = temp)
  unzip(temp)
  if (yr == 2009) {
    cbp <- fread(paste0("./Zbp", substr(yr, 3, 4), "detail.txt"))
  } else {
    cbp <- fread(paste0("./zbp", substr(yr, 3, 4), "detail.txt"))
  }
  cbp <- cbp[naics %in% naicsCode][, "year" := yr]
  system("rm *.txt")
  return(cbp)
}

fullCBP <- getCBP(2016)
setnames(fullCBP, "zip", "zip_code")
fwrite(fullCBP, "./code/0_data/cbp2016.csv")

################### GET CPI DATA ###############################################
fredr_set_key(fredAPI)
cpi <- setDT(fredr("CPIAUCSL", observation_start = as.Date("2004-01-01"),
                   observation_end = as.Date("2017-12-31")))[, "series_id" := NULL]
fwrite(cpi, "./code/0_data/cpi.csv")
# scp /home/mallick/Desktop/Research/OnlineShopping/WarehouseClubs/code/0_data/cpi.csv
# hossaine@wrds-cloud.wharton.upenn.edu:/home/upenn/hossaine/Nielsen/Data/cpi.csv

################### GET ZILLOW DATA ############################################
# $/sqft is units
sqft <- fread("http://files.zillowstatic.com/research/public/Zip/Zip_MedianValuePerSqft_AllHomes.csv")
sqftLong <- melt(sqft, id.vars = c("RegionID", "RegionName", "City", "State",
                                   "Metro", "CountyName", "SizeRank"),
                 variable.name = "YM", value.name = "Value")
sqftLong[, "year" := as.integer(substr(YM, 1, 4))]
sqftLong <- sqftLong[year %in% 2004:2017, .(sqftValue = mean(Value)),
                     by = .(year, RegionID, RegionName, City, State, Metro, CountyName)]
sqftFinal <- sqftLong[, .(year, zip_code = RegionName, sqftValue)]
fwrite(sqftFinal, "./code/0_data/sqftValue.csv")
#scp /home/mallick/Desktop/Research/OnlineShopping/WarehouseClubs/code/0_data/sqftValue.csv
# hossaine@wrds-cloud.wharton.upenn.edu:/home/upenn/hossaine/Nielsen/Data

################### GET ZIP LAT/LON DATA #######################################
temp <- tempfile()
download.file(paste0("http://www2.census.gov/geo/docs/maps-data/data/gazetteer/",
                     "2017_Gazetteer/2017_Gaz_zcta_national.zip"), destfile = temp)
unzip(temp)
zips <- fread("2017_Gaz_zcta_national.txt", select = c("GEOID", "INTPTLAT", "INTPTLONG"))
setnames(zips, c("zip_code", "lat", "lon"))
fwrite(zips, "./code/0_data/zipLatLon.csv")
system("rm *.txt")

#################### GET CAR OWNERSHIP #########################################
tracts <- geo.make(state = state.abb, county = "*", tract = "*")
carOwn <- acs.fetch(geography = tracts, endyear = 2013, table.number = "B08141")
carDT <- data.table(carOwn@geography, carOwn@estimate)
carDT <- carDT[, .(NAME, state, county, tract, B08141_001, B08141_002, B08141_016)]
setnames(carDT, c("name", "state", "county", "tract", "total", "Veh0", "PublicTrans"))
carDT[, "geoid" := as.numeric(paste0(str_pad(state, 2, "left", "0"),
                                     str_pad(county, 3, "left", "0"),
                                     str_pad(tract, 6, "left", "0")))]

# Adding in ZIP code and collapsing to ZIP
zipTract <- fread(paste0("https://www2.census.gov/geo/docs/maps-data/data/rel/",
                         "zcta_tract_rel_10.txt"),
                  select = c("ZCTA5", "GEOID", "TRPOPPCT"))
setnames(zipTract, tolower(names(zipTract)))
fullDT <- merge(carDT, zipTract, by = "geoid")
fullDT[, c("geoid", "name", "state", "county", "tract") := NULL]
fullDT <- fullDT[trpoppct > 0, lapply(.SD, sum), by = zcta5][, "trpoppct" := NULL]

# Collapsing to car access and public transit access
fullDT[, ':=' (carShare = (1 - (Veh0 / total)),
               publicTransShare = PublicTrans / total)]
fullDT <- fullDT[, .(zip_code = zcta5, carShare, publicTransShare)]
fwrite(fullDT, "./code/0_data/car.csv")

# scp /home/mallick/Desktop/Research/OnlineShopping/WarehouseClubs/code/0_data/car.csv
# hossaine@wrds-cloud.wharton.upenn.edu:/home/upenn/hossaine/Nielsen/Data
