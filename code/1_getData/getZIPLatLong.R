# Gets lat-long for each ZIP code from Census
library(data.table)
download.file("http://www2.census.gov/geo/docs/maps-data/data/gazetteer/2017_Gazetteer/2017_Gaz_zcta_national.zip", "temp.zip")
unzip("temp.zip")
zips <- fread("2017_Gaz_zcta_national.txt", select = c("GEOID", "INTPTLAT", "INTPTLONG"))
setnames(zips, c("zip_code", "lat", "lon"))
fwrite(zips, "./code/0_data/zipLatLon.csv")
system("rm *.zip")
system("rm *.txt")
