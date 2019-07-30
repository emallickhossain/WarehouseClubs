# Computes distance to stores
library(data.table)
library(geosphere)
library(stargazer)
library(lfe)
yrs <- 2004:2017
threads <- 8
path <- "/scratch/upenn/hossaine/"

# Getting trips, panel, and lat/lon data
trips <- fread(paste0(path, "fullTrips.csv"), nThread = threads,
               select = c("household_code", "panel_year", "trip_code_uc", "zipImpute"))
panel <- fread(paste0(path, "fullPanel.csv"), nThread = threads,
               select = c("household_code", "panel_year", "household_income",
                          "household_income_coarse", "lat", "lon", "college",
                          "projection_factor", "household_size", "age", "white",
                          "dma_cd"))
setnames(panel, c("lat", "lon"), c("hhLat", "hhLon"))
zipLatLon <- fread(paste0(path, "zipLatLon.csv"))

# Getting all purchases and collapsing to the trip level
fullPurch <- NULL
for (yr in yrs) {
  print(yr)
  purch <- fread(paste0(path, "fullPurch", yr, ".csv"), nThread = threads,
                 select = c("trip_code_uc", "packagePrice", "quantity", "storable"))
  purch <- purch[, .(totalSpend = sum(packagePrice * quantity)),
                 by = .(trip_code_uc, storable)]
  fullPurch <- rbindlist(list(fullPurch, purch), use.names = TRUE)
}

# Combining with panel and trips to get store location
fullPurch <- merge(fullPurch, trips, by = "trip_code_uc")
fullPurch <- merge(fullPurch, zipLatLon, by.x = "zipImpute", by.y = "zip_code")
setnames(fullPurch, c("lat", "lon"), c("storeLat", "storeLon"))
fullData <- merge(fullPurch, panel, by = c("household_code", "panel_year"))

# Computing distance
hhCoord <- cbind(fullData$hhLon, fullData$hhLat)
storeCoord <- cbind(fullData$storeLon, fullData$storeLat)
distance <- distGeo(hhCoord, storeCoord)
fullData[, "dist" := distance]

# Computing expenditure-weighted distance for each household
allPurch <- fullData[, .(totalSpend = sum(totalSpend)),
                     by = .(household_code, panel_year, trip_code_uc,
                            household_income, household_income_coarse, dist,
                            projection_factor, college, household_size,
                            age, white, dma_cd)]
avgHHDist <- allPurch[, .(dist = weighted.mean(dist, w = totalSpend)),
                      by = .(household_code, panel_year, household_income,
                             household_income_coarse, projection_factor,
                             college, household_size, age, white, dma_cd)]
reg <- felm(dist ~ as.factor(household_income) + college + household_size +
              age + white | dma_cd + panel_year,
            weights = avgHHDist$projection_factor, data = avgHHDist)
summary(reg)

# Computing expenditure-weighted distance for storable items
avgHHDistStorable <- fullData[, .(dist = weighted.mean(dist, w = totalSpend)),
                              by = .(household_code, panel_year, household_income,
                                     household_income_coarse, projection_factor,
                                     college, household_size, age, white,
                                     storable, dma_cd)]
reg1 <- felm(dist ~ household_income_coarse + college + household_size +
               age + white | dma_cd + panel_year,
             weights = avgHHDistStorable[storable == 0]$projection_factor,
             data = avgHHDistStorable[storable == 0])
reg2 <- felm(dist ~ household_income_coarse + college + household_size +
               age + white | dma_cd + panel_year,
             weights = avgHHDistStorable[storable == 1]$projection_factor,
             data = avgHHDistStorable[storable == 1])
stargazer(reg1, reg2, type = "text")
