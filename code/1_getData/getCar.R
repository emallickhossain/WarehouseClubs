# Gets car ownership by FIPS and year
# Pulled from IPUMS ACS which I had manually downloaded
# Vars: Year, datanum, serial, hhwt, statefip, puma, vehicles
library(data.table)
library(stringr)
acs <- unique(fread("./code/0_data/usa_00004.csv"))
acs[, "car" := ifelse(VEHICLES >= 1, 1L, 0L)]
own <- na.omit(acs[, .(ownership = weighted.mean(car, w = HHWT)), by = .(YEAR, STATEFIP, PUMA)])
setnames(own, c("panel_year", "state", "puma12", "ownership"))
fwrite(own, "./code/0_data/carOwnership.csv")
