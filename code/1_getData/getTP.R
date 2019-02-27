# Gets and cleans toilet paper data.
source("./Nielsen/getItem.R")
library(knitr)
library(stargazer)

# Inputs
prodCode <- 7260

# Restrictions
max_price <- 50 # maximum price paid on a single trip


# For Toilet paper, creating standardized roll
prod <- fread(paste0(path, "prod.csv"),
              select = c("upc", "upc_ver_uc", "upc_descr", "product_module_code",
                         "brand_code_uc", "brand_descr", "multi", "size1_amount",
                         "size1_units"))[product_module_code %in% prodCode]
prod[, "sizeUnadj" := multi * size1_amount]
prod[, "ply" := str_extract_all(upc_descr, "\\s\\dP\\s")]
prod[, "ply" := as.integer(gsub("P", "", ply))]
prod[, "sheet" := str_extract_all(upc_descr, "\\d{2,}S\\s")]
prod[, "sheet" := as.integer(gsub("S", "", sheet))]
prod[, "stdRolls" := ply * sheet / 550]
prod[, "size" := multi * size1_amount * stdRolls]
prod[, c("sheet", "stdRolls", "product_module_code", "multi", "size1_amount",
         "size1_units") := NULL]

# Removing "TO GO" packages
prod <- prod[!grep("TO GO", brand_descr)]

# Getting purchases
fullData <- getItem(prod)
fullData[, "totVol" := size * quantity]
fullData[, "purchase_date" := as.Date(purchase_date, format = "%Y-%m-%d")]
# keepForLater <- fullData
# fullData <- keepForLater[, .(household_code, upc, upc_ver_uc, trip_code_uc,
#                              quantity, total_price_paid_real, sizeUnadj,
#                              purchase_date, size, totVol)]

################################################################################
##################### COMPUTING ACTIVE SPELLS AND CONSUMPTION RATE #############
################################################################################
# Computing interpurchase duration times
setorder(fullData, household_code, purchase_date)
fullData[, "prev_date" := shift(purchase_date, 1, type = "lag"), keyby = household_code]
fullData[, "IPD" := purchase_date - prev_date]

# Computing average and SD of IPD for each toilet paper size.
# I compute this using only occasions where a single item was purchased
packSum <- fullData[, .(packages = sum(quantity)), by = .(household_code, purchase_date)]
singlePurch <- merge(fullData, packSum[packages == 1, .(household_code, purchase_date)],
                     by = c("household_code", "purchase_date"))
setorder(singlePurch, household_code, purchase_date)
singlePurch[, "prevSize" := shift(sizeUnadj, 1, "lag"), by = household_code]
singlePurch <- na.omit(singlePurch, cols = "prevSize")
IPDStats <- singlePurch[, .(mean = mean(IPD), sd = sd(IPD)), by = prevSize]

# Combine average inter-purchase duration of package sizes with the full data
# given their previous purchase
setorder(fullData, household_code, purchase_date)
fullData[, "prevSize" := shift(sizeUnadj, 1, "lag"), by = household_code]
fullData <- merge(fullData, IPDStats, by = "prevSize", all.x = TRUE)

# See if there are really long IPD by adding across all sizes purchased that day
# and comparing against the summed means plus 2 SDs. If IPD is larger, then
# I deem that a purchase is likely missing or the household was inactive for
# a certain period and will generate a new active period for them after. I assign
# each active period an identifier and increment it by 1 if there is a long IPD.
activePeriods <- fullData[, .(IPD = sum(IPD),
                              mean = sum(mean),
                              sd = sum(sd)),
                          keyby = .(household_code, purchase_date)]
activePeriods[, c("upper", "sd", "mean") := .(mean + 2 * sd, NULL, NULL)]
activePeriods[, "missingFlag" := ifelse(IPD > upper, 1L, 0L)]
activePeriods[is.na(missingFlag), "missingFlag" := 0L]
activePeriods[, "activePeriod" := cumsum(missingFlag), by = household_code]
fullData <- merge(fullData, activePeriods[, .(household_code, purchase_date, activePeriod)],
                  by = c("household_code", "purchase_date"), all.x = TRUE)
fullData[, c("prev_date", "prevSize", "mean", "sd") := NULL]

# Adding in final purchase of each active period because we will remove it
# from the consumption calculation since they don't consume their last purchase
# during the active period.
setorder(fullData, household_code, purchase_date)
fullData[, "finalPurch" := tail(size, 1), by = .(household_code, activePeriod)]
fullData[is.na(finalPurch), "finalPurch" := 0]

# Computing the consumption rate for each household-year. This is the total volume
# purchased across all active spells, excluding the last purchase of each spell
# (since that isn't consumed in the active period) divided by the duration of all
# active spells. Units are std rolls per day.
consRate <- fullData[, .(finalPurch = mean(finalPurch),
                         totalPurch = sum(size, na.rm = TRUE),
                         start = min(purchase_date),
                         end = max(purchase_date)),
                     by = .(household_code, activePeriod)]
consRate[, "volume" := totalPurch - finalPurch]
consRate[, "duration" := end - start]
consRate <- consRate[, .(volume = sum(volume),
                         duration = sum(duration)),
                     by = .(household_code)]
consRate[, "rate" := volume / as.numeric(duration)]
consRate <- consRate[, .(household_code, rate, duration)]

fullData <- merge(fullData, consRate, by = "household_code")

################################################################################
###################### CLEANING ################################################
################################################################################
# Starting with 4,241,992 purchases across 149,272 households
totalCount <- nrow(fullData)
totalHH <- uniqueN(fullData$household_code)
totRow <- c("Total HH/Observations:", totalCount, "-", totalHH, "-")

# For consideration
fullData[, ':=' (missing = uniqueN(activePeriod) - 1,
                 maxIPD = max(IPD, na.rm = TRUE),
                 maxQuant = max(quantity, na.rm = TRUE),
                 maxVol = max(totVol, na.rm = TRUE),
                 active = mean(duration)),
         by = .(household_code)]

# Excessive missingness (households with > 3 active periods <=>
# more than 3 missing purchases)
missID <- fullData[missing > 3]$ household_code
fullData[, "excessMissing" := ifelse(household_code %in% missID, 1L, 0L)]
excessMissing <- fullData[excessMissing == 1]
miss1 <- uniqueN(excessMissing$trip_code_uc)
miss2 <- round(miss1 / totalCount * 100, 1)
miss3 <- uniqueN(excessMissing$household_code)
miss4 <- round(miss3 / totalHH * 100, 1)
missRow <- c("Missing 3+ IPD:", miss1, miss2, miss3, miss4)

# Households for which interpurchase duration is higher than 99th percentile (392 days)
# Some of these households households number are households that are present
# one year and drop for a year and come back.
ipd99 <- unique(fullData[, .(household_code, maxIPD)])
ipd99 <- quantile(ipd99$maxIPD, 0.99, na.rm = TRUE)
fullData[, "ipd99" := ifelse(maxIPD > ipd99, 1L, 0L)]
longIPD <- fullData[ipd99 == 1]
long1 <- uniqueN(longIPD$trip_code_uc)
long2 <- round(long1 / totalCount * 100, 1)
long3 <- uniqueN(longIPD$household_code)
long4 <- round(long3 / totalHH * 100, 1)
longRow <- c("Max IPD > 99th Pct:", long1, long2, long3, long4)

# Cannot calculate consumption
fullData[, "nocons" := ifelse(is.na(rate), 1L, 0L)]
noCons <- fullData[nocons == 1]
nocon1 <- uniqueN(noCons$trip_code_uc)
nocon2 <- round(nocon1 / totalCount * 100, 1)
nocon3 <- uniqueN(noCons$household_code)
nocon4 <- round(nocon3 / totalHH * 100, 1)
noconRow <- c("Cannot calc. consumption:", nocon1, nocon2, nocon3, nocon4)

# Insufficient consumption
cons01 <- unique(fullData[, .(household_code, rate)])
cons01 <- quantile(cons01$rate, 0.01, na.rm = TRUE)
fullData[, "inCons" := ifelse(rate < cons01, 1L, 0L)]
inCons <- fullData[inCons == 1]
incons1 <- uniqueN(inCons$trip_code_uc)
incons2 <- round(incons1 / totalCount, 1)
incons3 <- uniqueN(inCons$household_code)
incons4 <- round(incons3 / totalHH, 1)
inconsRow <- c("Insufficient Consumption:", incons1, incons2, incons3, incons4)

# Active for less than 90 days
fullData[, "noActive" := ifelse(duration < 90, 1L, 0L)]
noActive <- fullData[noActive == 1]
noact1 <- uniqueN(noActive$trip_code_uc)
noact2 <- round(noact1 / totalCount * 100, 1)
noact3 <- uniqueN(noActive$household_code)
noact4 <- round(noact3 / totalHH * 100, 1)
noactRow <- c("Active < 90 days:", noact1, noact2, noact3, noact4)

# Excessive quantity purchase
quant99 <- unique(fullData[, .(household_code, maxQuant)])
quant99 <- quantile(quant99$maxQuant, 0.99, na.rm = TRUE)
fullData[, "abQuant" := ifelse(quantity > quant99, 1L, 0L)]
abQuant <- fullData[abQuant == 1]
abquan1 <- uniqueN(abQuant$trip_code_uc)
abquan2 <- round(abquan1 / totalCount * 100, 1)
abquan3 <- uniqueN(abQuant$household_code)
abquan4 <- round(abquan3 / totalHH * 100, 1)
abquanRow <- c("Abnormal Quantity:", abquan1, abquan2, abquan3, abquan4)

# Abnormal volume purchased
vol99 <- unique(fullData[, .(household_code, maxVol)])
vol99 <- quantile(vol99$maxVol, 0.99, na.rm = TRUE)
fullData[, "abVol" := ifelse(totVol > vol99, 1L, 0L)]
abVol <- fullData[abVol == 1]
abvol1 <- uniqueN(abVol$trip_code_uc)
abvol2 <- round(abvol1 / totalCount * 100, 1)
abvol3 <- uniqueN(abVol$household_code)
abvol4 <- round(abvol3 / totalHH * 100, 1)
abvolRow <- c("Abnormal Volume:", abvol1, abvol2, abvol3, abvol4)

# Abnormal price
fullData[, "abPrice" := ifelse(total_price_paid_real > max_price, 1L, 0L)]
fullData[total_price_paid_real == 0, "abPrice" := 1L]
abPrice <- fullData[abPrice == 1]
abprice1 <- uniqueN(abPrice$trip_code_uc)
abprice2 <- round(abprice1 / totalCount * 100, 1)
abprice3 <- uniqueN(abPrice$household_code)
abprice4 <- round(abprice3 / totalHH * 100, 1)
abpriceRow <- c("Abnormal Price:", abprice1, abprice2, abprice3, abprice4)

# Getting final tally
fullData[, "drop" := 0]
fullData[excessMissing == 1, "drop" := 1]
fullData[ipd99 == 1, "drop" := 1]
fullData[nocons == 1, "drop" := 1]
fullData[inCons == 1, "drop" := 1]
fullData[noActive == 1, "drop" := 1]
fullData[abQuant == 1, "drop" := 1]
fullData[abVol == 1, "drop" := 1]
fullData[abPrice == 1, "drop" := 1]
dropHH <- fullData[drop == 1]$household_code
fullData[household_code %in% dropHH, "drop" := 1]
totDrop <- fullData[drop == 1]
totDrop1 <- uniqueN(totDrop$trip_code_uc)
totDrop2 <- round(totDrop1 / totalCount * 100, 1)
totDrop3 <- uniqueN(totDrop$household_code)
totDrop4 <- round(totDrop3 / totalHH * 100, 1)
totDropRow <- c("Total Dropped:", totDrop1, totDrop2, totDrop3, totDrop4)

# Final table tally
cleanTable <- as.data.table(rbind(missRow, longRow, noconRow, inconsRow, noactRow,
                                  abquanRow, abvolRow, abpriceRow, totRow, totDropRow))
setnames(cleanTable, c("Criteria", "Obs", "Obs %", "HH", "HH %"))
cleanTable[, c("Obs", "HH") := lapply(.SD, as.integer), .SDcols = c("Obs", "HH")]
kable(cleanTable, type = "markdown", format.args = list(big.mark = ","))

# Comparing clean and raw data
qtile <- c(0.01, 0.25, 0.5, 0.75, 0.99)
consData <- unique(fullData[, .(household_code, rate, drop)])
consRaw <- c(quantile(consData$rate, qtile, na.rm = TRUE),
             uniqueN(consData$household_code))
consClean <- c(quantile(consData[drop == 0]$rate, qtile),
               uniqueN(consData[drop == 0]$household_code))
IPDRaw <- c(as.integer(quantile(fullData$IPD, qtile, na.rm = TRUE)),
            nrow(na.omit(fullData, cols = "IPD")))
IPDClean <- c(as.integer(quantile(fullData[drop == 0]$IPD, qtile, na.rm = TRUE)),
              nrow(na.omit(fullData[drop == 0], cols = "IPD")))
volRaw <- c(quantile(fullData$totVol, qtile, na.rm = TRUE),
            nrow(na.omit(fullData, cols = "totVol")))
volClean <- c(quantile(fullData[drop == 0]$totVol, qtile, na.rm = TRUE),
              nrow(na.omit(fullData[drop == 0], cols = "totVol")))
ptile <- c("1st pct", "25th pct", "50th pct", "75th pct", "99th pct", "N")

compTable <- data.table(ptile, consRaw, consClean, IPDRaw, IPDClean, volRaw, volClean)
kable(compTable, digits = 2, type = "markdown", format.args = list(big.mark = ","))

# Summary table of data
stargazer(fullData[, .(quantity, total_price_paid, sizeUnadj, ply, size, rate)],
          type = "text")
stargazer(fullData[drop == 0, .(quantity, total_price_paid, sizeUnadj,
                                ply, size, rate)], type = "text")

fullData[, c("finalPurch", "maxIPD", "active", "excessMissing", "ipd99", "nocons",
             "inCons", "noActive", "abQuant", "abVol", "abPrice", "duration",
             "activePeriod", "missing") := NULL]

fwrite(fullData, paste0("/home/upenn/hossaine/Nielsen/Data/", prodCode, "Purch.csv"),
       nThread = threads)
