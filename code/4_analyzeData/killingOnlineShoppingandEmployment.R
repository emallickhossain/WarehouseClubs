library(data.table)
library(plotly)

load("/home/mallick/Desktop/comScore/TransactionsClean.rda")
load("/home/mallick/Desktop/comScore/DemographicsClean.rda")

# Average per person spending in the county
fullData <- merge(transactions, demographics, by = c("machine_id", "year"))
rm(transactions, demographics)
countyTrans <- fullData[, .(spending = sum(prod_totprice_real),
                            people = uniqueN(machine_id)), by = .(year, fips)]
countyTrans[, "perPerson" := spending / people]

# Getting CBP
cbp <- fread("/home/mallick/Desktop/Census/cbp98_16.csv")

# Adding in estimated employment following Basker 2005
wgt <- 2 / 3
cbp[empflag == "A", "empflagNum" := wgt * 1 + (1 - wgt) * 19]
cbp[empflag == "B", "empflagNum" := wgt * 20 + (1 - wgt) * 99]
cbp[empflag == "C", "empflagNum" := wgt * 100 + (1 - wgt) * 249]
cbp[empflag == "E", "empflagNum" := wgt * 250 + (1 - wgt) * 499]
cbp[empflag == "F", "empflagNum" := wgt * 500 + (1 - wgt) * 999]
cbp[empflag == "G", "empflagNum" := wgt * 1000 + (1 - wgt) * 2499]
cbp[empflag == "H", "empflagNum" := wgt * 2500 + (1 - wgt) * 4999]
cbp[empflag == "I", "empflagNum" := wgt * 5000 + (1 - wgt) * 9999]
cbp[empflag == "J", "empflagNum" := wgt * 10000 + (1 - wgt) * 24999]
cbp[empflag == "K", "empflagNum" := wgt * 25000 + (1 - wgt) * 49999]
cbp[empflag == "L", "empflagNum" := wgt * 50000 + (1 - wgt) * 99999]
cbp[empflag == "M", "empflagNum" := 100000]
cbp[is.na(empflagNum), "empflagNum" := as.numeric(emp)]

# Retail employment and establishments by county and year
retailNAIC <- paste0(441:454, "///")
retail <- cbp[naics %in% retailNAIC]

fullData <- merge(retail, countyTrans, by = c("year", "fips"))

countyStats <- fread("./code/0_data/countyStats.csv")
fullData <- merge(fullData, countyStats, by = c("fips", "year"))
fullData <- fullData[year >= 2006]
setorder(fullData, fips, naics, year)
fullData[, "lagEst" := shift(est, 1, "lag"), by = c("naics", "fips")]

# Running regressions
for (i in 441:454) {
  tryCatch(
    {
      message("NAICS", i)
      print(summary(lm(data = fullData[naics == paste0(i, "///")],
                       est ~ perPerson + lagEst)))
      },
    error = function(cond) message("NAICS doesn't exist", cond))
}

# Warehouse vs other gen merch stores
warehouse <- cbp[naics == "4543//" & year > 2006]
wareAnn <- warehouse[, .(est = sum(est),
                         emp = sum(empflagNum)), by = year]
wareAnn[, "empGrowth" := emp - shift(emp, 1, "lag")]
plot_ly(data = wareAnn, x = ~year) %>%
  add_lines(y = ~est, name = "Establishments") %>%
  add_lines(y = ~emp, name = "Employment")

dollarGen <- cbp[naics == "452990" & year > 2006]
dollarAnn <- dollarGen[, .(est = sum(est),
                           emp = sum(empflagNum)), by = year]
dollarAnn[, "empGrowth" := emp - shift(emp, 1, "lag")]
plot_ly(data = dollarAnn, x = ~year) %>%
  add_lines(y = ~est, name = "Establishments") %>%
  add_lines(y = ~emp, name = "Employment")

retAnn <- retail[year > 2006, .(est = sum(est),
                     emp = sum(empflagNum)), by = year]
retAnn[, "empGrowth" := emp - shift(emp, 1, "lag")]

graphData <- merge(retAnn, wareAnn, by = "year")
setnames(graphData, c("year", "totEst", "totEmp", "totEmpGrowth", "wareEst", "wareEmp", "wareEmpGrowth"))
graphData <- merge(graphData, dollarAnn, by = "year")
setnames(graphData, c("est", "emp", "empGrowth"), c("dollarEst", "dollarEmp", "dollarEmpGrowth"))

# Cumsums
graphData[-1, ':=' (totEmpGrowthCum = cumsum(totEmpGrowth),
                  wareEmpGrowthCum = cumsum(wareEmpGrowth),
                  dollarEmpGrowthCum = cumsum(dollarEmpGrowth))]

plot_ly(data = graphData, x = ~year) %>%
  add_lines(y = ~wareEmpGrowthCum / totEmpGrowthCum * 100, name = "Warehouse") %>%
  add_lines(y = ~dollarEmpGrowthCum / totEmpGrowthCum * 100, name = "Dollar General")

plot_ly(data = graphData, x = ~year) %>%
  add_lines(y = ~wareEst / totEst * 100, name = "Warehouse") %>%
  add_lines(y = ~dollarEst / totEst * 100, name = "Dollar General")

