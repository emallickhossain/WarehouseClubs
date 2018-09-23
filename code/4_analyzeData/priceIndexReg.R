# This does Menzio's Table 8 regression
library(data.table)
library(purrr)
library(Hmisc)
library(plotly)
library(zoo)
library(lfe)
library(stargazer)
yr <- 2004:2016

stores <- c("Dollar Store", "Discount Store", "Warehouse Club", "Grocery")
retailer <- fread(paste0("/home/mallick/Desktop/Nielsen/Data/Consumer_Panel_",
                         "Data_2004-2016/nielsen_extracts/HMS/Master_Files/Latest/retailers.tsv"))
retailer <- retailer[channel_type %in% stores]

getInd <- function(yr) {
  print(yr)
  hhIndex <- unique(fread(paste0("/home/mallick/Desktop/Nielsen/Data/Clean/",
                                 "FullData/fullData", yr, ".csv"),
                          select = c("projection_factor", "index", "purchase_date",
                                     "household_income", "panel_year", "household_code",
                                     "market", "age")))
  hhIndex <- hhIndex[panel_year == floor(purchase_date)]
  return(hhIndex)
}
hhIndex <- rbindlist(map(yr, getInd))

getShopping <- function(yr) {
  print(yr)
  purch <- unique(fread(paste0("/home/mallick/Desktop/Nielsen/Data/Clean/",
                               "Purchases/purchase", yr, ".csv"), select = "trip_code_uc"))
  trips <- fread(paste0("/home/mallick/Desktop/Nielsen/Data/Clean/Trips/trips",
                        yr, ".csv"),
                 select = c("trip_code_uc", "household_code", "purchase_date",
                            "panel_year", "retailer_code"))
  trips <- trips[trip_code_uc %in% purch$trip_code_uc]
  trips <- merge(trips, retailer, by = "retailer_code")
  behav <- trips[floor(purchase_date) == panel_year, .(trips = uniqueN(trip_code_uc),
                     stores = uniqueN(retailer_code)),
                 by = .(household_code, panel_year, purchase_date, channel_type)]
  behavWide <- dcast(behav, household_code + panel_year + purchase_date ~ channel_type,
                     value.var = c("trips", "stores"))
  setnames(behavWide, c("household_code", "panel_year", "purchase_date", "trips_disc",
                        "trips_doll", "trips_groc", "trips_ware", "stores_disc",
                        "stores_doll", "stores_groc", "stores_ware"))
  behavWide[is.na(trips_disc), "trips_disc" := 0]
  behavWide[is.na(trips_doll), "trips_doll" := 0]
  behavWide[is.na(trips_groc), "trips_groc" := 0]
  behavWide[is.na(trips_ware), "trips_ware" := 0]
  behavWide[is.na(stores_disc), "stores_disc" := 0]
  behavWide[is.na(stores_doll), "stores_doll" := 0]
  behavWide[is.na(stores_groc), "stores_groc" := 0]
  behavWide[is.na(stores_ware), "stores_ware" := 0]
  behavWide[, "tripTot" := trips_disc + trips_doll + trips_groc + trips_ware]
  behavWide[, "storesTot" := stores_disc + stores_doll + stores_groc + stores_ware]
  return(behavWide)
}
behav <- rbindlist(map(yr, getShopping))
fullData <- merge(hhIndex, behav, by = c("household_code", "panel_year", "purchase_date"))

# Looking at the relationships between shopping trips, stores visited, and price indexes
reg1 <- felm(data = fullData, index ~ tripTot | as.factor(purchase_date) + as.factor(market))
reg2 <- felm(data = fullData, index ~ storesTot | as.factor(purchase_date) + as.factor(market))
reg3 <- felm(data = fullData, index ~ tripTot + storesTot | as.factor(purchase_date) + as.factor(market))
stargazer(reg1, reg2, reg3,
          no.space = FALSE, omit.stat = "ser", type = "latex",
          title = "Effect of Shopping Behavior on Price Index",
          dep.var.labels.include = FALSE,
          dep.var.caption = "Household Price Index",
          covariate.labels = c("Shopping Trips", "Stores Visited"),
          notes = c("Market and quarter fixed effects are included."),
          notes.append = TRUE,
          notes.align = "l",
          out = "./code/5_figures/tables/table8Menzio.tex")

reg <- felm(data = fullData, index ~ trips_disc + stores_disc |
              as.factor(purchase_date) + as.factor(market))
summary(reg)

reg <- felm(data = fullData, index ~ trips_doll + stores_doll |
              as.factor(purchase_date) + as.factor(market))
summary(reg)

reg <- felm(data = fullData, index ~ trips_groc + stores_groc |
              as.factor(purchase_date) + as.factor(market))
summary(reg)

reg <- felm(data = fullData, index ~ trips_ware + stores_ware |
              as.factor(purchase_date) + as.factor(market))
summary(reg)

# Adding in income
fullData[household_income %in% c("<25k", "25-50k"), "incomeFact" := "<50k"]
fullData[household_income %in% c("50-100k", ">100k"), "incomeFact" := ">50k"]
reg1 <- felm(data = fullData, index ~ tripTot * incomeFact |
                                      as.factor(purchase_date) + as.factor(market))
reg2 <- felm(data = fullData, index ~ storesTot * incomeFact |
                                      as.factor(purchase_date) + as.factor(market))
reg3 <- felm(data = fullData, index ~ tripTot * incomeFact + storesTot * incomeFact |
                                      as.factor(purchase_date) + as.factor(market))
stargazer(reg1, reg2, reg3,
          no.space = FALSE, omit.stat = "ser", type = "latex",
          title = "Effect of Shopping Behavior on Price Index",
          dep.var.labels.include = FALSE,
          dep.var.caption = "Household Price Index",
          covariate.labels = c("Shopping Trips", "Stores Visited", ">50k",
                               "Trips * >50k", "Stores * >50k", "Stores * >50k"),
          notes = c("Market and quarter fixed effects are included."),
          notes.append = TRUE,
          notes.align = "l",
          out = "./code/5_figures/tables/table8MenzioIncome.tex")

reg <- felm(data = fullData, index ~ stores_disc * household_income + trips_disc * household_income |
              as.factor(purchase_date) + as.factor(market))
summary(reg)

reg <- felm(data = fullData, index ~ stores_doll * household_income + trips_doll  * household_income|
              as.factor(purchase_date) + as.factor(market))
summary(reg)

reg <- felm(data = fullData, index ~ stores_groc * household_income + trips_groc  * household_income|
              as.factor(purchase_date) + as.factor(market))
summary(reg)

reg <- felm(data = fullData, index ~ stores_ware * household_income  + trips_ware  * household_income|
              as.factor(purchase_date) + as.factor(market))
summary(reg)
