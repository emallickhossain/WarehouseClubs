# Looks at size differences when warehouse clubs open within some distance cutoff
library(data.table)
library(geosphere)
library(lfe)
library(stargazer)
library(zoo)
library(lubridate)
library(ggplot2)
library(ggthemes)
path <- "/scratch/upenn/hossaine/"
threads <- 8
distanceKM <- 30

# Getting purchase data
tpPurch <- fread(paste0(path, "7260Purch.csv"), nThread = threads)[drop %in% c(0, 2)]
tpPurch[, "purchase_date" := as.Date(purchase_date, format = "%Y-%m-%d")]
tpPurch[, "month" := month(purchase_date)]
tpPurch[, "monthYear" := paste0(panel_year, month)]
tpPurch <- na.omit(tpPurch, cols = c("size", "rate"))
tpPurch <- tpPurch[rate < Inf & rate > 0]
setorder(tpPurch, household_code, purchase_date)
hhGeos <- na.omit(unique(tpPurch[, .(household_code, lat, lon)]))
hhGeos[, "hhGeoID" := 1:.N]

# Getting club opening data and merging with lat lon by zip code
clubs <- na.omit(fread("./Nielsen/Data/club_store_openings_by_zip.csv"))
setnames(clubs, "zip", "zip_code")
setkey(clubs, zip_code, open_club)
clubs[, "open_club" := as.Date(open_club)]
zipLatLon <- fread("./Nielsen/Data/zipLatLon.csv")
clubs <- merge(clubs, zipLatLon, by = "zip_code")
clubs[, "clubGeoID" := 1:.N]

# Computing distances and finding closest store (most everyone lives within about
# 100km to a warehouse club)
distMat <- distm(hhGeos[, .(lon, lat)], clubs[, .(lon, lat)])
closestClub <- apply(distMat, 1, which.min)
distClub <- apply(distMat, 1, min)
hhGeos[, ':=' (closestClub = as.integer(closestClub),
               distClubKM = distClub / 1000)]
hhGeos <- merge(hhGeos, clubs[, .(open_club, clubname, clubGeoID)],
                by.x = "closestClub", by.y = "clubGeoID")
tpPurch <- merge(tpPurch, hhGeos[, .(household_code, lat, lon, distClubKM, open_club, clubname)],
                 by = c("household_code", "lat", "lon"))
tpPurch[, "monthsSinceOpen" := interval(open_club, purchase_date) %/% months(1)]
tpPurch[, "nearby" := (distClubKM <= distanceKM)]
tpPurch[, "distClubKM2" := distClubKM ^ 2]

# Running regressions of purchase sizes pre and post opening
openingData <- tpPurch[monthsSinceOpen %in% -12:12]
openingData[, "monthsSinceOpen" := relevel(factor(monthsSinceOpen), ref = "-1")]
reg1 <- felm(data = openingData, log(size) ~ monthsSinceOpen |
               household_code + monthYear,
             weights = openingData$projection_factor)
reg2 <- felm(data = openingData, log(size) ~ monthsSinceOpen * nearby |
               household_code + monthYear,
             weights = openingData$projection_factor)
reg3 <- felm(data = openingData, log(size) ~ monthsSinceOpen * household_income_coarse |
               household_code + monthYear,
             weights = openingData$projection_factor)
reg4 <- felm(data = openingData, log(size) ~ monthsSinceOpen * household_income_coarse * nearby |
               household_code + monthYear,
             weights = openingData$projection_factor)
stargazer(reg1, reg2, reg3, reg4, type = "text")




stargazer(reg1, reg2, reg3, type = "text",
          add.lines = list(c("Household/MSA FE", "Y", "Y", "Y")),
          single.row = TRUE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("Openings"), column.separate = c(3),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          covariate.labels = c("Open", ">100k", "50-100k", "25-50k",
                               "Open:>100k", "Open:50-100k", "Open:25-50k"),
          notes.align = "l",
          notes = c("Standard errors are clustered at the market level.",
                    "Package size is of standardized 250, 2-ply rolls.",
                    "Club openings limited to a 30km radius."),
          order = c(1, 2, 4, 3, 5, 7, 6),
          digits = 2,
          label = "tab:clubOpening",
          title = "Warehouse Club Opening",
          out = "tables/clubOpeningTP.tex")
