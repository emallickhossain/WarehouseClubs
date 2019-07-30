# Looks at size differences when warehouse clubs open within some distance cutoff
library(data.table)
library(geosphere)
library(lfe)
library(stargazer)
library(zoo)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(readxl)
threads <- 8
distanceKM1 <- 20
distanceKM2 <- 40

# Classifying products as bulk/not and storable/not
storable <- fread(paste0("/scratch/upenn/hossaine/storableClassification.csv"))
retailers <- fread("/scratch/upenn/hossaine/fullRetailers.csv")
panel <- fread("/scratch/upenn/hossaine/fullPanel.csv",
               select = c("panel_year", "fips", "household_code",
                          "projection_factor", "household_income", "household_size",
                          "age", "child", "market", "lat", "lon"))

# Getting monthly bulk and warehouse "share" by household-month
# Getting all purchases by year
trips <- fread("/scratch/upenn/hossaine/fullTrips.csv", nThread = threads,
               select = c("trip_code_uc", "household_code", "panel_year",
                          "retailer_code", "purchase_date"))

quarterShares <- NULL
for (i in 2004:2017) {
  print(i)
  purch <- fread(paste0("/scratch/upenn/hossaine/fullPurch", i, ".csv"),
                 nThread = threads)

  # Merging with trips
  purch <- merge(purch, trips, by = "trip_code_uc")
  purch[, "purchase_date" := as.Date(purchase_date)]
  purch[, c("year", "quarter") := .(year(purchase_date), quarter(purchase_date))]
  purch[, "bulk" := ifelse(quintile >= 4, 1L, 0L)]
  purch[, "totalExp" := packagePrice * quantity]

  # Getting expenditure-weighted generic and bulk shares by month
  purch <- purch[, .(bulk = weighted.mean(bulk, w = totalExp)),
                 by = .(household_code, panel_year, year, quarter)]
  purch <- purch[panel_year == year]
  purch[, "year" := NULL]
  quarterShares <- rbindlist(list(quarterShares, purch), use.names = TRUE)
}

quarterShares <- merge(quarterShares, panel, by = c("household_code", "panel_year"))
quarterShares[, "household_income" := factor(household_income)]
quarterShares[, "YQ" := as.yearqtr(paste(panel_year, quarter, sep = "-"))]
fwrite(quarterShares, "/scratch/upenn/hossaine/quarterShares.csv", nThread = threads)

# Geocoding households
quarterShares <- fread("/scratch/upenn/hossaine/quarterShares.csv")
quarterShares[, "YQ" := as.yearqtr(YQ)]
setorder(quarterShares, household_code, YQ)
hhGeos <- na.omit(unique(quarterShares[, .(household_code, lat, lon)]))
hhGeos[, "hhGeoID" := 1:.N]

# Getting club opening data and merging with lat lon by zip code
clubs <- na.omit(fread("./Nielsen/Data/club_store_openings_by_zip.csv"))
setnames(clubs, "zip", "zip_code")
setkey(clubs, zip_code, open_club)
clubs[, "open_club" := as.Date(open_club)]
clubs[, c("open_year", "open_quarter") := .(year(open_club), quarter(open_club))]
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
hhGeos <- merge(hhGeos, clubs[, .(open_year, open_quarter, clubname, clubGeoID)],
                by.x = "closestClub", by.y = "clubGeoID")
quarterShares <- merge(quarterShares, hhGeos[, .(household_code, lat, lon, distClubKM,
                                                 open_year, open_quarter, clubname)],
                 by = c("household_code", "lat", "lon"))
quarterShares[, "openYQ" := as.yearqtr(paste(open_year, open_quarter, sep = "-"))]
quarterShares[, "open" := ifelse(YQ >= openYQ, 1L, 0L)]
quarterShares[, "nearby1" := (distClubKM <= distanceKM1) * open]
quarterShares[, "nearby2" := (distClubKM > distanceKM1 & distClubKM <= distanceKM2) * open]
quarterShares[, "HHMarket" := paste0(household_code, market)]
quarterShares[, "marketQuarter" := paste0(market, YQ)]
quarterShares[, "quartersAfterOpen" := (YQ - openYQ) * 4]

# Getting balanced panel for event study
# First getting all households that are in the window of 4 quarters before entry
# and 8 quarters after
# Second, getting all households that did not move in that window.
quarterShares[, "window" := ifelse(quartersAfterOpen %in% -4:8, 1L, 0L)]
quarterShares[, "moves" := uniqueN(HHMarket), by = .(household_code, window)]
quarterShares[, "B" := window * moves]
quarterShares[B != 1, "B" := 0]

# Running regressions of purchase sizes pre and post opening (coarse income shares)
quarterShares[household_income %in% 3:13, "household_income_coarse" := "<25k"]
quarterShares[household_income %in% 15:19, "household_income_coarse" := "25-50k"]
quarterShares[household_income %in% 21:26, "household_income_coarse" := "50-100k"]
quarterShares[household_income %in% 27:30, "household_income_coarse" := ">100k"]

reg1 <- felm(data = quarterShares, bulk ~ nearby1 + nearby2 +
               household_size + age + child | HHMarket + marketQuarter | 0 | HHMarket)
reg2 <- felm(data = quarterShares, bulk ~ nearby1 * household_income_coarse +
               nearby2 * household_income_coarse +
               household_size + age + child | HHMarket + marketQuarter | 0 | HHMarket)

stargazer(reg1, reg2, type = "text",
          add.lines = list(c("Household-Market FE", "Y", "Y"),
                           c("Market-Quarter FE", "Y", "Y")),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          dep.var.caption = "", dep.var.labels.include = FALSE,
          keep = c("nearby*", "household_income*"),
          order = c(1, 10, 11, 9, 5, 13, 14, 12, 3, 4, 2),
          covariate.labels = c("<20 km", "<20 km : 25-50k",
                               "<20 km : 50-100k", "<20 km : >100k",
                               "20-40 km", "20-40 km : 25-50k",
                               "20-40 km : 50-100k", "20-40 km : >100k",
                               "25-50k", "50-100k", ">100k"),
          notes.align = "l",
          notes = c("Standard errors are clustered at the household-market level."),
          digits = 3,
          label = "tab:clubOpening",
          title = "Warehouse Club Opening",
          out = "tables/clubOpening.tex")

# Running event study regression
quarterShares[, "BQ" := ifelse(B == 1, quartersAfterOpen, -99)]
quarterShares[, "BQ" := relevel(factor(BQ), ref = "-1")]
reg1 <- felm(data = quarterShares, bulk ~ BQ + household_income_coarse +
               household_size + age + child | HHMarket + marketQuarter | 0 | HHMarket)
reg2 <- felm(data = quarterShares[household_income_coarse == "<25k"], bulk ~ BQ +
               household_size + age + child | HHMarket + marketQuarter | 0 | HHMarket)
reg3 <- felm(data = quarterShares[household_income_coarse == "25-50k"], bulk ~ BQ +
               household_size + age + child | HHMarket + marketQuarter | 0 | HHMarket)
reg4 <- felm(data = quarterShares[household_income_coarse == "50-100k"], bulk ~ BQ +
               household_size + age + child | HHMarket + marketQuarter | 0 | HHMarket)
reg5 <- felm(data = quarterShares[household_income_coarse == ">100k"], bulk ~ BQ +
               household_size + age + child | HHMarket + marketQuarter | 0 | HHMarket)
stargazer(reg1, reg2, reg3, reg4, reg5, type = "text",
          add.lines = list(c("Household-Market FE", "Y", "Y", "Y", "Y", "Y"),
                           c("Market-Quarter FE", "Y", "Y", "Y", "Y", "Y")),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("All", "<25k", "25-50k", "50-100k", ">100k"),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          keep = c("nearby*"),
          covariate.labels = c("<20 km", "20-40km"),
          notes.align = "l",
          notes = c("Standard errors are clustered at the household-market level."),
          digits = 3,
          label = "tab:clubOpening",
          title = "Warehouse Club Opening",
          out = "tables/clubOpeningEventStudy.tex")

# Plotting Event study
graphData1 <- data.table(betas = c(reg1$coefficients[2:4], 0, reg1$coefficients[5:13]),
                         se = c(reg1$cse[2:4], 0, reg1$cse[5:13]),
                         quarters = -4:8,
                         type = "All")
graphData2 <- data.table(betas = c(reg2$coefficients[2:4], 0, reg2$coefficients[5:13]),
                         se = c(reg2$cse[2:4], 0, reg2$cse[5:13]),
                         quarters = -4:8,
                         type = "<25k")
graphData3 <- data.table(betas = c(reg3$coefficients[2:4], 0, reg3$coefficients[5:13]),
                         se = c(reg3$cse[2:4], 0, reg3$cse[5:13]),
                         quarters = -4:8,
                         type = "25-50k")
graphData4 <- data.table(betas = c(reg4$coefficients[2:4], 0, reg4$coefficients[5:13]),
                         se = c(reg4$cse[2:4], 0, reg4$cse[5:13]),
                         quarters = -4:8,
                         type = "50-100k")
graphData5 <- data.table(betas = c(reg5$coefficients[2:4], 0, reg5$coefficients[5:13]),
                         se = c(reg5$cse[2:4], 0, reg5$cse[5:13]),
                         quarters = -4:8,
                         type = ">100k")

graphData <- rbindlist(list(graphData1, graphData2, graphData3, graphData4, graphData5), use.names = TRUE)
graphData[, "type" := factor(type, levels = c("All", "<25k", "25-50k", "50-100k", ">100k"), ordered = TRUE)]
ggplot(graphData, aes(x = quarters, y = betas)) +
  geom_errorbar(aes(ymin = betas - 1.96 * se, ymax = betas + 1.96 * se), width = 1) +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  facet_wrap(vars(type)) +
  labs(title = "Club Openings Increase Bulk Buying",
       subtitle = "Low-Income Households Are Unaffected",
       x = "Quarters After Entry",
       y = "Change in Bulk Purchasing (pp)",
       caption = paste0("Source: Author calulations using Nielsen Consumer Panel.")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text())
ggsave(filename = "./figures/eventStudy.png", height = 6, width = 6)
