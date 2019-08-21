# Looks at size differences when warehouse clubs open within some distance cutoff
# Only go through 2015 because that's the last full year I observe in the club data
# Step 1: Get earliest club opening date by zip code
# Step 2: Get household lat lon pairs
# Step 3: Computing distances between clubs and households
# Step 4: Assigning treatment and control indicators
# Step 5: Getting summary stats of treatment and controls
# Step 6: Computer quarterly bulk purchasing
library(data.table)
library(geosphere)
library(lfe)
library(stargazer)
library(zoo)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(readxl)
library(Hmisc)
library(stringr)
library(knitr)
threads <- 8
distanceKM <- 15 * 1.609344 #15 miles converted to km

# Step 1: Getting earliest club open date by zip code
# Getting club opening data and merging with lat lon by zip code
# Only getting the first club opening. There might be interesting
# competitive effects, but that's of second-order importance.
clubs <- na.omit(fread("./Nielsen/Data/club_store_openings_by_zip.csv"))
setnames(clubs, "zip", "zip_code")
setkey(clubs, zip_code, open_club)
clubs[, "open_club" := as.Date(open_club)]
clubs <- clubs[, .(open_club = min(open_club)), by = zip_code]
clubs[, c("open_year", "open_quarter") := .(year(open_club), quarter(open_club))]
zipLatLon <- fread("./Nielsen/Data/zipLatLon.csv")
clubs <- merge(clubs, zipLatLon, by = "zip_code")
clubs[, "clubGeoID" := 1:.N]

# Step 2: Get household geocodes
hh <- unique(fread("/scratch/upenn/hossaine/fullPanel.csv",
                   select = c("household_code", "lat", "lon")))
setkey(hh, household_code, lat, lon)
hh[, "hhID" := 1:.N]

# Step 3: Computing distances between clubs and households
# Assigning opening dates for the first club within a certain radius
distMat <- distm(hh[, .(lon, lat)], clubs[, .(lon, lat)]) / 1000
distInd <- (distMat <= distanceKM)
openDatesDist <- matrix(clubs$open_club, ncol = nrow(clubs), nrow = nrow(hh), byrow = TRUE)
hhDatesDist <- distInd * openDatesDist
hhDatesDist[hhDatesDist == 0] <- NA
clubOpenDist <- apply(hhDatesDist, 1, min, na.rm = TRUE)
hh[, "openDateDist" := clubOpenDist]
hh[is.infinite(openDateDist), "openDateDist" := NA]
hh[, "openDateDist" := as.Date(openDateDist)]
hh[, "hhID" := NULL]

# Step 3b: Getting closest warehouse club by household
closestClub <- apply(distMat, 1, min)
hh[, "closestClub" := closestClub]

# Step 4: Assigning treatment and control indicators
# AlwaysClub is households that always had a club nearby
# NeverClub are households that never had one nearby
# Treatment are those that experienced an opening
panel <- fread("/scratch/upenn/hossaine/fullPanel.csv",
               select = c("panel_year", "fips", "household_code", "married",
                          "household_income", "household_size", "projection_factor",
                          "age", "child", "lat", "lon", "dma_cd",
                          "household_income_coarse", "zip_code"))
panel <- merge(panel, hh, by = c("household_code", "lat", "lon"))
panel[, "minYear" := min(panel_year), by = .(household_code, zip_code)]
panel[, "maxYear" := max(panel_year), by = .(household_code, zip_code)]
panel[, "group" := ifelse(year(openDateDist) >= minYear &
                          year(openDateDist) <= maxYear,
                          "Treatment", "Control")]
panel[is.na(group), "group" := "Control"]

# Step 4b: Getting average distance by household income and year
closestClubDist <- panel[, .(avg = weighted.mean(closestClub, w = projection_factor),
                             sd = sqrt(wtd.var(closestClub, w = projection_factor)),
                             q25 = wtd.quantile(closestClub, probs = 0.25,
                                                weights = projection_factor),
                             q75 = wtd.quantile(closestClub, probs = 0.75,
                                                weights = projection_factor)),
                         keyby = .(household_income_coarse, panel_year)]
closestClubDist[, lapply(.SD / 1.609344, mean), by = household_income_coarse]
# Save in costcoDist.tex

# Step 5: Summary stats of treatment and control groups
panel[, "household_income" := factor(household_income,
                                     levels = c(3, 4, 6, 8, 10, 11, 13, 15, 16,
                                                17, 18, 19, 21, 23, 26, 27),
                                     labels = c(2.5, 6.5, 9, 11, 13.5, 17.5,
                                                22.5, 27.5, 32.5, 37.5, 42.5,
                                                47.5, 55, 65, 85, 100),
                                     ordered = TRUE)]
panel[, "household_income" := as.numeric(as.character(household_income))]
cols <- c("household_income", "household_size" ,"age", "child", "married")

sumStatsMean <- panel[, lapply(.SD, mean),
                      by = .(group), .SDcols = cols][, "stat" := "Mean"]
sumStatsSD <- panel[, lapply(.SD, sd),
                      by = .(group), .SDcols = cols][, "stat" := "SD"]
sumStatsq25 <- panel[, lapply(.SD, quantile, 0.25),
                     by = group, .SDcols = cols][, "stat" := "25th Percentile"]
sumStatsq75 <- panel[, lapply(.SD, quantile, 0.75),
                     by = group, .SDcols = cols][, "stat" := "75th Percentile"]
sumStatsN <- panel[, uniqueN(household_code), by = group][, "stat" := "N"]
sumStats <- rbindlist(list(sumStatsMean, sumStatsSD, sumStatsq25, sumStatsq75),
                      use.names = TRUE)
sumStatsLong <- melt(data = sumStats, id.vars = c("group", "stat"))
sumStatsWide <- dcast(data = sumStatsLong, group + variable ~ stat, value.var = "value")
sumStatsWide[variable == "household_income", "variable" := "Income ($1000)"]
sumStatsWide[variable == "household_size", "variable" := "Household Size"]
sumStatsWide[variable == "age", "variable" := "Age"]
sumStatsWide[variable == "child", "variable" := "Child Present"]
sumStatsWide[variable == "married", "variable" := "Married"]
kable(sumStatsWide[group == "Treatment"])
kable(sumStatsWide[group == "Control"])
# Save to costcoSummaryStats.tex

# Step 6: Computing quarterly bulk purchasing
# Getting quarterly bulk shares for each household
trips <- fread("/scratch/upenn/hossaine/fullTrips.csv", nThread = threads,
               select = c("trip_code_uc", "household_code", "panel_year",
                          "retailer_code", "purchase_date"))
retailers <- fread("/scratch/upenn/hossaine/fullRetailers.csv")
trips <- merge(trips, retailers, by = "retailer_code")

quarterShares <- NULL
for (i in 2004:2015) {
  print(i)
  purch <- fread(paste0("/scratch/upenn/hossaine/fullPurch", i, ".csv"),
                 nThread = threads,
                 select = c("trip_code_uc", "quantity", "packagePrice",
                            "quintile", "food"))

  # Merging with trips
  purch <- merge(purch, trips, by = "trip_code_uc")
  purch[, "purchase_date" := as.Date(purchase_date)]
  purch[, c("year", "quarter") := .(year(purchase_date), quarter(purchase_date))]
  purch[, "bulk" := ifelse(quintile >= 4, 1L, 0L)]
  purch[, "club" := ifelse(channel_type == "Warehouse Club", 1L, 0L)]
  purch[, "totalExp" := packagePrice * quantity]

  # Summing expenditures by food/non-food, club/non-club, and bulk/non-bulk
  purch <- purch[, .(totalExp = sum(totalExp)),
                     by = .(household_code, panel_year, year, quarter,
                            food, bulk, club)]

  purch <- purch[panel_year == year]
  purch[, "year" := NULL]
  quarterShares <- rbindlist(list(quarterShares, purch), use.names = TRUE)
}

quarterShares <- merge(quarterShares, panel, by = c("household_code", "panel_year"))
quarterShares[, c("lat", "lon", "fips", "minYear", "maxYear") := NULL]

# Step 7: Adding pre- and post-opening indicators
quarterShares[, "YQ" := as.yearqtr(paste(panel_year, quarter, sep = "-"))]
quarterShares[, "openDateQtr" := as.yearqtr(openDateDist)]
quarterShares[, "postOpenQtr" := (YQ - openDateQtr) * 4]
quarterShares[, "postOpen" := (postOpenQtr >= 0)]
quarterShares[is.na(postOpen), "postOpen" := FALSE]
fwrite(quarterShares, "/scratch/upenn/hossaine/quarterShares.csv", nThread = threads)

#################################################################################
############# RUNNING REGRESSION ON OVERALL BULK PURCHASING ####################
################################################################################
purchData <- fread("/scratch/upenn/hossaine/quarterShares.csv", nThread = threads)
purchData[, "hhMarket" := paste(household_code, zip_code, sep = "_")]

# Cleaning up groups. If household entered right when warehouse club entered, they
# are not a treatment group
purchData[, c("postOpen", "group") := NULL]
purchData[, "postOpen" := (openDateQtr < YQ)]
purchData[is.na(postOpen), "postOpen" := FALSE]
purchData[, "preInd" := sum(postOpenQtr < 0), by = hhMarket]
purchData[, "postInd" := sum(postOpenQtr > 0), by = hhMarket]
purchData[is.na(preInd), "preInd" := 0]
purchData[is.na(postInd), "postInd" := 0]
purchData[preInd > 0 & postInd > 0, "group" := "Treatment"]
purchData[preInd >= 0 & postInd == 0, "group" := "NeverClub"]
purchData[preInd == 0 & postInd > 0, "group" := "AlwaysClub"]
purchData[, c("preInd", "postInd") := NULL]

# Some households are in the sample right when a club opens. I remove them so they
# don't affect the estimate of pre-/post-entry behavior since I code 0 as pre-open
dropIDs <- purchData[, uniqueN(postOpen), by = .(hhMarket, group)]
dropIDs <- dropIDs[group != "Treatment" & V1 == 2]$hhMarket
purchData <- purchData[!hhMarket %in% dropIDs]

# Getting expenditure-weighted generic and bulk shares by month (distance 1)
quarterSharesAll <- purchData[, .(bulk = weighted.mean(bulk, w = totalExp),
                                  club = as.integer(sum(club) > 0)),
                              by = .(household_code, panel_year, quarter, YQ,
                                     married, household_income_coarse,
                                     household_size, age, child, group,
                                     postOpenQtr, postOpen, hhMarket)]
quarterSharesFood <- purchData[, .(bulk = weighted.mean(bulk, w = totalExp),
                                   club = as.integer(sum(club) > 0)),
                               by = .(household_code, panel_year, quarter, YQ,
                                      married, household_income_coarse,
                                      household_size, age, child, group,
                                      postOpenQtr, postOpen, hhMarket, food)]

# Running diff-in-diff on treatment and never club group. I ignore whether they
# are a club shopper or not. This should be a lower bound on the effect
quarterSharesAll[, "treatment" := ifelse(group == "Treatment", 1L, 0L)]
quarterSharesAll[, "groupYQ" := paste(YQ, group, sep = "_")]
quarterSharesAll[, "always" := ifelse(group == "AlwaysClub", 1L, 0L)]
quarterSharesAll[, "postOpenQtr" := relevel(as.factor(postOpenQtr), ref = "-1")]
quarterSharesFood[, "treatment" := ifelse(group == "Treatment", 1L, 0L)]
quarterSharesFood[, "groupYQ" := paste(YQ, group, sep = "_")]
quarterSharesFood[, "always" := ifelse(group == "AlwaysClub", 1L, 0L)]
quarterSharesFood[, "postOpenQtr" := relevel(as.factor(postOpenQtr), ref = "-1")]

reg1 <- felm(bulk ~ treatment + treatment : postOpen +
               household_size + age + married + child |
               YQ | 0 | household_code,
             data = quarterSharesAll[group != "AlwaysClub"])
reg2 <- felm(bulk ~ treatment * household_income_coarse +
               treatment : postOpen * household_income_coarse +
               household_size + age + married + child |
               YQ | 0 | household_code,
             data = quarterSharesAll[group != "AlwaysClub"])
reg3 <- felm(bulk ~ treatment * household_income_coarse +
               treatment : postOpen * household_income_coarse +
               household_size + age + married + child |
               groupYQ | 0 | household_code,
             data = quarterSharesAll[group != "AlwaysClub"])
reg4 <- felm(bulk ~ treatment * household_income_coarse +
               treatment : postOpen * household_income_coarse +
               household_size + age + married + child |
               groupYQ | 0 | household_code,
             data = quarterSharesFood[group != "AlwaysClub" & food == 0])
reg5 <- felm(bulk ~ treatment * household_income_coarse +
               treatment : postOpen * household_income_coarse +
               household_size + age + married + child |
               groupYQ | 0 | household_code,
             data = quarterSharesFood[group != "AlwaysClub" & food == 1])
stargazer(reg1, reg2, reg3, reg4, reg5, type = "text",
          add.lines = list(c("Demographic Controls", "Y", "Y", "Y", "Y", "Y"),
                           c("Year-Qtr FE's", "Y", "Y", "Y", "Y", "Y"),
                           c("Group-Year-Qtr FE's", "N", "N", "Y", "Y", "Y")),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("All", "Food", "Non-Food"),
          column.separate = c(3, 1, 1),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          keep = c("treatment*"),
          order = c(1, 10, 11, 9, 12, 14, 15, 13),
          covariate.labels = c("Treatment",
                               " . : 25-50k", " . : 50-100k", " . : >100k",
                               "Treatment : Post-Entry",
                               " . : 25-50k", " . : 50-100k", " . : >100k"),
          notes.align = "l",
          notes.append = TRUE,
          digits = 3,
          out = "tables/costcoEntryDD.tex")

# Adding in always group
reg1 <- felm(bulk ~ treatment +
               always +
               treatment : postOpen +
               household_size + age + married + child |
               YQ | 0 | household_code,
             data = quarterSharesAll)
reg2 <- felm(bulk ~ treatment * household_income_coarse +
               always * household_income_coarse +
               treatment : postOpen * household_income_coarse +
               household_size + age + married + child |
               YQ | 0 | household_code,
             data = quarterSharesAll)
reg3 <- felm(bulk ~ treatment * household_income_coarse +
               always * household_income_coarse +
               treatment : postOpen * household_income_coarse +
               household_size + age + married + child |
               groupYQ | 0 | household_code,
             data = quarterSharesAll)
reg4 <- felm(bulk ~ treatment * household_income_coarse +
               always * household_income_coarse +
               treatment : postOpen * household_income_coarse +
               household_size + age + married + child |
               groupYQ | 0 | household_code,
             data = quarterSharesFood[food == 0])
reg5 <- felm(bulk ~ treatment * household_income_coarse +
               always * household_income_coarse +
               treatment : postOpen * household_income_coarse +
               household_size + age + married + child |
               groupYQ | 0 | household_code,
             data = quarterSharesFood[food == 1])
stargazer(reg1, reg2, reg3, reg4, reg5, type = "text",
          add.lines = list(c("Demographic Controls", "Y", "Y", "Y", "Y", "Y"),
                           c("Year-Qtr FE's", "Y", "Y", "Y", "Y", "Y"),
                           c("Group-Year-Qtr FE's", "N", "N", "Y", "Y", "Y")),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("All", "Food", "Non-Food"),
          column.separate = c(3, 1, 1),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          keep = c("treatment*", "*always", "*income"),
          order = c(16, 18, 19, 17, 1, 11, 12, 10, 5, 14, 15, 13, 3, 4, 2),
          covariate.labels = c("Treatment : Post-Entry",
                               " . : 25-50k", " . : 50-100k", " . : >100k",
                               "Treatment",
                               " . : 25-50k", " . : 50-100k", " . : >100k",
                               "Always Club",
                               " . : 25-50k", " . : 50-100k", " . : >100k",
                               "25-50k", "50-100k", ">100k"),
          notes.align = "l",
          notes.append = TRUE,
          digits = 3,
          out = "tables/costcoEntryDDAlways.tex")













# Running diff-in-diff regression
reg1 <- felm(bulk ~  postOpen * club +
               household_size + age + married + child |
               hhMarket + YQ | 0 | household_code,
             data = quarterSharesFood[group == "Treatment" & food == 1])
reg2 <- felm(bulk ~  postOpen * club + household_size + age + married + child |
               hhMarket + YQ | 0 | household_code,
             data = quarterSharesFood[group == "Treatment" & food == 0])

stargazer(reg1, reg2, type = "text",
          add.lines = list(c("Demographic Controls", "Y", "Y"),
                           c("Household-ZIP FE's", "Y", "Y")),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("Food", "Non-Food"),
          column.separate = c(1, 1),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          keep = c("postOpen", "club*"),
          order = c(1, 2, 7),
          covariate.labels = c("Post-Entry", "Club Shopper", "Post-Entry : Club Shopper"),
          notes.align = "l",
          notes.append = TRUE,
          digits = 3,
          notes = c("Source: Author calulations from Nielsen Consumer Panel.",
                    "Demographic controls include household size, age, and ",
                    "presence of children. Standard errors are clustered at ",
                    "the household level."),
          label = "tab:costcoEntry",
          title = "Warehouse Club Entry Increases Bulk Buying",
          out = "tables/costcoEntry.tex")

# Event study
quarterSharesAll[, "postOpenQtr" := relevel(as.factor(postOpenQtr), ref = "-1")]

reg1 <- felm(bulk ~ postOpenQtr * club +
               household_size + age + married + child |
               hhMarket | 0 | household_code,
             data = quarterSharesAll[group == "Treatment" &
                                       postOpenQtr %in% paste(-8:8)])

# Plotting Event study
graphData1 <- as.data.table(summary(reg1)$coefficients, keep.rownames = TRUE)
confInt1 <- as.data.table(confint(reg1), keep.rownames = TRUE)
graphData1 <- merge(graphData1, confInt1, by = "rn")
graphData1 <- graphData1[grepl("postOpenQtr*", rn)]
graphData1[, c("quarter", "club") := tstrsplit(rn, ":", fixed = TRUE)][, "rn" := NULL]
graphData1[is.na(club), "club" := "nonclub"]
graphData1[, "quarter" := as.integer(gsub("postOpenQtr", "", quarter))]

graphData <- rbindlist(list(graphData1,
                            list(0, 0, 0, 0, 0, 0, -1, "club"),
                            list(0, 0, 0, 0, 0, 0, -1, "nonclub")),
                       use.names = TRUE)
setnames(graphData, c("betas", "se", "t", "p", "LCL", "UCL", "quarter", "club"))
graphData[club == "club", "club" := "Club Shopper"]
graphData[club == "nonclub", "club" := "Not Club Shopper"]
ggplot(graphData, aes(x = quarter, y = betas, color = club)) +
  geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.2) +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  labs(x = "Quarters After Entry",
       y = "Change in Bulk Purchasing (pp)",
       color = "Shopper Type") +
  theme_tufte() +
  theme(axis.title = element_text(),
        legend.position = "bottom") +
  scale_color_grey()

ggsave(filename = "./figures/eventStudy.png", height = 4, width = 6)
