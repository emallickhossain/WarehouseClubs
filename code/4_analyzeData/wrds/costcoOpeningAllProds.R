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

# Looping over distances
for (i in c(5, 10, 15, 20)) {
  print(i)
  distanceKM <- i * 1.609344
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
                            "household_income", "men", "women", "projection_factor",
                            "age", "nChildren", "lat", "lon", "dma_cd", "college",
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
  panel[, "hhSize" := men + women + nChildren]
  cols <- c("household_income", "hhSize", "age", "married", "college")

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
  sumStatsWide[variable == "hhSize", "variable" := "Household Size"]
  sumStatsWide[variable == "age", "variable" := "Age"]
  sumStatsWide[variable == "married", "variable" := "Married"]
  sumStatsWide[variable == "college", "variable" := "College"]
  kable(sumStatsWide[group == "Treatment"])
  kable(sumStatsWide[group == "Control"])
  panel[, .N, by = group]
  # Testing for differences in means
  t.test(household_income ~ group, data = panel, conf.level = 0.95)
  t.test(hhSize ~ group, data = panel, conf.level = 0.95)
  t.test(age ~ group, data = panel, conf.level = 0.95)
  t.test(married ~ group, data = panel, conf.level = 0.95)
  t.test(college ~ group, data = panel, conf.level = 0.95)
  # Save to costcoSummaryStats.tex

  # Step 6: Computing quarterly bulk purchasing
  # Getting quarterly bulk shares for each household
  trips <- fread("/scratch/upenn/hossaine/fullTrips.csv", nThread = threads,
                 select = c("trip_code_uc", "household_code", "panel_year",
                            "retailer_code", "purchase_date"))
  retailers <- fread("/scratch/upenn/hossaine/fullRetailers.csv")
  trips <- merge(trips, retailers, by = "retailer_code")

  quarterShares <- NULL
  for (j in 2004:2015) {
    print(j)
    purch <- fread(paste0("/scratch/upenn/hossaine/fullPurch", j, ".csv"),
                   nThread = threads,
                   select = c("trip_code_uc", "quantity", "packagePrice",
                              "quintile", "food"))[food == 0]

    # Merging with trips
    purch <- merge(purch, trips, by = "trip_code_uc")
    purch[, "purchase_date" := as.Date(purchase_date)]
    purch[, c("year", "quarter") := .(year(purchase_date), quarter(purchase_date))]
    purch[, "bulk" := ifelse(quintile >= 4, 1L, 0L)]
    purch[, "club" := ifelse(channel_type == "Warehouse Club", 1L, 0L)]
    purch[, "totalExp" := packagePrice * quantity]

    # Summing expenditures by club/non-club, and bulk/non-bulk
    purch <- purch[, .(totalExp = sum(totalExp)),
                       by = .(household_code, panel_year, year, quarter, bulk, club)]

    purch <- purch[panel_year == year]
    purch[, "year" := NULL]
    quarterShares <- rbindlist(list(quarterShares, purch), use.names = TRUE)
  }

  quarterSharesFinal <- merge(quarterShares, panel, by = c("household_code", "panel_year"))
  quarterSharesFinal[, c("lat", "lon", "fips", "minYear", "maxYear") := NULL]

  # Step 7: Adding pre- and post-opening indicators
  quarterSharesFinal[, "YQ" := as.yearqtr(paste(panel_year, quarter, sep = "-"))]
  quarterSharesFinal[, "openDateQtr" := as.yearqtr(openDateDist)]
  quarterSharesFinal[, "postOpenQtr" := (YQ - openDateQtr) * 4]
  quarterSharesFinal[, "postOpen" := (postOpenQtr >= 0)]
  quarterSharesFinal[is.na(postOpen), "postOpen" := FALSE]
  fwrite(quarterSharesFinal, paste0("/scratch/upenn/hossaine/quarterShares", i,
                                    "Mi.csv"), nThread = threads)
}

#################################################################################
############# RUNNING REGRESSION ON OVERALL BULK PURCHASING ####################
################################################################################
for (i in c(5, 10, 15, 20)) {
  print(i)
  purchData <- fread(paste0("/scratch/upenn/hossaine/quarterShares", i, "Mi.csv"),
                            nThread = threads)
  purchData[, "hhMarket" := paste(household_code, zip_code, sep = "_")]

  # Cleaning up groups. If household entered right when warehouse club entered, they
  # are not a treatment group
  purchData[, c("postOpen", "group") := NULL]
  purchData[, "postOpen" := (postOpenQtr > 0)]
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
                                       men, women, age, nChildren, group, college,
                                       postOpenQtr, postOpen, hhMarket,
                                       openDateQtr)]

  quarterSharesClub <- purchData[, .(bulk = weighted.mean(bulk, w = totalExp)),
                                 by = .(household_code, panel_year, quarter, club)]

  # Filling in zeros for households that do not shop at clubs
  fillDat <- unique(quarterSharesClub[, .(household_code, panel_year, quarter)])
  fillDat[, "hh_yr_qtr" := paste(household_code, panel_year, quarter, sep = "_")]
  toFill <- setDT(expand.grid(hh_yr_qtr = fillDat$hh_yr_qtr, club = 0:1))
  toFill[, c("household_code", "panel_year", "quarter") := tstrsplit(hh_yr_qtr, "_", fixed = TRUE)]
  toFill[, "hh_yr_qtr" := NULL]
  toFill <- toFill[, lapply(.SD, as.integer)]
  quarterSharesClub <- merge(quarterSharesClub, toFill, all.y = TRUE,
                             by = c("household_code", "panel_year", "quarter", "club"))
  quarterSharesClub[is.na(bulk), "bulk" := 0]

  # Adding demographics
  panel <- unique(purchData[, .(household_code, panel_year, quarter, YQ,
                                married, household_income_coarse,
                                men, women, age, nChildren, group, college,
                                postOpenQtr, postOpen, hhMarket, openDateQtr)])
  quarterSharesClub <- merge(quarterSharesClub, panel,
                             by = c("household_code", "panel_year", "quarter"))

  # Running diff-in-diff on treatment and never club group. I ignore whether they
  # are a club shopper or not. This should be a lower bound on the effect
  quarterSharesAll[, "postOpen25" := ifelse(group == "Treatment",
                                            postOpen * (household_income_coarse == "<25k"), 0L)]
  quarterSharesAll[, "postOpen2550" := ifelse(group == "Treatment",
                                              postOpen * (household_income_coarse == "25-50k"), 0L)]
  quarterSharesAll[, "postOpen50100" := ifelse(group == "Treatment",
                                               postOpen * (household_income_coarse == "50-100k"), 0L)]
  quarterSharesAll[, "postOpen100" := ifelse(group == "Treatment",
                                             postOpen * (household_income_coarse == ">100k"), 0L)]

  reg1 <- felm(bulk ~ postOpen | hhMarket + YQ | 0 | household_code,
               data = quarterSharesAll)
  reg2 <- felm(bulk ~ postOpen + men + women + nChildren + married + age +
                 college + household_income_coarse |
                 hhMarket + YQ | 0 | household_code, data = quarterSharesAll)
  reg3 <- felm(bulk ~ postOpen25 + postOpen2550 + postOpen50100 + postOpen100 +
                 men + women + nChildren + married + age + college +
                 household_income_coarse | hhMarket + YQ | 0 |
                 household_code, data = quarterSharesAll)

  mean1 <- round(quarterSharesAll[, mean(bulk)], 2)

  stargazer(reg1, reg2, reg3, type = "text",
            add.lines = list(c("Avg Bulk", rep(mean1, 3)),
                             c("Household-ZIP FE's", "Y", "Y", "Y"),
                             c("Year-Quarter FE's", "Y", "Y", "Y"),
                             c("Demographic Controls", "N", "Y", "Y")),
            single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
            out.header = FALSE,
            dep.var.caption = "", dep.var.labels.include = FALSE,
            keep = c("postOpen*"),
            covariate.labels = c("Post-Entry", "Post-Entry : <25k",
                                 "Post-Entry : 25-50k", "Post-Entry : 50-100k",
                                 "Post-Entry : >100k"),
            notes.align = "l",
            notes.append = TRUE,
            digits = 3,
            out = paste0("tables/costcoEntryDD", i, "Mi.tex"))

  # Checking various margins (bulk buying at non-club stores)
  reg1 <- felm(bulk ~ postOpen | hhMarket + YQ | 0 | household_code,
               data = quarterSharesClub[club == 0])
  reg2 <- felm(bulk ~ postOpen + men + women + nChildren + married + age +
                 college + household_income_coarse |
                 hhMarket + YQ | 0 | household_code,
               data = quarterSharesClub[club == 0])
  reg3 <- felm(bulk ~ postOpen * household_income_coarse + men + women +
                 nChildren + married + age + college | hhMarket + YQ | 0 |
                 household_code, data = quarterSharesClub[club == 0])

  # Checking various margins (bulk buying at club stores, including zeros)
  reg4 <- felm(bulk ~ postOpen | hhMarket + YQ | 0 | household_code,
               data = quarterSharesClub[club == 1])
  reg5 <- felm(bulk ~ postOpen + men + women + nChildren + married + age +
                 college + household_income_coarse |
                 hhMarket + YQ | 0 | household_code,
               data = quarterSharesClub[club == 1])
  reg6 <- felm(bulk ~ postOpen * household_income_coarse + men + women +
                 nChildren + married + age + college | hhMarket + YQ | 0 |
                 household_code, data = quarterSharesClub[club == 1])

  # Checking various margins (bulk buying at club stores, only positive spending)
  reg7 <- felm(bulk ~ postOpen | hhMarket + YQ | 0 | household_code,
               data = quarterSharesClub[club == 1 & bulk > 0])
  reg8 <- felm(bulk ~ postOpen + men + women + nChildren + married + age +
                 college + household_income_coarse |
                 hhMarket + YQ | 0 | household_code,
               data = quarterSharesClub[club == 1 & bulk > 0])
  reg9 <- felm(bulk ~ postOpen * household_income_coarse + men + women +
                 nChildren + married + age + college | hhMarket + YQ | 0 |
                 household_code, data = quarterSharesClub[club == 1 & bulk > 0])

  mean1 <- round(quarterSharesClub[club == 0, mean(bulk)], 2)
  mean2 <- round(quarterSharesClub[club == 1, mean(bulk)], 2)
  mean3 <- round(quarterSharesClub[club == 1 & bulk > 0, mean(bulk)], 2)

  stargazer(reg3, reg6, reg9, type = "text",
            add.lines = list(c("Avg Bulk", mean1, mean2, mean3),
                             c("Household-ZIP FE's", "Y", "Y", "Y"),
                             c("Year-Quarter FE's", "Y", "Y", "Y"),
                             c("Demographic Controls", "Y", "Y", "Y")),
            single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
            out.header = FALSE,
            dep.var.caption = "", dep.var.labels.include = FALSE,
            order = c(1, 9, 10, 8),
            keep = c("postOpen*"),
            column.labels = c("Non-Club", "Club", "Club Intensive"),
            covariate.labels = c("Post-Entry", "Post-Entry : 25-50k",
                                 "Post-Entry : 50-100k", "Post-Entry : >100k"),
            notes.align = "l",
            notes.append = TRUE,
            digits = 3,
            out = paste0("tables/costcoEntryDD", i, "MiClubStoresExtIntMargins.tex"))

  # Getting number of households that switch to club buying post-entry
  apple <- quarterSharesClub[club == 1, .(clubTrips = sum(bulkInd)),
                             by = .(household_code, group, postOpen)]

  # Checking various margins (probability of shopping at club stores)
  quarterSharesClub[, "bulkInd" := (bulk > 0)]
  reg1 <- felm(bulkInd ~ postOpen | hhMarket + YQ | 0 | household_code,
               data = quarterSharesClub[club == 1])
  reg2 <- felm(bulkInd ~ postOpen + men + women + nChildren + married + age +
                 college + household_income_coarse |
                 hhMarket + YQ | 0 | household_code,
               data = quarterSharesClub[club == 1])
  reg3 <- felm(bulkInd ~ postOpen * household_income_coarse + men + women +
                 nChildren + married + age + college | hhMarket + YQ | 0 |
                 household_code, data = quarterSharesClub[club == 1])

  mean1 <- round(quarterSharesClub[club == 1, mean((bulk > 0))], 2)

  stargazer(reg1, reg2, reg3, type = "text",
            add.lines = list(c("Avg Bulk", rep(mean1, 3)),
                             c("Household-ZIP FE's", "Y", "Y", "Y"),
                             c("Year-Quarter FE's", "Y", "Y", "Y"),
                             c("Demographic Controls", "N", "Y", "Y")),
            single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
            out.header = FALSE,
            dep.var.caption = "", dep.var.labels.include = FALSE,
            order = c(1, 9, 10, 8),
            keep = c("postOpen*"),
            covariate.labels = c("Post-Entry", "Post-Entry : 25-50k",
                                 "Post-Entry : 50-100k", "Post-Entry : >100k"),
            notes.align = "l",
            notes.append = TRUE,
            digits = 3,
            out = paste0("tables/costcoEntryDD", i, "MiClubStoresIndicator.tex"))

  # Checking for pre-trends
  quarterSharesAll[, "qtrEntry" := as.character((YQ - openDateQtr) * 4)]
  quarterSharesAll[is.na(qtrEntry), "qtrEntry" := "No Entry"]
  quarterSharesAll[, "qtrEntry" := relevel(as.factor(qtrEntry), ref = "No Entry")]
  quarterSharesAll[, "open" := (qtrEntry == "No Entry")]
  quarterSharesAll[qtrEntry == "-1", "qtrEntry" := "No Entry"]

  reg1 <- felm(bulk ~ qtrEntry | hhMarket + YQ | 0 | household_code,
               data = quarterSharesAll)
  reg2 <- felm(bulk ~ qtrEntry + men + women + nChildren + married + age +
                 college + household_income_coarse | hhMarket + YQ | 0 |
                 household_code, data = quarterSharesAll)
  stargazer(reg1, reg2, type = "text")

  # Plotting pre-trends
  graphData <- as.data.table(summary(reg2)$coefficients, keep.rownames = TRUE)
  confInt <- as.data.table(confint(reg2), keep.rownames = TRUE)
  graphData <- merge(graphData, confInt, by = "rn")
  graphData <- graphData[grepl("qtrEntry*", rn)]
  graphData[, "rn" := as.integer(gsub("qtrEntry", "", rn))]
  graphData <- rbindlist(list(graphData, list(-1, 0, 0, 0, 0, 0, 0)))

  setnames(graphData, c("quarter", "betas", "se", "t", "p", "LCL", "UCL"))
  ggplot(graphData[quarter %between% c(-8, 8)], aes(x = quarter, y = betas)) +
    geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.2) +
    geom_point() +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    labs(x = "Quarters After Entry",
         y = "Change in Bulk Purchasing (pp)") +
    theme_tufte() +
    theme(axis.title = element_text(),
          legend.position = "bottom")
  ggsave(filename = paste0("./figures/eventStudy", i, "Mi.pdf"), height = 4, width = 6)

  # Checking pre-trends by income group
  quarterSharesAll[household_income_coarse == "<25k", "qtrEntry0025" := qtrEntry]
  quarterSharesAll[is.na(qtrEntry0025), "qtrEntry0025" := "No Entry"]
  quarterSharesAll[household_income_coarse == "25-50k", "qtrEntry2550" := qtrEntry]
  quarterSharesAll[is.na(qtrEntry2550), "qtrEntry2550" := "No Entry"]
  quarterSharesAll[household_income_coarse == "50-100k", "qtrEntry5010" := qtrEntry]
  quarterSharesAll[is.na(qtrEntry5010), "qtrEntry5010" := "No Entry"]
  quarterSharesAll[household_income_coarse == ">100k", "qtrEntry100k" := qtrEntry]
  quarterSharesAll[is.na(qtrEntry100k), "qtrEntry100k" := "No Entry"]

  reg3 <- felm(bulk ~ qtrEntry0025 + qtrEntry2550 + qtrEntry5010 + qtrEntry100k +
                 men + women + nChildren + married + age + college +
                 household_income_coarse | hhMarket + YQ | 0 |
                 household_code, data = quarterSharesAll)
  stargazer(reg3, type = "text")

  # Plotting Event study by income
  graphData <- as.data.table(summary(reg3)$coefficients, keep.rownames = TRUE)
  confInt <- as.data.table(confint(reg3), keep.rownames = TRUE)
  graphData <- merge(graphData, confInt, by = "rn")
  graphData <- graphData[grepl("qtrEntry*", rn)]
  graphData[, "rn" := gsub("qtrEntry", "", rn)]
  graphData[, "income" := substr(rn, 1, 4)]
  graphData[income == "100k", "incomeBin" := ">100k"]
  graphData[income == "5010", "incomeBin" := "50-100k"]
  graphData[income == "2550", "incomeBin" := "25-50k"]
  graphData[is.na(incomeBin), "incomeBin" := "<25k"]
  graphData[, "quarter" := as.integer(substring(rn, 5, nchar(rn)))]
  graphData[, c("income", "rn") := NULL]
  graphData <- rbindlist(list(graphData,
                              list(0, 0, 0, 0, 0, 0, "<25k", -1),
                              list(0, 0, 0, 0, 0, 0, "25-50k", -1),
                              list(0, 0, 0, 0, 0, 0, "50-100k", -1),
                              list(0, 0, 0, 0, 0, 0, ">100k", -1)))

  setnames(graphData, c("betas", "se", "t", "p", "LCL", "UCL", "income", "quarter"))
  graphData[, "income" := factor(income, levels = c("<25k", "25-50k", "50-100k", ">100k"),
                                 ordered = TRUE)]
  ggplot(graphData[quarter %between% c(-8, 8)], aes(x = quarter, y = betas)) +
    geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.2) +
    geom_point() +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    facet_wrap(vars(income)) +
    labs(x = "Quarters After Entry",
         y = "Change in Bulk Purchasing\n(Percentage Points)") +
    theme_tufte() +
    theme(axis.title = element_text(),
          legend.position = "bottom",
          text = element_text(size = 14),
          axis.ticks.length = unit(0.25, "cm"))
  ggsave(filename = paste0("./figures/eventStudyIncome", i, "Mi.pdf"), height = 4, width = 6)
}

################################################################################
# Making appendix table of different radii
################################################################################
fullReg <- NULL
for (i in c("5Mi", "10Mi", "15Mi", "20Mi")) {
  purchData <- fread(paste0("/scratch/upenn/hossaine/quarterShares", i, ".csv"),
                     nThread = threads)
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
                                       men, women, age, nChildren, group, college,
                                       postOpenQtr, postOpen, hhMarket)]

  # Running diff-in-diff on treatment and never club group. I ignore whether they
  # are a club shopper or not. This should be a lower bound on the effect
  quarterSharesAll[, "postOpen25" := ifelse(group == "Treatment",
                                            postOpen * (household_income_coarse == "<25k"), 0L)]
  quarterSharesAll[, "postOpen2550" := ifelse(group == "Treatment",
                                              postOpen * (household_income_coarse == "25-50k"), 0L)]
  quarterSharesAll[, "postOpen50100" := ifelse(group == "Treatment",
                                               postOpen * (household_income_coarse == "50-100k"), 0L)]
  quarterSharesAll[, "postOpen100" := ifelse(group == "Treatment",
                                             postOpen * (household_income_coarse == ">100k"), 0L)]

  reg4 <- felm(bulk ~ postOpen25 + postOpen2550 + postOpen50100 + postOpen100 +
                 men + women + nChildren + married + age + college +
                 household_income_coarse | hhMarket + YQ | 0 |
                 household_code, data = quarterSharesAll)
  fullReg[[i]] <- reg4
}

stargazer(fullReg, type = "text",
          add.lines = list(c("Household-ZIP FE's", "Y", "Y", "Y", "Y"),
                           c("Year-Qtr FE's", "Y", "Y", "Y", "Y"),
                           c("Demographic Controls", "Y", "Y", "Y", "Y")),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("5 Mi", "10 Mi", "15 Mi", "20 Mi"),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          keep = c("postOpen*"),
          covariate.labels = c("Post-Entry", "Post-Entry : 25-50k",
                               "Post-Entry : 50-100k", "Post-Entry : >100k"),
          notes.align = "l",
          notes.append = TRUE,
          digits = 3,
          out = "tables/appendixCostcoEntryDifferentRadii.tex")
