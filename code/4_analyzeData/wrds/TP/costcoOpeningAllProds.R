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
path <- path <- "/scratch/upenn/hossaine/nielsen_extracts/HMS/"
threads <- 8
distanceKM1 <- 20
distanceKM2 <- 40

# Classifying products as bulk/not and storable/not
storable <- fread(paste0("/scratch/upenn/hossaine/storableClassification.csv"))
retailers <- fread("/scratch/upenn/hossaine/retailers.csv")
panel <- fread("/scratch/upenn/hossaine/fullPanel.csv",
               select = c("panel_year", "fips", "household_code",
                          "projection_factor", "household_income", "household_size",
                          "age", "child", "market", "lat", "lon"))

# Getting products
prod <- na.omit(fread(paste0(path, "Master_Files/Latest/products.tsv"), quote = "",
                      nThread = threads, key = c("upc", "upc_ver_uc")))

# Keeping most common size categories for each module
prod[, "count" := .N, by = .(product_module_code, size1_units)]
prod[, "max" := max(count), by = product_module_code]
prod <- prod[count == max]
prod[, c("count", "max") := NULL]
prod[, "totalAmount" := multi * size1_amount]

# Excluding "deferred" modules per Nielsen's suggestion (while there are 164 of
# these, only 60 are actually in the products file)
deferred <- setDT(read_xlsx("/home/upenn/hossaine/Nielsen/Data/Product_Hierarchy.xlsx"))
deferred <- unique(deferred[, .(product_module_code, `Deferred (Please see documentation for explanation and notes)`)])
setnames(deferred, c("product_module_code", "deferred"))
prod <- merge(prod, deferred, by = "product_module_code")
prod <- prod[is.na(deferred)]
prod[, "deferred" := NULL]

# Excluding all alcohol purchases
prod <- prod[!product_module_code %in% c(5000:5060, 7806)]

# Classifying bulk sizes in product file
quartiles <- prod[, .(cutoff = quantile(totalAmount, c(0.25, 0.5, 0.75, 1))),
                  by = .(product_module_code)]
quartiles[, "quartile" := 1:4]
quarWide <- dcast(data = quartiles, product_module_code ~ quartile, value.var = "cutoff")
setnames(quarWide, c("product_module_code", "q1", "q2", "q3", "q4"))
prod <- merge(prod, quarWide, by = "product_module_code")
rm(quartiles, quarWide)
prod[totalAmount > q3 & totalAmount <= q4, "quartile" := 4L]
prod[totalAmount > q2 & totalAmount <= q3, "quartile" := 3L]
prod[totalAmount > q1 & totalAmount <= q2, "quartile" := 2L]
prod[totalAmount > 0 & totalAmount <= q1, "quartile" := 1L]
prod <- merge(prod, storable, by = c("product_group_code", "product_group_descr"))
prod <- prod[, .(upc, upc_ver_uc, product_module_code, brand_code_uc,
                 totalAmount, size1_units, quartile, storable)]

# Getting monthly bulk and warehouse "share" by household-month
# Getting all purchases by year
monthlyShares <- NULL
for (i in 2004:2017) {
  print(i)
  purch <- fread(paste0(path, i, "/Annual_Files/purchases_", i, ".tsv"), nThread = threads)
  purch <- merge(purch, prod, by = c("upc", "upc_ver_uc"))

  # Only keeping modules with more than 100 recorded purchases
  purch[, "modCount" := .N, by = product_module_code]
  purch <- purch[modCount > 100]

  # Only keeping modules with more than 3 unique sizes purchased
  purch[, "uniqueSizes" := uniqueN(totalAmount), by = product_module_code]
  purch <- purch[uniqueSizes > 3]
  purch[, c("modCount", "uniqueSizes") := NULL]

  purch[, ':=' (generic = ifelse(brand_code_uc == 536746, 1L, 0L),
                price_paid_wCoupon = total_price_paid - coupon_value)]
  purch[, "packagePrice" := price_paid_wCoupon / quantity]
  purch[, c("upc_ver_uc", "quantity", "total_price_paid", "coupon_value", "deal_flag_uc") := NULL]

  # Merging with trips
  trips <- fread(paste0(path, i, "/Annual_Files/trips_", i, ".tsv"), nThread = threads,
                 select = c("trip_code_uc", "household_code", "panel_year",
                            "retailer_code", "purchase_date"))
  purch <- merge(purch, trips, by = "trip_code_uc")
  purch[, "purchase_date" := as.Date(purchase_date)]
  purch[, c("year", "quarter") := .(year(purchase_date), quarter(purchase_date))]
  purch[, "bulk" := ifelse(quartile >= 4, 1L, 0L)]

  # Getting expenditure-weighted generic and bulk shares by month
  purch <- purch[, .(bulk = weighted.mean(bulk, w = price_paid_wCoupon),
                     generic = weighted.mean(generic, w = price_paid_wCoupon)),
                 by = .(household_code, panel_year, year, quarter)]
  purch <- purch[panel_year == year]
  monthlyShares <- rbindlist(list(monthlyShares, purch), use.names = TRUE)
}

monthlyShares <- merge(monthlyShares, panel, by = c("household_code", "panel_year"))
monthlyShares[, "household_income" := factor(household_income)]
monthlyShares[, "YQ" := as.yearqtr(paste(year, quarter, sep = "-"))]
fwrite(monthlyShares, "/scratch/upenn/hossaine/monthlyShares.csv")


# Geocoding households
monthlyShares <- fread("/scratch/upenn/hossaine/monthlyShares.csv")
setorder(monthlyShares, household_code, YQ)
hhGeos <- na.omit(unique(monthlyShares[, .(household_code, lat, lon)]))
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
monthlyShares <- merge(monthlyShares, hhGeos[, .(household_code, lat, lon, distClubKM,
                                                 open_year, open_quarter, clubname)],
                 by = c("household_code", "lat", "lon"))
monthlyShares[, "openYQ" := as.yearqtr(paste(open_year, open_quarter, sep = "-"))]
monthlyShares[, "open" := ifelse(YQ >= openYQ, 1L, 0L)]
monthlyShares[, "nearby1" := (distClubKM <= distanceKM1) * open]
monthlyShares[, "nearby2" := (distClubKM > distanceKM1 & distClubKM <= distanceKM2) * open]
monthlyShares[, "HHMarket" := paste0(household_code, market)]
monthlyShares[, "marketQuarter" := paste0(market, YQ)]
monthlyShares[, "quartersAfterOpen" := (YQ - openYQ) * 4]

# Getting balanced panel for event study
# First getting all households that are in the window of 4 quarters before entry
# and 8 quarters after
# Second, getting all households that did not move in that window.
monthlyShares[, "window" := ifelse(quartersAfterOpen %in% -4:8, 1L, 0L)]
monthlyShares[, "moves" := uniqueN(HHMarket), by = .(household_code, window)]
monthlyShares[, "B" := window * moves]
monthlyShares[B != 1, "B" := 0]

# Running regressions of purchase sizes pre and post opening (coarse income shares)
monthlyShares[household_income %in% 3:13, "household_income_coarse" := "<25k"]
monthlyShares[household_income %in% 15:19, "household_income_coarse" := "25-50k"]
monthlyShares[household_income %in% 21:26, "household_income_coarse" := "50-100k"]
monthlyShares[household_income %in% 27:30, "household_income_coarse" := ">100k"]

reg1 <- felm(data = monthlyShares, bulk ~ nearby1 + nearby2 + household_income_coarse +
               household_size + age + child | HHMarket + marketQuarter | 0 | HHMarket)
reg2 <- felm(data = monthlyShares[household_income_coarse == "<25k"],
             bulk ~ nearby1 + nearby2 + household_size + age + child |
               HHMarket + marketQuarter | 0 | HHMarket)
reg3 <- felm(data = monthlyShares[household_income_coarse == "25-50k"],
             bulk ~ nearby1 + nearby2 + household_size + age + child |
               HHMarket + marketQuarter | 0 | HHMarket)
reg4 <- felm(data = monthlyShares[household_income_coarse == "50-100k"],
             bulk ~ nearby1 + nearby2 + household_size + age + child |
               HHMarket + marketQuarter | 0 | HHMarket)
reg5 <- felm(data = monthlyShares[household_income_coarse == ">100k"],
             bulk ~ nearby1 + nearby2 + household_size + age + child |
               HHMarket + marketQuarter | 0 | HHMarket)
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
          out = "tables/clubOpening.tex")

# Running event study regression
monthlyShares[, "BQ" := ifelse(B == 1, quartersAfterOpen, -99)]
monthlyShares[, "BQ" := relevel(factor(BQ), ref = "-1")]
reg1 <- felm(data = monthlyShares, bulk ~ BQ +
               household_income_coarse +
               household_size + age + child | HHMarket + marketQuarter | 0 | HHMarket)
reg2 <- felm(data = monthlyShares[household_income_coarse == "<25k"], bulk ~ BQ +
               household_size + age + child | HHMarket + marketQuarter | 0 | HHMarket)
reg3 <- felm(data = monthlyShares[household_income_coarse == "25-50k"], bulk ~ BQ +
               household_size + age + child | HHMarket + marketQuarter | 0 | HHMarket)
reg4 <- felm(data = monthlyShares[household_income_coarse == "50-100k"], bulk ~ BQ +
               household_size + age + child | HHMarket + marketQuarter | 0 | HHMarket)
reg5 <- felm(data = monthlyShares[household_income_coarse == ">100k"], bulk ~ BQ +
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
