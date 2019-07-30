# Step 1: Estimates mlogit model
# Step 2: Computes willingness to pay table
# Step 3: Estimates counterfactuals:
# Counterfactuals:
#     (a) Remove storage costs by mapping all packages to 12-roll or smaller
#     (b) Change quantity preferences to rich households
#     (c) Remove storage and change quantity preferences
#     (d) Linear prices using 4-roll packs as base
#     (e) Linear prices using 24-roll packs as base
library(mlogit)
library(data.table)
library(stargazer)
library(purrr)
library(msm)
library(foreach)
library(doParallel)
library(ggplot2)
library(ggthemes)
threads <- 2

################################################################################
############### STEP 1: ESTIMATION #############################################
################################################################################
tp <- unique(fread("/scratch/upenn/hossaine/fullChoiceFinal.csv", nThread = threads))

# Because UPCs might denote insubstantial differences, I define a product as a
# unique brand-roll-size, where I've also collapsed some similar sheet count rolls
# combination (this reduces number of unique products from 907 to 141)
tp[, c("upc", "upc_ver_uc", "brand_descr", "rolls", "sheet", "totalSheet",
       "price", "stdSheet", "stdTotalSheet", "week_end", "store_code_uc") := NULL]

# Getting panel and trips
panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("household_code", "panel_year", "dma_name", "dma_cd",
                          "household_income_coarse", "household_size", "married",
                          "projection_factor"))
tp <- merge(tp, panel, by = c("household_code", "panel_year"))

# Getting product characteristics
tp[, c("brand_descr", "rolls", "sheets") := tstrsplit(brandRollSheet, "_", fixed = TRUE)]
tp[, "rolls" := as.integer(rolls)]
tp[, "sheets" := as.integer(sheets)]

# Assessing how different prices are for cases where there are duplicate
# brand-roll-size combinations. Usually, the prices seem to be the same
tp[, "avgPrice" := mean(stdPrice), by = .(trip_code_uc, brandRollSheet)]
tp[, "priceDiff" := abs(stdPrice - avgPrice)]
round(quantile(tp$priceDiff, seq(0, 1, 0.01)), 2)
tp <- tp[, .(price = mean(stdPrice),
             choice = sum(choice)),
         by = .(household_code, panel_year, trip_code_uc, brandRollSheet,
                dma_cd, household_income_coarse, household_size, married,
                brand_descr, rolls, sheets, projection_factor, FiveFri,
                FiveFriLag, monthStart, hasCredit)]

# Coding package sizes and brands
tp[, "large6" := (rolls > 6)]
tp[, "large12" := (rolls > 12)]
tp[, "brand_descr" := relevel(as.factor(brand_descr), ref = "SCOTT 1000")]
tp[, "sheetPP" := sheets / household_size]
tp[, "logSheetPP" := log(sheetPP)]
tp[, "priceMonthStart" := price * monthStart]

# Running in parallel on income and years
registerDoParallel()
getDoParWorkers()

ids <- sample(unique(tp[panel_year == i]$trip_code_uc), 1000)

r <- foreach(i = c(2006:2016)) %:%
  # Running MNL model
  foreach(incBin = c("<25k", "25-50k", "50-100k", ">100k")) %dopar% {
    print(c(i, incBin))
    # Creating mlogit data for analysis
    tpML <- mlogit.data(tp[panel_year == i & trip_code_uc %in% ids],
                        choice = "choice", shape = "long", alt.var = "brandRollSheet",
                        chid.var = "trip_code_uc", id.var = "household_code")
    reg <- mlogit(choice ~ price + priceMonthStart + brand_descr + logSheetPP + large12 + 0,
                  data = tpML[tpML$household_income_coarse == incBin, ],
                  weights = tpML[tpML$household_income_coarse == incBin, ]$projection_factor)
    save(reg, file = paste0("/scratch/upenn/hossaine/mlogit/incYearWeightLiq/mlogit", incBin, i, ".rda"))
  }
  save(r, file = paste0("/scratch/upenn/hossaine/mlogitFullIncYearWeightLiq.rda"), compress = TRUE)

# Running in parallel for market-income-years
registerDoParallel()
getDoParWorkers()
markets <- unique(tp$dma_cd)

r <- foreach(i = c(2006:2016)) %:%
  foreach(j = markets, .errorhandling = "remove") %:%
  # Running MNL model
  foreach(incBin = c("<25k", "25-50k", "50-100k", ">100k")) %dopar% {
    print(c(i, j, incBin))
    # Creating mlogit data for analysis
    tpML <- mlogit.data(tp[panel_year == i & dma_cd == j],
                        choice = "choice", shape = "long", alt.var = "brandRollSheet",
                        chid.var = "trip_code_uc", id.var = "household_code")
    reg <- mlogit(choice ~ price + brand_descr + logSheetPP + large12 + 0,
                  data = tpML[tpML$household_income_coarse == incBin, ],
                  weights = tpML[tpML$household_income_coarse == incBin, ]$projection_factor)
    save(reg, file = paste0("/scratch/upenn/hossaine/mlogit/incYearMktWeight/mlogit",
                            incBin, i, j, ".rda"))
  }
save(r, file = paste0("/scratch/upenn/hossaine/mlogitFullIncYearMktWeight.rda"), compress = TRUE)


################################################################################
############## STEP 2: WTP TABLE ###############################################
################################################################################
# Function to generate WTP table computing standard errors using the delta method
getTable <- function(reg) {
  wtp1 <- -coef(reg)[-1] / coef(reg)[1]
  wtpSE1 <- deltamethod(list(~ x2 / x1, ~ x3 / x1, ~ x4 / x1, ~ x5 / x1,
                             ~ x6 / x1, ~ x7 / x1, ~ x8 / x1),
                        coef(reg), vcov(reg))

  finalTable <- data.table(coef = names(coef(reg))[-1],
                           WTP = wtp1,
                           SE = wtpSE1)
  return(finalTable)
}

# Generating WTP table for each year and income group
combineTable <- function(yr, income) {
  print(paste(yr, income))
  load(paste0("/home/upenn/hossaine/Nielsen/mlogit/incYear/mlogit", income, yr, ".rda"))
  dt <- getTable(reg)
  dt[, "year" := yr]
  dt[, "income" := income]
  return(dt)
}
mapArgs <- expand.grid(year = 2006:2016, income = c("<25k", "25-50k", "50-100k", ">100k"))
finalTable <- rbindlist(map2(mapArgs$year, mapArgs$income, combineTable), use.names = TRUE)

# Computing t-tests
finalTable[, ':=' (t = WTP / SE,
                   p = 2 * (1 - pnorm(abs(WTP / SE))))]
finalTable[p <= 0.05, "stars" := "*"]
finalTable[p <= 0.01, "stars" := "**"]
finalTable[p <= 0.001, "stars" := "***"]
finalTable[is.na(stars), "stars" := ""]
finalTable[, "WTPPrint" := paste0(round(WTP, 3), stars)]
finalTable[, "SEPrint" := paste0("(", round(SE, 3), ")")]

# Housekeeping and reshaping
finalTable[, "coef" := gsub("brand_descr", "", coef)]
finalTable[, "coef" := gsub("logSheetPP", "Log Sheets Per Person", coef)]
finalTable[, "coef" := gsub("large12TRUE", "Large", coef)]

finalTableWide <- dcast(finalTable, coef + year ~ income, value.var = c("WTPPrint", "SEPrint"))
setcolorder(finalTableWide, c(1, 4, 8, 2, 6, 3, 7, 5, 9))
stargazer(finalTableWide, summary = FALSE, type = "text", rownames = FALSE,
          out = paste0("/home/upenn/hossaine/tables/mlogit", i, ".tex"))

# Plotting WTP over time for each income group and for each variable
ggplot(data = finalTable[coef %in% c("Large", "Log Sheets Per Person")],
       aes(x = as.integer(year), y = WTP, color = income)) +
  geom_line() +
  geom_errorbar(aes(ymin = WTP - 1.96 * SE,
                    ymax = WTP + 1.96 * SE), width = 0.2) +
  geom_point(aes(shape = income), size = 3) +
  geom_hline(yintercept = 0) +
  facet_wrap(vars(coef), scales = "fixed") +
  scale_x_continuous(limits = c(2006, 2016)) +
  labs(title = "Willingness To Pay Over Time",
       x = "Year", y = "Willingness to Pay ($)",
       color = "Income", shape = "Income",
       caption = paste0("Source: Author calulations from Nielsen Consumer Panel. \n",
                        "Note: Figure plots willingness to pay (in nominal dollars) \n",
                        "derived from a multinomial logit model of consumer toilet \n",
                        "paper purchases.")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0)) +
  scale_color_grey()
ggsave(filename = "figures/logitCoefficients.png")

################################################################################
################ STEP 3: Counterfactual Exercise: PREDICTING AVERAGE SHEETS ####
################################################################################
# Load parameters
yr <- 2016
getCoefs <- function(income) {
  load(paste0("/home/upenn/hossaine/Nielsen/mlogit/incYear/mlogit", income, "2016.rda"))
  return(reg)
}
r <- map(c("<25k", "25-50k", "50-100k", ">100k"), getCoefs)
inc25 <- coef(r[[1]])
inc2550 <- coef(r[[2]])
inc50100 <- coef(r[[3]])
inc100 <- coef(r[[4]])

# Getting actual market shares of each product by income group #################
ms1 <- as.data.table(prop.table(summary(r[[1]])$freq), keep.rownames = TRUE)
ms1[, "income" := "<25k"]
ms2 <- as.data.table(prop.table(summary(r[[2]])$freq), keep.rownames = TRUE)
ms2[, "income" := "25-50k"]
ms3 <- as.data.table(prop.table(summary(r[[3]])$freq), keep.rownames = TRUE)
ms3[, "income" := "50-100k"]
ms4 <- as.data.table(prop.table(summary(r[[4]])$freq), keep.rownames = TRUE)
ms4[, "income" := ">100k"]
actualShares <- rbindlist(list(ms1, ms2, ms3, ms4), use.names = TRUE)
setnames(actualShares, c("brandRollSheet", "actualShare", "income"))
actualShares[, c("brand_descr", "rolls", "sheets") :=
               tstrsplit(brandRollSheet, "_", fixed = TRUE)]
actualShares[, "sheets" := as.integer(sheets)]
actualSheets <- actualShares[, .(Data = sum(sheets * actualShare)), by = income]

# Getting predicted market shares of each product by income group ##############
ps1 <- as.data.table(apply(fitted(r[[1]], outcome = FALSE), 2, mean), keep.rownames = TRUE)
ps1[, "income" := "<25k"]
ps2 <- as.data.table(apply(fitted(r[[2]], outcome = FALSE), 2, mean), keep.rownames = TRUE)
ps2[, "income" := "25-50k"]
ps3 <- as.data.table(apply(fitted(r[[3]], outcome = FALSE), 2, mean), keep.rownames = TRUE)
ps3[, "income" := "50-100k"]
ps4 <- as.data.table(apply(fitted(r[[4]], outcome = FALSE), 2, mean), keep.rownames = TRUE)
ps4[, "income" := ">100k"]
baseCase <- rbindlist(list(ps1, ps2, ps3, ps4), use.names = TRUE)
setnames(baseCase, c("brandRollSheet", "predictedShare", "income"))
baseCase[, c("brand_descr", "rolls", "sheets") :=
               tstrsplit(brandRollSheet, "_", fixed = TRUE)]
baseCase[, "sheets" := as.integer(sheets)]
baseCaseSheets <- baseCase[, .(Base = sum(sheets * predictedShare)), by = income]

# Getting estimates without storage costs ######################################
noStorage <- tp[panel_year == yr, ]
noStorage[, "large12" := FALSE]

# Generating coefficients
noStorage[household_income_coarse == "<25k",
      ':=' (priceCoef = inc25[1],
            asCoef = inc25[2],
            charCoef = inc25[3],
            ctlCoef = inc25[4],
            cottonCoef = inc25[5],
            qnCoef = inc25[6],
            sheetCoef = inc25[7],
            largeCoef = inc25[8])]

noStorage[household_income_coarse == "25-50k",
      ':=' (priceCoef = inc2550[1],
            asCoef = inc2550[2],
            charCoef = inc2550[3],
            ctlCoef = inc2550[4],
            cottonCoef = inc2550[5],
            qnCoef = inc2550[6],
            sheetCoef = inc2550[7],
            largeCoef = inc2550[8])]

noStorage[household_income_coarse == "50-100k",
      ':=' (priceCoef = inc50100[1],
            asCoef = inc50100[2],
            charCoef = inc50100[3],
            ctlCoef = inc50100[4],
            cottonCoef = inc50100[5],
            qnCoef = inc50100[6],
            sheetCoef = inc50100[7],
            largeCoef = inc50100[8])]

noStorage[household_income_coarse == ">100k",
      ':=' (priceCoef = inc100[1],
            asCoef = inc100[2],
            charCoef = inc100[3],
            ctlCoef = inc100[4],
            cottonCoef = inc100[5],
            qnCoef = inc100[6],
            sheetCoef = inc100[7],
            largeCoef = inc100[8])]

# Getting numerators
noStorage[brand_descr == "CHARMIN", "eX" := exp(price * priceCoef + charCoef +
                                              logSheetPP * sheetCoef + large12 * largeCoef)]
noStorage[brand_descr == "ANGEL SOFT", "eX" := exp(price * priceCoef + asCoef +
                                                 logSheetPP * sheetCoef + large12 * largeCoef)]
noStorage[brand_descr == "CTL BR", "eX" := exp(price * priceCoef + ctlCoef +
                                             logSheetPP * sheetCoef + large12 * largeCoef)]
noStorage[brand_descr == "KLEENEX COTTONELLE", "eX" := exp(price * priceCoef + cottonCoef +
                                                         logSheetPP * sheetCoef + large12 * largeCoef)]
noStorage[brand_descr == "QUILTED NORTHERN", "eX" := exp(price * priceCoef + qnCoef +
                                                       logSheetPP * sheetCoef + large12 * largeCoef)]
noStorage[brand_descr == "SCOTT 1000", "eX" := exp(price * priceCoef +
                                                 logSheetPP * sheetCoef + large12 * largeCoef)]

# Computing probabilities
noStorage[, "prob" := eX / sum(eX), by = .(trip_code_uc)]

# Expanding grid to get proper mean probabilities
tripID <- unique(noStorage$trip_code_uc)
brandID <- unique(noStorage$brandRollSheet)
finalProbs <- as.data.table(expand.grid(trip_code_uc = tripID,
                                        brandRollSheet = brandID))
finalProbs <- merge(finalProbs, noStorage[, .(trip_code_uc, brandRollSheet, prob)],
                    by = c("trip_code_uc", "brandRollSheet"), all.x = TRUE)
finalProbs[is.na(prob), "prob" := 0]
finalProbs <- merge(finalProbs, unique(noStorage[, .(trip_code_uc, household_income_coarse)]),
                    by = "trip_code_uc")
noStorageCase <- finalProbs[, mean(prob), by = .(brandRollSheet, household_income_coarse)]
setnames(noStorageCase, c("brandRollSheet", "income", "noStorage"))
noStorageCase[, c("brand_descr", "rolls", "sheets") :=
           tstrsplit(brandRollSheet, "_", fixed = TRUE)]
noStorageCase[, "sheets" := as.integer(sheets)]
noStorageSheets <- noStorageCase[, .(`No Storage` = sum(sheets * noStorage)), by = income]

# Same quantity preferences as high-income households ##########################
sameQPref <- tp[panel_year == yr]
sameQPref[household_income_coarse == "<25k",
          ':=' (priceCoef = inc25[1],
                asCoef = inc25[2],
                charCoef = inc25[3],
                ctlCoef = inc25[4],
                cottonCoef = inc25[5],
                qnCoef = inc25[6],
                sheetCoef = inc100[7],
                largeCoef = inc25[8])]

sameQPref[household_income_coarse == "25-50k",
          ':=' (priceCoef = inc2550[1],
                asCoef = inc2550[2],
                charCoef = inc2550[3],
                ctlCoef = inc2550[4],
                cottonCoef = inc2550[5],
                qnCoef = inc2550[6],
                sheetCoef = inc100[7],
                largeCoef = inc2550[8])]

sameQPref[household_income_coarse == "50-100k",
          ':=' (priceCoef = inc50100[1],
                asCoef = inc50100[2],
                charCoef = inc50100[3],
                ctlCoef = inc50100[4],
                cottonCoef = inc50100[5],
                qnCoef = inc50100[6],
                sheetCoef = inc100[7],
                largeCoef = inc50100[8])]

sameQPref[household_income_coarse == ">100k",
          ':=' (priceCoef = inc100[1],
                asCoef = inc100[2],
                charCoef = inc100[3],
                ctlCoef = inc100[4],
                cottonCoef = inc100[5],
                qnCoef = inc100[6],
                sheetCoef = inc100[7],
                largeCoef = inc100[8])]

# Getting numerators
sameQPref[brand_descr == "CHARMIN", "eX" := exp(price * priceCoef + charCoef +
                                                  logSheetPP * sheetCoef + large12 * largeCoef)]
sameQPref[brand_descr == "ANGEL SOFT", "eX" := exp(price * priceCoef + asCoef +
                                                     logSheetPP * sheetCoef + large12 * largeCoef)]
sameQPref[brand_descr == "CTL BR", "eX" := exp(price * priceCoef + ctlCoef +
                                                 logSheetPP * sheetCoef + large12 * largeCoef)]
sameQPref[brand_descr == "KLEENEX COTTONELLE", "eX" := exp(price * priceCoef + cottonCoef +
                                                             logSheetPP * sheetCoef + large12 * largeCoef)]
sameQPref[brand_descr == "QUILTED NORTHERN", "eX" := exp(price * priceCoef + qnCoef +
                                                           logSheetPP * sheetCoef + large12 * largeCoef)]
sameQPref[brand_descr == "SCOTT 1000", "eX" := exp(price * priceCoef +
                                                     logSheetPP * sheetCoef + large12 * largeCoef)]

# Computing probabilities
sameQPref[, "prob" := eX / sum(eX), by = .(trip_code_uc)]

# Expanding grid to get proper mean probabilities
tripID <- unique(sameQPref$trip_code_uc)
brandID <- unique(sameQPref$brandRollSheet)
finalProbs <- as.data.table(expand.grid(trip_code_uc = tripID,
                                        brandRollSheet = brandID))
finalProbs <- merge(finalProbs, sameQPref[, .(trip_code_uc, brandRollSheet, prob)],
                    by = c("trip_code_uc", "brandRollSheet"), all.x = TRUE)
finalProbs[is.na(prob), "prob" := 0]
finalProbs <- merge(finalProbs, unique(sameQPref[, .(trip_code_uc, household_income_coarse)]),
                    by = "trip_code_uc")
sameQPrefCase <- finalProbs[, mean(prob), by = .(brandRollSheet, household_income_coarse)]
setnames(sameQPrefCase, c("brandRollSheet", "income", "sameQPref"))
sameQPrefCase[, c("brand_descr", "rolls", "sheets") :=
                tstrsplit(brandRollSheet, "_", fixed = TRUE)]
sameQPrefCase[, "sheets" := as.integer(sheets)]
sameQPrefSheets <- sameQPrefCase[, .(`Same Quantity Pref.` = sum(sheets * sameQPref)),
                                 by = income]

# Same quantity preferences and no storage costs ##########################
sameQNoStorage <- tp[panel_year == yr]
sameQNoStorage[, "large12" := FALSE]

sameQNoStorage[household_income_coarse == "<25k",
          ':=' (priceCoef = inc25[1],
                asCoef = inc25[2],
                charCoef = inc25[3],
                ctlCoef = inc25[4],
                cottonCoef = inc25[5],
                qnCoef = inc25[6],
                sheetCoef = inc100[7],
                largeCoef = inc25[8])]

sameQNoStorage[household_income_coarse == "25-50k",
          ':=' (priceCoef = inc2550[1],
                asCoef = inc2550[2],
                charCoef = inc2550[3],
                ctlCoef = inc2550[4],
                cottonCoef = inc2550[5],
                qnCoef = inc2550[6],
                sheetCoef = inc100[7],
                largeCoef = inc2550[8])]

sameQNoStorage[household_income_coarse == "50-100k",
          ':=' (priceCoef = inc50100[1],
                asCoef = inc50100[2],
                charCoef = inc50100[3],
                ctlCoef = inc50100[4],
                cottonCoef = inc50100[5],
                qnCoef = inc50100[6],
                sheetCoef = inc100[7],
                largeCoef = inc50100[8])]

sameQNoStorage[household_income_coarse == ">100k",
          ':=' (priceCoef = inc100[1],
                asCoef = inc100[2],
                charCoef = inc100[3],
                ctlCoef = inc100[4],
                cottonCoef = inc100[5],
                qnCoef = inc100[6],
                sheetCoef = inc100[7],
                largeCoef = inc100[8])]

# Getting numerators
sameQNoStorage[brand_descr == "CHARMIN", "eX" := exp(price * priceCoef + charCoef +
                                                  logSheetPP * sheetCoef + large12 * largeCoef)]
sameQNoStorage[brand_descr == "ANGEL SOFT", "eX" := exp(price * priceCoef + asCoef +
                                                     logSheetPP * sheetCoef + large12 * largeCoef)]
sameQNoStorage[brand_descr == "CTL BR", "eX" := exp(price * priceCoef + ctlCoef +
                                                 logSheetPP * sheetCoef + large12 * largeCoef)]
sameQNoStorage[brand_descr == "KLEENEX COTTONELLE", "eX" := exp(price * priceCoef + cottonCoef +
                                                             logSheetPP * sheetCoef + large12 * largeCoef)]
sameQNoStorage[brand_descr == "QUILTED NORTHERN", "eX" := exp(price * priceCoef + qnCoef +
                                                           logSheetPP * sheetCoef + large12 * largeCoef)]
sameQNoStorage[brand_descr == "SCOTT 1000", "eX" := exp(price * priceCoef +
                                                     logSheetPP * sheetCoef + large12 * largeCoef)]

# Computing probabilities
sameQNoStorage[, "prob" := eX / sum(eX), by = .(trip_code_uc)]

# Expanding grid to get proper mean probabilities
tripID <- unique(sameQNoStorage$trip_code_uc)
brandID <- unique(sameQNoStorage$brandRollSheet)
finalProbs <- as.data.table(expand.grid(trip_code_uc = tripID,
                                        brandRollSheet = brandID))
finalProbs <- merge(finalProbs, sameQNoStorage[, .(trip_code_uc, brandRollSheet, prob)],
                    by = c("trip_code_uc", "brandRollSheet"), all.x = TRUE)
finalProbs[is.na(prob), "prob" := 0]
finalProbs <- merge(finalProbs, unique(sameQNoStorage[, .(trip_code_uc, household_income_coarse)]),
                    by = "trip_code_uc")
sameQNoStorageCase <- finalProbs[, mean(prob), by = .(brandRollSheet, household_income_coarse)]
setnames(sameQNoStorageCase, c("brandRollSheet", "income", "sameQNoStorage"))
sameQNoStorageCase[, c("brand_descr", "rolls", "sheets") :=
                tstrsplit(brandRollSheet, "_", fixed = TRUE)]
sameQNoStorageCase[, "sheets" := as.integer(sheets)]
sameQNoStorageSheets <- sameQNoStorageCase[, .(Both = sum(sheets * sameQNoStorage)),
                                 by = income]

# Making final table of estimates
avgSheetTable <- merge(actualSheets, baseCaseSheets, by = "income")
avgSheetTable <- merge(avgSheetTable, noStorageSheets, by = "income")
avgSheetTable <- merge(avgSheetTable, sameQPrefSheets, by = "income")
avgSheetTable <- merge(avgSheetTable, sameQNoStorageSheets, by = "income")
avgSheetTable[, "income" := factor(income, levels = c("<25k", "25-50k", "50-100k", ">100k"),
                                   ordered = TRUE)]
setnames(avgSheetTable, "income", "Income")
setorder(avgSheetTable, Income)
avgSheetTable[, "Data" := NULL]

stargazer(avgSheetTable, summary = FALSE, type = "text", digits = 0)
# Saved in counterfactualMNL.tex
