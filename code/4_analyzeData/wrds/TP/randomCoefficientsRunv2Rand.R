# Step 1: Estimates mlogit model for random coefficients
# Step 2: Computes willingness to pay table
# Step 3: Estimates counterfactuals:
# Counterfactuals:
#     (a) Introduce Charmin forever roll
#     (b) Introduce "bulk" sizes (36 rolls)
#     (c) Remove "storage" by mapping all 24 roll packs to 12 roll packs
#     (d) Linear prices using 4-roll packs as base
#     (e) Linear prices using 24-roll packs as base
#     (f) Change size preferences to rich households
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

# fwrite(tp[, .(household_code, panel_year, trip_code_uc, stdPrice, choice, dma_cd,
#               brandRollSheet, household_income_coarse, household_size, married)],
#        "/scratch/upenn/hossaine/toCopy.csv", nThread = threads)
#
# # scp hossaine@wrds-cloud.wharton.upenn.edu:/scratch/upenn/hossaine/toCopy.csv /home/mallick/Downloads
# tp <- fread("/home/mallick/Downloads/toCopy.csv", nThread = threads)
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
                brand_descr, rolls, sheets, projection_factor)]

# Coding package sizes and brands
tp[, "large6" := (rolls > 6)]
tp[, "large12" := (rolls > 12)]
tp[, "brand_descr" := relevel(as.factor(brand_descr), ref = "SCOTT 1000")]
tp[, "sheetPP" := sheets / household_size]
tp[, "logSheetPP" := log(sheetPP)]

# Running in parallel on income and years
registerDoParallel()
getDoParWorkers()

r <- foreach(i = c(2006:2016)) %:%
  # Running MNL model
  foreach(incBin = c("<25k", "25-50k", "50-100k", ">100k")) %dopar% {
    print(c(i, incBin))
    # Creating mlogit data for analysis
    tpML <- mlogit.data(tp[panel_year == i],
                        choice = "choice", shape = "long", alt.var = "brandRollSheet",
                        chid.var = "trip_code_uc", id.var = "household_code",
                        opposite = c("price", "large12"))
    reg <- mlogit(choice ~ price + brand_descr + logSheetPP + large12 + 0,
                  data = tpML[tpML$household_income_coarse == incBin, ],
                  weights = tpML[tpML$household_income_coarse == incBin, ]$projection_factor,
                  rpar = c(price = "n", logSheetPP = "n", large12 = "n"),
                  R = 25, halton = NA, panel = TRUE)
    save(reg, file = paste0("/scratch/upenn/hossaine/mlogit/incYearRand/mlogit", incBin, i, ".rda"))
  }
save(r, file = paste0("/scratch/upenn/hossaine/mlogitFullIncYearRand.rda"), compress = TRUE)

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
    save(reg, file = paste0("/scratch/upenn/hossaine/mlogit/incYearMktRand/mlogit",
                            incBin, i, j, ".rda"))
  }
save(r, file = paste0("/scratch/upenn/hossaine/mlogitFullIncYearMktRand.rda"), compress = TRUE)


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

# Generating WTP table computing standard errors using the delta method
combineTable <- function(yr, income) {
  print(paste(yr, income))
  load(paste0("/home/upenn/hossaine/Nielsen/mlogit/incYearRand/mlogit", income, yr, ".rda"))
  dt <- getTable(reg)
  dt[, "year" := yr]
  dt[, "income" := income]
  return(dt)
}
mapArgs <- expand.grid(year = 2006, income = c("<25k", "25-50k", "50-100k", ">100k"))
finalTable <- rbindlist(map2(mapArgs$year, mapArgs$income, combineTable), use.names = TRUE)


for (i in 2006:2016) {
  load(paste0("/home/upenn/hossaine/Nielsen/logitResults/mlogit", i, ".rda"))
  finalTable <- rbindlist(map(r, getTable), use.names = TRUE)
  finalTable[, "income" := c(rep("<25k", 7), rep("25-50k", 7),
                             rep("50-100k", 7), rep(">100k", 7))]
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
  finalTable[, "coef" := gsub("sheetPP", "Sheets Per Person", coef)]
  finalTable[, "coef" := gsub("large12TRUE", "Large", coef)]

  finalTableWide <- dcast(finalTable, coef ~ income,
                          value.var = c("WTPPrint", "SEPrint"))
  setcolorder(finalTableWide, c(1, 4, 8, 2, 6, 3, 7, 5, 9))
  stargazer(finalTableWide, summary = FALSE, type = "text", rownames = FALSE,
            out = paste0("/home/upenn/hossaine/tables/mlogit", i, ".tex"))
}

################################################################################
################ STEP 3: Counterfactual 1: PREDICTING MARKET SHARES of Charmin Forever
################################################################################
yr <- 2006
load(paste0("/home/upenn/hossaine/Nielsen/logitResults/mlogit", yr, ".rda"))

# Getting actual market shares of each product by income group
ms1 <- as.data.table(prop.table(summary(r[[1]])$freq), keep.rownames = TRUE)
ms1[, "income" := "<25k"]
ms2 <- as.data.table(prop.table(summary(r[[2]])$freq), keep.rownames = TRUE)
ms2[, "income" := "25-50k"]
ms3 <- as.data.table(prop.table(summary(r[[3]])$freq), keep.rownames = TRUE)
ms3[, "income" := "50-100k"]
ms4 <- as.data.table(prop.table(summary(r[[4]])$freq), keep.rownames = TRUE)
ms4[, "income" := ">100k"]
ms <- rbindlist(list(ms1, ms2, ms3, ms4), use.names = TRUE)
setnames(ms, c("brandRollSheet", "actualShare", "income"))

# Getting predicted market shares of each product by income group
ps1 <- as.data.table(apply(fitted(r[[1]], outcome = FALSE), 2, mean), keep.rownames = TRUE)
ps1[, "income" := "<25k"]
ps2 <- as.data.table(apply(fitted(r[[2]], outcome = FALSE), 2, mean), keep.rownames = TRUE)
ps2[, "income" := "25-50k"]
ps3 <- as.data.table(apply(fitted(r[[3]], outcome = FALSE), 2, mean), keep.rownames = TRUE)
ps3[, "income" := "50-100k"]
ps4 <- as.data.table(apply(fitted(r[[4]], outcome = FALSE), 2, mean), keep.rownames = TRUE)
ps4[, "income" := ">100k"]
ps <- rbindlist(list(ps1, ps2, ps3, ps4), use.names = TRUE)
setnames(ps, c("brandRollSheet", "predictedShare", "income"))

# Generating forever rolls
# 1-pack
forever1 <- unique(tp[panel_year == yr,
                      .(household_code, panel_year, trip_code_uc, dma_cd,
                        household_income_coarse, household_size, married,
                        projection_factor)])
forever1[, ':=' (brandRollSheet = "CHARMIN_1_1700",
                 price = 9.99)]

# 2-pack
forever2 <- unique(tp[panel_year == yr,
                      .(household_code, panel_year, trip_code_uc, dma_cd,
                        household_income_coarse, household_size, married,
                        projection_factor)])
forever2[, ':=' (brandRollSheet = "CHARMIN_2_3400",
                 price = 10.98)]

# 4-pack
forever4 <- unique(tp[panel_year == yr,
                      .(household_code, panel_year, trip_code_uc, dma_cd,
                        household_income_coarse, household_size, married,
                        projection_factor)])
forever4[, ':=' (brandRollSheet = "CHARMIN_4_6800",
                 price = 31.97)]

# 12-pack
forever12 <- unique(tp[panel_year == yr,
                       .(household_code, panel_year, trip_code_uc, dma_cd,
                         household_income_coarse, household_size, married,
                         projection_factor)])
forever12[, ':=' (brandRollSheet = "CHARMIN_12_20400",
                  price = 71.93)]

forever <- rbindlist(list(forever1, forever2, forever4, forever12), use.names = TRUE)

forever[, c("brand_descr", "rolls", "sheets") := tstrsplit(brandRollSheet, "_", fixed = TRUE)]
forever[, "rolls" := as.integer(rolls)]
forever[, "sheets" := as.integer(sheets)]
forever[, "large6" := (rolls > 6)]
forever[, "large12" := (rolls > 12)]
forever[, "sheetPP" := sheets / household_size]
forever[, "logSheetPP" := log(sheetPP)]
forever[, "choice" := 0]

# Adding to choice data
newTP <- rbindlist(list(tp[panel_year == yr], forever), use.names = TRUE)

# Generating coefficients
inc25 <- coef(r[[1]])
inc2550 <- coef(r[[2]])
inc50100 <- coef(r[[3]])
inc100 <- coef(r[[4]])

newTP[household_income_coarse == "<25k",
      ':=' (priceCoef = inc25[1],
            asCoef = inc25[2],
            charCoef = inc25[3],
            ctlCoef = inc25[4],
            cottonCoef = inc25[5],
            qnCoef = inc25[6],
            sheetCoef = inc25[7],
            largeCoef = inc25[8])]

newTP[household_income_coarse == "25-50k",
      ':=' (priceCoef = inc2550[1],
            asCoef = inc2550[2],
            charCoef = inc2550[3],
            ctlCoef = inc2550[4],
            cottonCoef = inc2550[5],
            qnCoef = inc2550[6],
            sheetCoef = inc2550[7],
            largeCoef = inc2550[8])]

newTP[household_income_coarse == "50-100k",
      ':=' (priceCoef = inc50100[1],
            asCoef = inc50100[2],
            charCoef = inc50100[3],
            ctlCoef = inc50100[4],
            cottonCoef = inc50100[5],
            qnCoef = inc50100[6],
            sheetCoef = inc50100[7],
            largeCoef = inc50100[8])]

newTP[household_income_coarse == ">100k",
      ':=' (priceCoef = inc100[1],
            asCoef = inc100[2],
            charCoef = inc100[3],
            ctlCoef = inc100[4],
            cottonCoef = inc100[5],
            qnCoef = inc100[6],
            sheetCoef = inc100[7],
            largeCoef = inc100[8])]

# Getting numerators
newTP[brand_descr == "CHARMIN", "eX" := exp(price * priceCoef + charCoef +
                                              logSheetPP * sheetCoef + large12 * largeCoef)]
newTP[brand_descr == "ANGEL SOFT", "eX" := exp(price * priceCoef + asCoef +
                                                 logSheetPP * sheetCoef + large12 * largeCoef)]
newTP[brand_descr == "CTL BR", "eX" := exp(price * priceCoef + ctlCoef +
                                             logSheetPP * sheetCoef + large12 * largeCoef)]
newTP[brand_descr == "KLEENEX COTTONELLE", "eX" := exp(price * priceCoef + cottonCoef +
                                                         logSheetPP * sheetCoef + large12 * largeCoef)]
newTP[brand_descr == "QUILTED NORTHERN", "eX" := exp(price * priceCoef + qnCoef +
                                                       logSheetPP * sheetCoef + large12 * largeCoef)]
newTP[brand_descr == "SCOTT 1000", "eX" := exp(price * priceCoef +
                                                 logSheetPP * sheetCoef + large12 * largeCoef)]

# Computing probabilities
newTP[, "prob" := eX / sum(eX), by = .(trip_code_uc)]

# Expanding grid to get proper mean probabilities
tripID <- unique(newTP$trip_code_uc)
brandID <- unique(newTP$brandRollSheet)
finalProbs <- as.data.table(expand.grid(trip_code_uc = tripID,
                                        brandRollSheet = brandID))
finalProbs <- merge(finalProbs, newTP[, .(trip_code_uc, brandRollSheet, prob)],
                    by = c("trip_code_uc", "brandRollSheet"), all.x = TRUE)
finalProbs[is.na(prob), "prob" := 0]
finalProbs <- merge(finalProbs, unique(newTP[, .(trip_code_uc, household_income_coarse)]),
                    by = "trip_code_uc")
probs <- finalProbs[, mean(prob), by = .(brandRollSheet, household_income_coarse)]
setnames(probs, c("brandRollSheet", "income", "foreverShare"))

# Merging actual and predicted shares
# Predictions are off. Some of this is because I omitted alternative-specific
# intercepts since there wasn't a strong intuition for including them.
# However, there are some severe underpredictions for popular products
# so I'll have to look into that later.
sharePredictions <- merge(ms, ps, by = c("brandRollSheet", "income"), all = TRUE)
sharePredictions <- merge(sharePredictions, probs, by = c("brandRollSheet", "income"),
                          all = TRUE)
sharePredictions[brandRollSheet == "CHARMIN_1_1700"]
sharePredictions[brandRollSheet == "CHARMIN_2_3400"]
sharePredictions[brandRollSheet == "CHARMIN_4_6800"]
sharePredictions[brandRollSheet == "CHARMIN_12_20400"]

# Splitting up to look at sheet-roll market shares
sharePredictions[, c("brand_descr", "rolls", "sheets") :=
                   tstrsplit(brandRollSheet, "_", fixed = TRUE)]
sharePredictions[, "rolls" := as.integer(rolls)]
sharePredictions[, "sheets" := as.integer(sheets)]
graphData <- sharePredictions[, .(actualShare = sum(actualShare, na.rm = TRUE),
                                  predictedShare = sum(predictedShare, na.rm = TRUE),
                                  foreverShare = sum(foreverShare, na.rm = TRUE)),
                              by = .(income, rolls)]
graphDataLong <- melt(graphData, id.vars = c("income", "rolls"),
                      variable.name = "shareType", value.name = "share")
ggplot(data = graphDataLong, aes(x = rolls, y = share, fill = shareType)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(vars(income)) +
  theme_fivethirtyeight()


