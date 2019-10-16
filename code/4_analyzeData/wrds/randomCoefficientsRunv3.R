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
library(stringr)
threads <- 8

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
                          "household_income_coarse", "adults", "nChildren",
                          "married", "college", "projection_factor", "law", "age",
                          "type_of_residence"))
panel[, "lawInd" := (law >= 3)]
tp <- merge(tp, panel, by = c("household_code", "panel_year"))

# Getting product characteristics
tp[, c("brand_descr", "rolls", "sheets") := tstrsplit(brandRollSheet, "_", fixed = TRUE)]
tp[, "rolls" := as.integer(rolls)]
tp[, "sheets" := as.integer(sheets)]
tp[, "days" := sheets / (57 * 2)]
tp[, "lDays" := log(days)]

# Assessing how different prices are for cases where there are duplicate
# brand-roll-size combinations. Usually, the prices seem to be the same
# 77% of products have the same price and 95% have price differences of less
# than $0.5
tp[, "avgPrice" := mean(stdPrice), by = .(trip_code_uc, brandRollSheet)]
tp[, "priceDiff" := abs(stdPrice - avgPrice)]
round(quantile(tp$priceDiff, seq(0, 1, 0.01)), 2)
tp <- tp[, .(price = mean(stdPrice),
             choice = sum(choice)),
         by = .(household_code, panel_year, trip_code_uc, brandRollSheet, age,
                dma_cd, household_income_coarse, adults, nChildren, married,
                brand_descr, rolls, sheets, days, projection_factor, college,
                lawInd, lDays, type_of_residence)]
tp[, "unitPrice" := price / days]

# Generating price interactions
tp[, "pReg"      := price * lawInd]

# Generating unit price interactions
tp[, "unitReg"      := unitPrice * lawInd]

# Generating size interactions
tp[, "large"       := (rolls > 12)]
tp[, "largeHome"   := large * (type_of_residence == "Single-Family")]
tp[, "small"       := (rolls < 12)]
tp[, "smallHome"   := small * (type_of_residence == "Single-Family")]

# Coding package sizes and brands
tp[, "brand_descr" := relevel(as.factor(brand_descr), ref = "SCOTT 1000")]

# Running in parallel on years
registerDoParallel()
getDoParWorkers()

# hhInc <- unique(tp[, .(household_code, panel_year, household_income_coarse)])
# ids <- hhInc[panel_year == 2016, .SD[sample(.N, 250)],
#              by = household_income_coarse]$household_code

r <- foreach(i = 2016) %:%
  foreach(incBin = c("<25k", "25-50k", "50-100k", ">100k")) %dopar% {
    print(c(i, incBin))
    # Running MNL model
    # Creating mlogit data for analysis
    tpSub <- tp[panel_year == i]
    tpML <- mlogit.data(tpSub, choice = "choice", shape = "long",
                        alt.var = "brandRollSheet", chid.var = "trip_code_uc")
    reg1 <- mlogit(choice ~ price + 0,
                   data = tpML[tpML$household_income_coarse == incBin, ])
    reg2 <- mlogit(choice ~ price + unitPrice + 0,
                   data = tpML[tpML$household_income_coarse == incBin, ])
    reg3 <- mlogit(choice ~ price + unitPrice + lDays + 0,
                   data = tpML[tpML$household_income_coarse == incBin, ])
    reg4 <- mlogit(choice ~ price + unitPrice + lDays + large + small + 0,
                   data = tpML[tpML$household_income_coarse == incBin, ])
    reg5 <- mlogit(choice ~ price + unitPrice + lDays + large + small + brand_descr + 0,
                   data = tpML[tpML$household_income_coarse == incBin, ])
    reg6 <- mlogit(choice ~ price + pReg +
                     unitPrice + lDays + large + small + brand_descr + 0,
                   data = tpML[tpML$household_income_coarse == incBin, ])
    reg7 <- mlogit(choice ~ price + pReg +
                     unitPrice + unitReg +
                     lDays + large + small + brand_descr + 0,
                   data = tpML[tpML$household_income_coarse == incBin, ])
    reg8 <- mlogit(choice ~ price + pReg +
                     unitPrice + unitReg +
                     lDays + large + largeHome + small + smallHome + brand_descr + 0,
                   data = tpML[tpML$household_income_coarse == incBin, ])
    stargazer(reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8, type = "text")
    print(lrtest(reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8))
    save(reg8, file = paste0("/home/upenn/hossaine/Nielsen/mlogit/newModel2016Only/mlogit",
                             incBin, i, ".rda"), compress = TRUE)
  }

################################################################################
############## STEP 1A: ELASTICITIES ###########################################
################################################################################
fullElast <- NULL
fullCoefs <- NULL
for (i in c("<25k", "25-50k", "50-100k", ">100k")) {
  load(paste0("/home/upenn/hossaine/Nielsen/mlogit/newModel2016Only/mlogit", i, "2016.rda"))
  margs <- effects(reg8, covariate = "price", type = "rr")
  ownElast <- as.data.table(diag(margs), keep.rownames = TRUE)
  ownElast[, "household_income_coarse" := i]
  fullElast <- rbindlist(list(fullElast, ownElast), use.names = TRUE)
  fullCoefs[[i]] <- reg8
}

stargazer(fullCoefs, type = "text",
          add.lines = list(c("Brand FE's", "Y", "Y", "Y", "Y")),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("<25k", "25-50k", "50-100k", ">100k"),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          omit = c("brand_descr*"),
          covariate.labels = c("Total Price", ". : Reg",
                               "Unit Price", ". : Reg",
                               "Log(Days)",
                               "Large Size", ". : Home",
                               "Small Size", ". : Home"),
          notes.align = "l",
          notes.append = TRUE,
          digits = 3,
          out = "tables/mlogit2016.tex")

fullElast[, c("brand_descr", "rolls", "totalSheet") := tstrsplit(V1, "_", fixed = TRUE)]
fullElast[, "V1" := NULL]
setnames(fullElast, "V2", "elast")

fullElast[, "brand_descr" := factor(brand_descr, ordered = TRUE,
                                    levels = c("SCOTT 1000", "CTL BR",
                                               "ANGEL SOFT", "QUILTED NORTHERN",
                                               "KLEENEX COTTONELLE", "CHARMIN"))]
fullElast[, "household_income_coarse" := factor(household_income_coarse, ordered = TRUE,
                                               levels = c("<25k", "25-50k", "50-100k", ">100k"))]
ggplot(data = fullElast, aes(x = elast)) +
  geom_histogram(aes(y = ..density..), bins = 50) +
  geom_hline(yintercept = 0) +
  facet_grid(rows = vars(household_income_coarse)) +
  labs(x = "Elasticity",
       y = "Density") +
  scale_x_continuous(limits = c(-4, 0)) +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom")
ggsave(filename = "./figures/elasticity2016.pdf", height = 4, width = 6)

################################################################################
############## STEP 2: WTP TABLE ###############################################
################################################################################
# Function to generate WTP table computing standard errors using the delta method
getTable <- function(reg) {
  wtp1 <- -coef(reg)[-1] / coef(reg)[1]
  wtpSE1 <- deltamethod(list(~ x2 / x1, ~ x3 / x1, ~ x4 / x1, ~ x5 / x1,
                             ~ x6 / x1, ~ x7 / x1, ~ x8 / x1, ~ x9 / x1,
                             ~ x10 / x1, ~ x11 / x1, ~ x12 / x1),
                        coef(reg), vcov(reg))

  finalTable <- data.table(coef = names(coef(reg))[-1],
                           WTP = wtp1,
                           SE = wtpSE1)
  return(finalTable)
}

# Generating WTP table for each year and income group
combineTable <- function(income) {
  print(paste(income))
  load(paste0("/scratch/upenn/hossaine/mlogit/newModel2016Only/mlogit", income, "2016.rda"))
  dt <- getTable(reg7)
  dt[, "income" := income]
  return(dt)
}
finalTable <- rbindlist(map(c("<25k", "25-50k", "50-100k", ">100k"), combineTable), use.names = TRUE)

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
finalTable <- finalTable[!grepl("brand_descr", coef)]
finalTable[, "coef" := gsub("brand_descr", "", coef)]
finalTable[, "coef" := gsub("largeTRUE", "Large", coef)]
finalTable[, "coef" := gsub("smallTRUE", "Small", coef)]

finalTableWide <- dcast(finalTable, coef ~ income, value.var = c("WTPPrint", "SEPrint"))
setcolorder(finalTableWide, c(1, 4, 8, 2, 6, 3, 7, 5, 9))
stargazer(finalTableWide, summary = FALSE, type = "text", rownames = FALSE,
          out = paste0("/home/upenn/hossaine/tables/mlogitWTP.tex"))

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
  labs(x = "Year", y = "Willingness to Pay ($)",
       color = "Income", shape = "Income") +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom") +
  scale_color_grey() +
  scale_shape_manual(values = c(0:2, 5:7))
ggsave(filename = "figures/logitCoefficients.png", height = 4, width = 6)

################################ Coefficient plots for each year-income-market
# Generating WTP table for each year and income group
mkts <- list.files("/home/upenn/hossaine/mlogit/incYearMktWeight", full.names = TRUE)
combineTable <- function(fileN) {
  print(fileN)
  load(fileN)
  dt <- NULL
  tryCatch({
    dt <- getTable(reg)
    dt[, "year" := str_sub(fileN, -11, -8)]
    dt[, "income" := str_sub(fileN, -18, -12)]
  }, error=function(e){})
  return(dt)
}
finalTable <- rbindlist(map(mkts, combineTable), use.names = TRUE)

# Computing t-tests
finalTable[, "income" := gsub("git", "", income)]
finalTable[, "income" := gsub("it", "", income)]
finalTable[, "income" := gsub("t", "", income)]

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
finalTable[, "WTPSig" := ifelse(p < 0.05, WTP, 0)]
ggplot(data = finalTable[coef %in% c("Large", "Log Sheets Per Person")],
       aes(x = WTPSig, fill = coef)) +
  geom_density() +
  geom_vline(xintercept = 0) +
  facet_grid(cols = vars(year), rows = vars(income)) +
  scale_x_continuous(limits = c(-10, 10)) +
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
ggsave(filename = "figures/logitCoefficientsMkt.png")


################################################################################
################ STEP 3: Counterfactual Exercise: PREDICTING AVERAGE SHEETS ####
################################################################################
# Load parameters
yr <- 2016
getCoefs <- function(income) {
  load(paste0("/scratch/upenn/hossaine/mlogit/newModel2016Only/mlogit", income, "2016.rda"))
  return(reg7)
}
r <- map(c("<25k", "25-50k", "50-100k", ">100k"), getCoefs)

# Getting actual market shares of each product by income group #################
getData <- function(X) {
  as.data.table(prop.table(summary(X)$freq), keep.rownames = TRUE)
}
actualShares <- rbindlist(lapply(r, getData), use.names = TRUE, idcol = "Income")
setnames(actualShares, c("Income", "brandRollSheet", "actualShare"))
actualShares[, "Income" := factor(Income, levels = 1:4, ordered = TRUE,
                                  labels = c("<25k", "25-50k", "50-100k", ">100k"))]
actualShares[, c("brand_descr", "rolls", "sheets") :=
               tstrsplit(brandRollSheet, "_", fixed = TRUE)]
actualShares[, "sheets" := as.integer(sheets)]
actualSheets <- actualShares[, .(Data = sum(sheets * actualShare)), by = Income]

# Function to compute predicted probabilities. Key feature is it changes
# any non-significant coefficients to 0
getProbs <- function(reg, X, beta = NULL) {
  if (is.null(beta)) {
    beta <- as.data.table(summary(reg)$CoefTable)
  }
  beta[`Pr(>|z|)` > 0.05, "Estimate" := 0]
  XB <- X %*% beta$Estimate
  eXB <- exp(XB)
  eXB[is.na(eXB), ] <- 0
  eXBDT <- as.data.table(eXB, keep.rownames = TRUE)
  eXBDT[, c("trip_code_uc", "rn") := tstrsplit(rn, ".", fixed = TRUE)]
  eXBDT[, c("brand_descr", "rolls", "sheets") := tstrsplit(rn, "_", fixed = TRUE)]
  eXBDT[, "prob" := V1 / sum(V1), by = trip_code_uc]
  probs <- eXBDT[, .(predictedShare = mean(prob)), keyby = .(brand_descr, rolls, sheets)]
  probs[, "sheets" := as.integer(sheets)]
  return(probs)
}

################################################################################
############ PREDICTED -> ALL REG -> NO STORAGE COSTS ##########################
################################################################################
# Getting predicted market shares of each product by income group ##############
getPS <- function(X) {
  getProbs(X, model.matrix(X))
}
baseCase <- rbindlist(lapply(r, getPS), use.names = TRUE, idcol = "Income")
baseCase[, "Income" := factor(Income, levels = 1:4, ordered = TRUE,
                              labels = c("<25k", "25-50k", "50-100k", ">100k"))]
baseCaseSheets <- baseCase[, .(Base = sum(sheets * predictedShare)), by = Income]

# Comparing predictions
comp <- merge(actualShares, baseCase, by = c("brand_descr", "rolls", "sheets", "Income"))
comp[, "brandRollSheet" := NULL]
comp[, "diff" := actualShare - predictedShare]
comp[abs(diff) > 0.01]

# Add regulations
getAllRegs <- function(X) {
  allRegs <- model.matrix(X)
  allRegs[, "pReg"] <- allRegs[, "price"]
  allRegs[, "unitReg"] <- allRegs[, "unitPrice"]
  getProbs(X, allRegs)
}
allRegs <- rbindlist(lapply(r, getAllRegs), use.names = TRUE, idcol = "Income")
allRegs[, "Income" := factor(Income, levels = 1:4, ordered = TRUE,
                             labels = c("<25k", "25-50k", "50-100k", ">100k"))]
allRegsSheets <- allRegs[, .(allRegs = sum(predictedShare * sheets)), by = Income]

# Same storage costs
richBeta <- as.data.table(summary(r[[4]])$CoefTable, keep.rownames = TRUE)
richBeta[`Pr(>|z|)` > 0.05, "Estimate" := 0]

getNS <- function(X) {
  betaNew <- as.data.table(summary(X)$CoefTable, keep.rownames = TRUE)
  betaNew[grepl("large*", rn), "Estimate"] <- richBeta[grepl("large*", rn), "Estimate"]
  betaNew[grepl("small*", rn), "Estimate"] <- richBeta[grepl("small*", rn), "Estimate"]
  getProbs(X, model.matrix(X), beta = betaNew)
}
noStorage <- rbindlist(lapply(r, getNS), use.names = TRUE, idcol = "Income")
noStorage[, "Income" := factor(Income, levels = 1:4, ordered = TRUE,
                               labels = c("<25k", "25-50k", "50-100k", ">100k"))]
noStorageSheets <- noStorage[, .(noStorage = sum(predictedShare * sheets)), by = Income]

# All regs and no storage
getNSAR <- function(X) {
  mat <- model.matrix(X)
  mat[, "pReg"] <- mat[, "price"]
  mat[, "unitReg"] <- mat[, "unitPrice"]
  betaNew <- as.data.table(summary(X)$CoefTable, keep.rownames = TRUE)
  betaNew[grepl("large*", rn), "Estimate"] <- richBeta[grepl("large*", rn), "Estimate"]
  betaNew[grepl("small*", rn), "Estimate"] <- richBeta[grepl("small*", rn), "Estimate"]
  getProbs(X, mat, beta = betaNew)
}
noStorageAllRegs <- rbindlist(lapply(r, getNSAR), use.names = TRUE, idcol = "Income")
noStorageAllRegs[, "Income" := factor(Income, levels = 1:4, ordered = TRUE,
                                      labels = c("<25k", "25-50k", "50-100k", ">100k"))]
noStorageAllRegsSheets <- noStorageAllRegs[, .(noStorageAllRegs = sum(predictedShare * sheets)),
                                           by = Income]

# Summary Table
avgSheetTable <- merge(actualSheets, baseCaseSheets, by = "Income")
avgSheetTable <- merge(avgSheetTable, allRegsSheets, by = "Income")
# avgSheetTable <- merge(avgSheetTable, noStorageSheets, by = "Income")
avgSheetTable <- merge(avgSheetTable, noStorageAllRegsSheets, by = "Income")
setorder(avgSheetTable, Income)
stargazer(avgSheetTable, summary = FALSE, type = "text", digits = 0)
# Saving in counterfactualMNLDays.tex
# Save actual and base in modelFit.tex

################################################################################
############ DOLLAR -> ALL REG -> NO STORAGE COSTS #############################
################################################################################
# Getting estimates on dollar store assortments #######################
getStore <- function(storeType) {
  tpData <- fread(paste0("/scratch/upenn/hossaine/tp", storeType, ".csv"))
  tpData[, "brand_descr" := factor(brand_descr,
                                   levels = c("ANGEL SOFT", "CHARMIN", "CTL BR",
                                              "KLEENEX COTTONELLE", "QUILTED NORTHERN",
                                              "SCOTT 1000"))]
  tpData[, "brand_descr" := relevel(brand_descr, ref = "SCOTT 1000")]
  fullMerge <- as.data.table(expand.grid(brandRollSheet = tpData$brandRollSheet,
                                         household_code = unique(tp[panel_year == 2016]$household_code)))
  fullMerge <- merge(fullMerge, tpData, by = "brandRollSheet")
  fullMerge <- merge(fullMerge, unique(tp[panel_year == 2016,
                                          .(household_code, household_income_coarse,
                                            adults, nChildren, married, age,
                                            college, lawInd)]),
                     by = "household_code")

  # Generating price interactions
  fullMerge[, "pReg"      := price * lawInd]

  # Generating unit price interactions
  fullMerge[, "unitReg"      := unitPrice * lawInd]

  # Generating size interactions
  fullMerge[, "large"       := (rolls > 12)]
  fullMerge[, "small"       := (rolls < 12)]

  fullMerge[, "choice" := 0L]
  fullMerge[, "household_income_coarse" := factor(household_income_coarse,
                                                  ordered = TRUE,
                                                  levels = c("<25k", "25-50k",
                                                             "50-100k", ">100k"))]
  return(fullMerge)
}
tpDollar <- getStore("Dollar")

inc25DT <- tpDollar[household_income_coarse == "<25k"]
Xds <- model.matrix(formula(r[[1]]), inc25DT)
rownames(Xds) <- with(inc25DT, paste0(household_code, ".", brandRollSheet))
ds1 <- getProbs(r[[1]], Xds)
ds1[, "Income" := "<25k"]

inc2550DT <- tpDollar[household_income_coarse == "25-50k"]
Xds <- model.matrix(formula(r[[2]]), inc2550DT)
rownames(Xds) <- with(inc2550DT, paste0(household_code, ".", brandRollSheet))
ds2 <- getProbs(r[[2]], Xds)
ds2[, "Income" := "25-50k"]

inc50100DT <- tpDollar[household_income_coarse == "50-100k"]
Xds <- model.matrix(formula(r[[3]]), inc50100DT)
rownames(Xds) <- with(inc50100DT, paste0(household_code, ".", brandRollSheet))
ds3 <- getProbs(r[[3]], Xds)
ds3[, "Income" := "50-100k"]

inc100DT <- tpDollar[household_income_coarse == ">100k"]
Xds <- model.matrix(formula(r[[4]]), inc100DT)
rownames(Xds) <- with(inc100DT, paste0(household_code, ".", brandRollSheet))
ds4 <- getProbs(r[[4]], Xds)
ds4[, "Income" := ">100k"]

dollarAssort <- rbindlist(list(ds1, ds2, ds3, ds4), use.names = TRUE)
dollarAssortSheets <- dollarAssort[, .(dollarAssort = sum(predictedShare * sheets)), by = Income]

# Adding regulations
inc25DT[, "pReg"] <- inc25DT[, "unitPrice"]
Xds <- model.matrix(formula(r[[1]]), inc25DT)
rownames(Xds) <- with(inc25DT, paste0(household_code, ".", brandRollSheet))
ar1 <- getProbs(r[[1]], Xds)
ar1[, "Income" := "<25k"]

inc2550DT[, "pReg"] <- inc2550DT[, "unitPrice"]
Xds <- model.matrix(formula(r[[2]]), inc2550DT)
rownames(Xds) <- with(inc2550DT, paste0(household_code, ".", brandRollSheet))
ar2 <- getProbs(r[[2]], Xds)
ar2[, "Income" := "25-50k"]

inc50100DT[, "pReg"] <- inc50100DT[, "unitPrice"]
Xds <- model.matrix(formula(r[[3]]), inc50100DT)
rownames(Xds) <- with(inc50100DT, paste0(household_code, ".", brandRollSheet))
ar3 <- getProbs(r[[3]], Xds)
ar3[, "Income" := "50-100k"]

inc100DT[, "pReg"] <- inc100DT[, "unitPrice"]
Xds <- model.matrix(formula(r[[4]]), inc100DT)
rownames(Xds) <- with(inc100DT, paste0(household_code, ".", brandRollSheet))
ar4 <- getProbs(r[[4]], Xds)
ar4[, "Income" := ">100k"]

allRegs <- rbindlist(list(ar1, ar2, ar3, ar4), use.names = TRUE)
allRegsSheets <- allRegs[, .(allRegs = sum(predictedShare * sheets)), by = Income]

# Same storage costs
richBeta <- as.data.table(summary(r[[4]])$CoefTable, keep.rownames = TRUE)
richBeta[`Pr(>|z|)` > 0.05, "Estimate" := 0]

beta25 <- as.data.table(summary(r[[1]])$CoefTable, keep.rownames = TRUE)
beta25[grepl("large*", rn), "Estimate"] <- richBeta[grepl("large*", rn), "Estimate"]
Xds <- model.matrix(formula(r[[1]]), inc25DT)
rownames(Xds) <- with(inc25DT, paste0(household_code, ".", brandRollSheet))
ns1 <- getProbs(r[[1]], Xds, beta = beta25)
ns1[, "Income" := "<25k"]

beta2550 <- as.data.table(summary(r[[2]])$CoefTable, keep.rownames = TRUE)
beta2550[grepl("large*", rn), "Estimate"] <- richBeta[grepl("large*", rn), "Estimate"]
Xds <- model.matrix(formula(r[[2]]), inc2550DT)
rownames(Xds) <- with(inc2550DT, paste0(household_code, ".", brandRollSheet))
ns2 <- getProbs(r[[2]], Xds, beta = beta2550)
ns2[, "Income" := "25-50k"]

beta50100 <- as.data.table(summary(r[[3]])$CoefTable, keep.rownames = TRUE)
beta50100[grepl("large*", rn), "Estimate"] <- richBeta[grepl("large*", rn), "Estimate"]
Xds <- model.matrix(formula(r[[3]]), inc50100DT)
rownames(Xds) <- with(inc50100DT, paste0(household_code, ".", brandRollSheet))
ns3 <- getProbs(r[[3]], Xds, beta = beta50100)
ns3[, "Income" := "50-100k"]

beta100 <- as.data.table(summary(r[[4]])$CoefTable, keep.rownames = TRUE)
beta100[grepl("large*", rn), "Estimate"] <- richBeta[grepl("large*", rn), "Estimate"]
Xds <- model.matrix(formula(r[[4]]), inc100DT)
rownames(Xds) <- with(inc100DT, paste0(household_code, ".", brandRollSheet))
ns4 <- getProbs(r[[4]], Xds, beta = beta100)
ns4[, "Income" := ">100k"]

noStorage <- rbindlist(list(ns1, ns2, ns3, ns4), use.names = TRUE)
noStorageSheets <- noStorage[, .(noStorage = sum(predictedShare * sheets)), by = Income]

# All regs and no storage
Xds <- model.matrix(formula(r[[1]]), inc25DT)
Xds[, "pReg"] <- Xds[, "unitPrice"]
rownames(Xds) <- with(inc25DT, paste0(household_code, ".", brandRollSheet))
nsar1 <- getProbs(r[[1]], Xds, beta = beta25)
nsar1[, "Income" := "<25k"]

Xds <- model.matrix(formula(r[[2]]), inc2550DT)
Xds[, "pReg"] <- Xds[, "unitPrice"]
rownames(Xds) <- with(inc2550DT, paste0(household_code, ".", brandRollSheet))
nsar2 <- getProbs(r[[2]], Xds, beta = beta2550)
nsar2[, "Income" := "25-50k"]

Xds <- model.matrix(formula(r[[3]]), inc50100DT)
Xds[, "pReg"] <- Xds[, "unitPrice"]
rownames(Xds) <- with(inc50100DT, paste0(household_code, ".", brandRollSheet))
nsar3 <- getProbs(r[[3]], Xds, beta = beta50100)
nsar3[, "Income" := "50-100k"]

Xds <- model.matrix(formula(r[[4]]), inc100DT)
Xds[, "pReg"] <- Xds[, "unitPrice"]
rownames(Xds) <- with(inc100DT, paste0(household_code, ".", brandRollSheet))
nsar4 <- getProbs(r[[4]], Xds, beta = beta100)
nsar4[, "Income" := ">100k"]

noStorageAllRegs <- rbindlist(list(nsar1, nsar2, nsar3, nsar4), use.names = TRUE)
noStorageAllRegsSheets <- noStorageAllRegs[, .(noStorageAllRegs = sum(predictedShare * sheets)),
                                           by = Income]

# Summary Table
# baseCaseSheets[Income == "25-50k", "Income" := ">100k"]
avgSheetTable <- merge(actualSheets, baseCaseSheets, by = "Income")
avgSheetTable <- merge(avgSheetTable, dollarAssortSheets, by = "Income")
avgSheetTable <- merge(avgSheetTable, allRegsSheets, by = "Income")
#avgSheetTable <- merge(avgSheetTable, noStorageSheets, by = "Income")
avgSheetTable <- merge(avgSheetTable, noStorageAllRegsSheets, by = "Income")
avgSheetTable[, "Income" := factor(Income, ordered = TRUE,
                                   levels = c("<25k", "25-50k", "50-100k", ">100k"))]
setorder(avgSheetTable, Income)
stargazer(avgSheetTable, summary = FALSE, type = "text", digits = 0)

# Saved in counterfactualMNL.tex

################################################################################
############ DRUG -> ALL REG -> NO STORAGE COSTS #############################
################################################################################
# Getting estimates on Drug store assortments #######################
tpDrug <- getStore("Drug")

inc25DT <- tpDrug[household_income_coarse == "<25k"]
Xds <- model.matrix(formula(r[[1]]), inc25DT)
rownames(Xds) <- with(inc25DT, paste0(household_code, ".", brandRollSheet))
ds1 <- getProbs(r[[1]], Xds)
ds1[, "Income" := "<25k"]

inc2550DT <- tpDrug[household_income_coarse == "25-50k"]
Xds <- model.matrix(formula(r[[2]]), inc2550DT)
rownames(Xds) <- with(inc2550DT, paste0(household_code, ".", brandRollSheet))
ds2 <- getProbs(r[[2]], Xds)
ds2[, "Income" := "25-50k"]

inc50100DT <- tpDrug[household_income_coarse == "50-100k"]
Xds <- model.matrix(formula(r[[3]]), inc50100DT)
rownames(Xds) <- with(inc50100DT, paste0(household_code, ".", brandRollSheet))
ds3 <- getProbs(r[[3]], Xds)
ds3[, "Income" := "50-100k"]

inc100DT <- tpDrug[household_income_coarse == ">100k"]
Xds <- model.matrix(formula(r[[4]]), inc100DT)
rownames(Xds) <- with(inc100DT, paste0(household_code, ".", brandRollSheet))
ds4 <- getProbs(r[[4]], Xds)
ds4[, "Income" := ">100k"]

drugAssort <- rbindlist(list(ds1, ds2, ds3, ds4), use.names = TRUE)
drugAssortSheets <- drugAssort[, .(drugAssort = sum(predictedShare * sheets)), by = Income]

# Adding regulations
inc25DT[, "pReg"] <- inc25DT[, "unitPrice"]
Xds <- model.matrix(formula(r[[1]]), inc25DT)
rownames(Xds) <- with(inc25DT, paste0(household_code, ".", brandRollSheet))
ar1 <- getProbs(r[[1]], Xds)
ar1[, "Income" := "<25k"]

inc2550DT[, "pReg"] <- inc2550DT[, "unitPrice"]
Xds <- model.matrix(formula(r[[2]]), inc2550DT)
rownames(Xds) <- with(inc2550DT, paste0(household_code, ".", brandRollSheet))
ar2 <- getProbs(r[[2]], Xds)
ar2[, "Income" := "25-50k"]

inc50100DT[, "pReg"] <- inc50100DT[, "unitPrice"]
Xds <- model.matrix(formula(r[[3]]), inc50100DT)
rownames(Xds) <- with(inc50100DT, paste0(household_code, ".", brandRollSheet))
ar3 <- getProbs(r[[3]], Xds)
ar3[, "Income" := "50-100k"]

inc100DT[, "pReg"] <- inc100DT[, "unitPrice"]
Xds <- model.matrix(formula(r[[4]]), inc100DT)
rownames(Xds) <- with(inc100DT, paste0(household_code, ".", brandRollSheet))
ar4 <- getProbs(r[[4]], Xds)
ar4[, "Income" := ">100k"]

allRegs <- rbindlist(list(ar1, ar2, ar3, ar4), use.names = TRUE)
allRegsSheets <- allRegs[, .(allRegs = sum(predictedShare * sheets)), by = Income]

# Same storage costs
richBeta <- as.data.table(summary(r[[4]])$CoefTable, keep.rownames = TRUE)
richBeta[`Pr(>|z|)` > 0.05, "Estimate" := 0]

beta25 <- as.data.table(summary(r[[1]])$CoefTable, keep.rownames = TRUE)
beta25[grepl("large*", rn), "Estimate"] <- richBeta[grepl("large*", rn), "Estimate"]
Xds <- model.matrix(formula(r[[1]]), inc25DT)
rownames(Xds) <- with(inc25DT, paste0(household_code, ".", brandRollSheet))
ns1 <- getProbs(r[[1]], Xds, beta = beta25)
ns1[, "Income" := "<25k"]

beta2550 <- as.data.table(summary(r[[2]])$CoefTable, keep.rownames = TRUE)
beta2550[grepl("large*", rn), "Estimate"] <- richBeta[grepl("large*", rn), "Estimate"]
Xds <- model.matrix(formula(r[[2]]), inc2550DT)
rownames(Xds) <- with(inc2550DT, paste0(household_code, ".", brandRollSheet))
ns2 <- getProbs(r[[2]], Xds, beta = beta2550)
ns2[, "Income" := "25-50k"]

beta50100 <- as.data.table(summary(r[[3]])$CoefTable, keep.rownames = TRUE)
beta50100[grepl("large*", rn), "Estimate"] <- richBeta[grepl("large*", rn), "Estimate"]
Xds <- model.matrix(formula(r[[3]]), inc50100DT)
rownames(Xds) <- with(inc50100DT, paste0(household_code, ".", brandRollSheet))
ns3 <- getProbs(r[[3]], Xds, beta = beta50100)
ns3[, "Income" := "50-100k"]

beta100 <- as.data.table(summary(r[[4]])$CoefTable, keep.rownames = TRUE)
beta100[grepl("large*", rn), "Estimate"] <- richBeta[grepl("large*", rn), "Estimate"]
Xds <- model.matrix(formula(r[[4]]), inc100DT)
rownames(Xds) <- with(inc100DT, paste0(household_code, ".", brandRollSheet))
ns4 <- getProbs(r[[4]], Xds, beta = beta100)
ns4[, "Income" := ">100k"]

noStorage <- rbindlist(list(ns1, ns2, ns3, ns4), use.names = TRUE)
noStorageSheets <- noStorage[, .(noStorage = sum(predictedShare * sheets)), by = Income]

# All regs and no storage
Xds <- model.matrix(formula(r[[1]]), inc25DT)
Xds[, "pReg"] <- Xds[, "unitPrice"]
rownames(Xds) <- with(inc25DT, paste0(household_code, ".", brandRollSheet))
nsar1 <- getProbs(r[[1]], Xds, beta = beta25)
nsar1[, "Income" := "<25k"]

Xds <- model.matrix(formula(r[[2]]), inc2550DT)
Xds[, "pReg"] <- Xds[, "unitPrice"]
rownames(Xds) <- with(inc2550DT, paste0(household_code, ".", brandRollSheet))
nsar2 <- getProbs(r[[2]], Xds, beta = beta2550)
nsar2[, "Income" := "25-50k"]

Xds <- model.matrix(formula(r[[3]]), inc50100DT)
Xds[, "pReg"] <- Xds[, "unitPrice"]
rownames(Xds) <- with(inc50100DT, paste0(household_code, ".", brandRollSheet))
nsar3 <- getProbs(r[[3]], Xds, beta = beta50100)
nsar3[, "Income" := "50-100k"]

Xds <- model.matrix(formula(r[[4]]), inc100DT)
Xds[, "pReg"] <- Xds[, "unitPrice"]
rownames(Xds) <- with(inc100DT, paste0(household_code, ".", brandRollSheet))
nsar4 <- getProbs(r[[4]], Xds, beta = beta100)
nsar4[, "Income" := ">100k"]

noStorageAllRegs <- rbindlist(list(nsar1, nsar2, nsar3, nsar4), use.names = TRUE)
noStorageAllRegsSheets <- noStorageAllRegs[, .(noStorageAllRegs = sum(predictedShare * sheets)),
                                           by = Income]

# Summary Table
# baseCaseSheets[Income == "25-50k", "Income" := ">100k"]
avgSheetTable <- merge(actualSheets, baseCaseSheets, by = "Income")
avgSheetTable <- merge(avgSheetTable, drugAssortSheets, by = "Income")
avgSheetTable <- merge(avgSheetTable, allRegsSheets, by = "Income")
#avgSheetTable <- merge(avgSheetTable, noStorageSheets, by = "Income")
avgSheetTable <- merge(avgSheetTable, noStorageAllRegsSheets, by = "Income")
avgSheetTable[, "Income" := factor(Income, ordered = TRUE,
                                   levels = c("<25k", "25-50k", "50-100k", ">100k"))]
setorder(avgSheetTable, Income)
stargazer(avgSheetTable, summary = FALSE, type = "text", digits = 0)
# Saved in counterfactualMNL.tex

################################################################################
############ DISCOUNT -> ALL REG -> NO STORAGE COSTS ###########################
################################################################################
# Getting estimates on Discount store assortments #######################
tpDiscount <- getStore("Discount")

inc25DT <- tpDiscount[household_income_coarse == "<25k"]
Xds <- model.matrix(formula(r[[1]]), inc25DT)
rownames(Xds) <- with(inc25DT, paste0(household_code, ".", brandRollSheet))
ds1 <- getProbs(r[[1]], Xds)
ds1[, "Income" := "<25k"]

inc2550DT <- tpDiscount[household_income_coarse == "25-50k"]
Xds <- model.matrix(formula(r[[2]]), inc2550DT)
rownames(Xds) <- with(inc2550DT, paste0(household_code, ".", brandRollSheet))
ds2 <- getProbs(r[[2]], Xds)
ds2[, "Income" := "25-50k"]

inc50100DT <- tpDiscount[household_income_coarse == "50-100k"]
Xds <- model.matrix(formula(r[[3]]), inc50100DT)
rownames(Xds) <- with(inc50100DT, paste0(household_code, ".", brandRollSheet))
ds3 <- getProbs(r[[3]], Xds)
ds3[, "Income" := "50-100k"]

inc100DT <- tpDiscount[household_income_coarse == ">100k"]
Xds <- model.matrix(formula(r[[4]]), inc100DT)
rownames(Xds) <- with(inc100DT, paste0(household_code, ".", brandRollSheet))
ds4 <- getProbs(r[[4]], Xds)
ds4[, "Income" := ">100k"]

discountAssort <- rbindlist(list(ds1, ds2, ds3, ds4), use.names = TRUE)
discountAssortSheets <- discountAssort[, .(discountAssort = sum(predictedShare * sheets)), by = Income]

# Adding regulations
inc25DT[, "pReg"] <- inc25DT[, "unitPrice"]
Xds <- model.matrix(formula(r[[1]]), inc25DT)
rownames(Xds) <- with(inc25DT, paste0(household_code, ".", brandRollSheet))
ar1 <- getProbs(r[[1]], Xds)
ar1[, "Income" := "<25k"]

inc2550DT[, "pReg"] <- inc2550DT[, "unitPrice"]
Xds <- model.matrix(formula(r[[2]]), inc2550DT)
rownames(Xds) <- with(inc2550DT, paste0(household_code, ".", brandRollSheet))
ar2 <- getProbs(r[[2]], Xds)
ar2[, "Income" := "25-50k"]

inc50100DT[, "pReg"] <- inc50100DT[, "unitPrice"]
Xds <- model.matrix(formula(r[[3]]), inc50100DT)
rownames(Xds) <- with(inc50100DT, paste0(household_code, ".", brandRollSheet))
ar3 <- getProbs(r[[3]], Xds)
ar3[, "Income" := "50-100k"]

inc100DT[, "pReg"] <- inc100DT[, "unitPrice"]
Xds <- model.matrix(formula(r[[4]]), inc100DT)
rownames(Xds) <- with(inc100DT, paste0(household_code, ".", brandRollSheet))
ar4 <- getProbs(r[[4]], Xds)
ar4[, "Income" := ">100k"]

allRegs <- rbindlist(list(ar1, ar2, ar3, ar4), use.names = TRUE)
allRegsSheets <- allRegs[, .(allRegs = sum(predictedShare * sheets)), by = Income]

# Same storage costs
richBeta <- as.data.table(summary(r[[4]])$CoefTable, keep.rownames = TRUE)
richBeta[`Pr(>|z|)` > 0.05, "Estimate" := 0]

beta25 <- as.data.table(summary(r[[1]])$CoefTable, keep.rownames = TRUE)
beta25[grepl("large*", rn), "Estimate"] <- richBeta[grepl("large*", rn), "Estimate"]
Xds <- model.matrix(formula(r[[1]]), inc25DT)
rownames(Xds) <- with(inc25DT, paste0(household_code, ".", brandRollSheet))
ns1 <- getProbs(r[[1]], Xds, beta = beta25)
ns1[, "Income" := "<25k"]

beta2550 <- as.data.table(summary(r[[2]])$CoefTable, keep.rownames = TRUE)
beta2550[grepl("large*", rn), "Estimate"] <- richBeta[grepl("large*", rn), "Estimate"]
Xds <- model.matrix(formula(r[[2]]), inc2550DT)
rownames(Xds) <- with(inc2550DT, paste0(household_code, ".", brandRollSheet))
ns2 <- getProbs(r[[2]], Xds, beta = beta2550)
ns2[, "Income" := "25-50k"]

beta50100 <- as.data.table(summary(r[[3]])$CoefTable, keep.rownames = TRUE)
beta50100[grepl("large*", rn), "Estimate"] <- richBeta[grepl("large*", rn), "Estimate"]
Xds <- model.matrix(formula(r[[3]]), inc50100DT)
rownames(Xds) <- with(inc50100DT, paste0(household_code, ".", brandRollSheet))
ns3 <- getProbs(r[[3]], Xds, beta = beta50100)
ns3[, "Income" := "50-100k"]

beta100 <- as.data.table(summary(r[[4]])$CoefTable, keep.rownames = TRUE)
beta100[grepl("large*", rn), "Estimate"] <- richBeta[grepl("large*", rn), "Estimate"]
Xds <- model.matrix(formula(r[[4]]), inc100DT)
rownames(Xds) <- with(inc100DT, paste0(household_code, ".", brandRollSheet))
ns4 <- getProbs(r[[4]], Xds, beta = beta100)
ns4[, "Income" := ">100k"]

noStorage <- rbindlist(list(ns1, ns2, ns3, ns4), use.names = TRUE)
noStorageSheets <- noStorage[, .(noStorage = sum(predictedShare * sheets)), by = Income]

# All regs and no storage
Xds <- model.matrix(formula(r[[1]]), inc25DT)
Xds[, "pReg"] <- Xds[, "unitPrice"]
rownames(Xds) <- with(inc25DT, paste0(household_code, ".", brandRollSheet))
nsar1 <- getProbs(r[[1]], Xds, beta = beta25)
nsar1[, "Income" := "<25k"]

Xds <- model.matrix(formula(r[[2]]), inc2550DT)
Xds[, "pReg"] <- Xds[, "unitPrice"]
rownames(Xds) <- with(inc2550DT, paste0(household_code, ".", brandRollSheet))
nsar2 <- getProbs(r[[2]], Xds, beta = beta2550)
nsar2[, "Income" := "25-50k"]

Xds <- model.matrix(formula(r[[3]]), inc50100DT)
Xds[, "pReg"] <- Xds[, "unitPrice"]
rownames(Xds) <- with(inc50100DT, paste0(household_code, ".", brandRollSheet))
nsar3 <- getProbs(r[[3]], Xds, beta = beta50100)
nsar3[, "Income" := "50-100k"]

Xds <- model.matrix(formula(r[[4]]), inc100DT)
Xds[, "pReg"] <- Xds[, "unitPrice"]
rownames(Xds) <- with(inc100DT, paste0(household_code, ".", brandRollSheet))
nsar4 <- getProbs(r[[4]], Xds, beta = beta100)
nsar4[, "Income" := ">100k"]

noStorageAllRegs <- rbindlist(list(nsar1, nsar2, nsar3, nsar4), use.names = TRUE)
noStorageAllRegsSheets <- noStorageAllRegs[, .(noStorageAllRegs = sum(predictedShare * sheets)),
                                           by = Income]

# Summary Table
# baseCaseSheets[Income == "25-50k", "Income" := ">100k"]
avgSheetTable <- merge(actualSheets, baseCaseSheets, by = "Income")
avgSheetTable <- merge(avgSheetTable, discountAssortSheets, by = "Income")
avgSheetTable <- merge(avgSheetTable, allRegsSheets, by = "Income")
#avgSheetTable <- merge(avgSheetTable, noStorageSheets, by = "Income")
avgSheetTable <- merge(avgSheetTable, noStorageAllRegsSheets, by = "Income")
avgSheetTable[, "Income" := factor(Income, ordered = TRUE,
                                   levels = c("<25k", "25-50k", "50-100k", ">100k"))]
setorder(avgSheetTable, Income)
stargazer(avgSheetTable, summary = FALSE, type = "text", digits = 0)
# Saved in counterfactualMNLDiscount.tex

################################################################################
############ CLUB -> ALL REG -> NO STORAGE COSTS ###############################
################################################################################
# Getting estimates on Club store assortments #######################
tpClub <- getStore("Club")

inc25DT <- tpClub[household_income_coarse == "<25k"]
Xds <- model.matrix(formula(r[[1]]), inc25DT)
rownames(Xds) <- with(inc25DT, paste0(household_code, ".", brandRollSheet))
ds1 <- getProbs(r[[1]], Xds)
ds1[, "Income" := "<25k"]

inc2550DT <- tpClub[household_income_coarse == "25-50k"]
Xds <- model.matrix(formula(r[[2]]), inc2550DT)
rownames(Xds) <- with(inc2550DT, paste0(household_code, ".", brandRollSheet))
ds2 <- getProbs(r[[2]], Xds)
ds2[, "Income" := "25-50k"]

inc50100DT <- tpClub[household_income_coarse == "50-100k"]
Xds <- model.matrix(formula(r[[3]]), inc50100DT)
rownames(Xds) <- with(inc50100DT, paste0(household_code, ".", brandRollSheet))
ds3 <- getProbs(r[[3]], Xds)
ds3[, "Income" := "50-100k"]

inc100DT <- tpClub[household_income_coarse == ">100k"]
Xds <- model.matrix(formula(r[[4]]), inc100DT)
rownames(Xds) <- with(inc100DT, paste0(household_code, ".", brandRollSheet))
ds4 <- getProbs(r[[4]], Xds)
ds4[, "Income" := ">100k"]

clubAssort <- rbindlist(list(ds1, ds2, ds3, ds4), use.names = TRUE)
clubAssortSheets <- clubAssort[, .(clubAssort = sum(predictedShare * sheets)), by = Income]

# Adding regulations
inc25DT[, "pReg"] <- inc25DT[, "unitPrice"]
Xds <- model.matrix(formula(r[[1]]), inc25DT)
rownames(Xds) <- with(inc25DT, paste0(household_code, ".", brandRollSheet))
ar1 <- getProbs(r[[1]], Xds)
ar1[, "Income" := "<25k"]

inc2550DT[, "pReg"] <- inc2550DT[, "unitPrice"]
Xds <- model.matrix(formula(r[[2]]), inc2550DT)
rownames(Xds) <- with(inc2550DT, paste0(household_code, ".", brandRollSheet))
ar2 <- getProbs(r[[2]], Xds)
ar2[, "Income" := "25-50k"]

inc50100DT[, "pReg"] <- inc50100DT[, "unitPrice"]
Xds <- model.matrix(formula(r[[3]]), inc50100DT)
rownames(Xds) <- with(inc50100DT, paste0(household_code, ".", brandRollSheet))
ar3 <- getProbs(r[[3]], Xds)
ar3[, "Income" := "50-100k"]

inc100DT[, "pReg"] <- inc100DT[, "unitPrice"]
Xds <- model.matrix(formula(r[[4]]), inc100DT)
rownames(Xds) <- with(inc100DT, paste0(household_code, ".", brandRollSheet))
ar4 <- getProbs(r[[4]], Xds)
ar4[, "Income" := ">100k"]

allRegs <- rbindlist(list(ar1, ar2, ar3, ar4), use.names = TRUE)
allRegsSheets <- allRegs[, .(allRegs = sum(predictedShare * sheets)), by = Income]

# Same storage costs
richBeta <- as.data.table(summary(r[[4]])$CoefTable, keep.rownames = TRUE)
richBeta[`Pr(>|z|)` > 0.05, "Estimate" := 0]

beta25 <- as.data.table(summary(r[[1]])$CoefTable, keep.rownames = TRUE)
beta25[grepl("large*", rn), "Estimate"] <- richBeta[grepl("large*", rn), "Estimate"]
Xds <- model.matrix(formula(r[[1]]), inc25DT)
rownames(Xds) <- with(inc25DT, paste0(household_code, ".", brandRollSheet))
ns1 <- getProbs(r[[1]], Xds, beta = beta25)
ns1[, "Income" := "<25k"]

beta2550 <- as.data.table(summary(r[[2]])$CoefTable, keep.rownames = TRUE)
beta2550[grepl("large*", rn), "Estimate"] <- richBeta[grepl("large*", rn), "Estimate"]
Xds <- model.matrix(formula(r[[2]]), inc2550DT)
rownames(Xds) <- with(inc2550DT, paste0(household_code, ".", brandRollSheet))
ns2 <- getProbs(r[[2]], Xds, beta = beta2550)
ns2[, "Income" := "25-50k"]

beta50100 <- as.data.table(summary(r[[3]])$CoefTable, keep.rownames = TRUE)
beta50100[grepl("large*", rn), "Estimate"] <- richBeta[grepl("large*", rn), "Estimate"]
Xds <- model.matrix(formula(r[[3]]), inc50100DT)
rownames(Xds) <- with(inc50100DT, paste0(household_code, ".", brandRollSheet))
ns3 <- getProbs(r[[3]], Xds, beta = beta50100)
ns3[, "Income" := "50-100k"]

beta100 <- as.data.table(summary(r[[4]])$CoefTable, keep.rownames = TRUE)
beta100[grepl("large*", rn), "Estimate"] <- richBeta[grepl("large*", rn), "Estimate"]
Xds <- model.matrix(formula(r[[4]]), inc100DT)
rownames(Xds) <- with(inc100DT, paste0(household_code, ".", brandRollSheet))
ns4 <- getProbs(r[[4]], Xds, beta = beta100)
ns4[, "Income" := ">100k"]

noStorage <- rbindlist(list(ns1, ns2, ns3, ns4), use.names = TRUE)
noStorageSheets <- noStorage[, .(noStorage = sum(predictedShare * sheets)), by = Income]

# All regs and no storage
Xds <- model.matrix(formula(r[[1]]), inc25DT)
Xds[, "pReg"] <- Xds[, "unitPrice"]
rownames(Xds) <- with(inc25DT, paste0(household_code, ".", brandRollSheet))
nsar1 <- getProbs(r[[1]], Xds, beta = beta25)
nsar1[, "Income" := "<25k"]

Xds <- model.matrix(formula(r[[2]]), inc2550DT)
Xds[, "pReg"] <- Xds[, "unitPrice"]
rownames(Xds) <- with(inc2550DT, paste0(household_code, ".", brandRollSheet))
nsar2 <- getProbs(r[[2]], Xds, beta = beta2550)
nsar2[, "Income" := "25-50k"]

Xds <- model.matrix(formula(r[[3]]), inc50100DT)
Xds[, "pReg"] <- Xds[, "unitPrice"]
rownames(Xds) <- with(inc50100DT, paste0(household_code, ".", brandRollSheet))
nsar3 <- getProbs(r[[3]], Xds, beta = beta50100)
nsar3[, "Income" := "50-100k"]

Xds <- model.matrix(formula(r[[4]]), inc100DT)
Xds[, "pReg"] <- Xds[, "unitPrice"]
rownames(Xds) <- with(inc100DT, paste0(household_code, ".", brandRollSheet))
nsar4 <- getProbs(r[[4]], Xds, beta = beta100)
nsar4[, "Income" := ">100k"]

noStorageAllRegs <- rbindlist(list(nsar1, nsar2, nsar3, nsar4), use.names = TRUE)
noStorageAllRegsSheets <- noStorageAllRegs[, .(noStorageAllRegs = sum(predictedShare * sheets)),
                                           by = Income]

# Summary Table
# baseCaseSheets[Income == "25-50k", "Income" := ">100k"]
avgSheetTable <- merge(actualSheets, baseCaseSheets, by = "Income")
avgSheetTable <- merge(avgSheetTable, clubAssortSheets, by = "Income")
avgSheetTable <- merge(avgSheetTable, allRegsSheets, by = "Income")
#avgSheetTable <- merge(avgSheetTable, noStorageSheets, by = "Income")
avgSheetTable <- merge(avgSheetTable, noStorageAllRegsSheets, by = "Income")
avgSheetTable[, "Income" := factor(Income, ordered = TRUE,
                                   levels = c("<25k", "25-50k", "50-100k", ">100k"))]
setorder(avgSheetTable, Income)
stargazer(avgSheetTable, summary = FALSE, type = "text", digits = 0)
# Saved in counterfactualMNL.tex
