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
    reg1 <- mlogit(choice ~ price + unitPrice + lDays + large + small + brand_descr + 0,
                   data = tpML[tpML$household_income_coarse == incBin, ])
    reg2 <- mlogit(choice ~ price +
                     unitPrice + unitReg +
                     lDays + large + small + brand_descr + 0,
                   data = tpML[tpML$household_income_coarse == incBin, ])
    reg3 <- mlogit(choice ~ price +
                     unitPrice + unitReg +
                     lDays + large + largeHome + small + smallHome + brand_descr + 0,
                   data = tpML[tpML$household_income_coarse == incBin, ])
    stargazer(reg1, reg2, reg3, type = "text")
    print(lrtest(reg1, reg2, reg3))
    save(reg3, file = paste0("/home/upenn/hossaine/Nielsen/mlogit/MNLOnly/mlogit",
                             incBin, i, ".rda"), compress = TRUE)
  }

################################################################################
############## STEP 1A: ELASTICITIES ###########################################
################################################################################
fullElast <- NULL
fullCoefs <- NULL
for (i in c("<25k", "25-50k", "50-100k", ">100k")) {
  load(paste0("/home/upenn/hossaine/Nielsen/mlogit/MNLOnly/mlogit", i, "2016.rda"))
  margs <- effects(reg3, covariate = "price", type = "rr")
  ownElast <- as.data.table(diag(margs), keep.rownames = TRUE)
  ownElast[, "household_income_coarse" := i]
  fullElast <- rbindlist(list(fullElast, ownElast), use.names = TRUE)
  fullCoefs[[i]] <- reg3
}

stargazer(fullCoefs, type = "text",
          add.lines = list(c("Brand FE's", "Y", "Y", "Y", "Y")),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("<25k", "25-50k", "50-100k", ">100k"),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          omit = c("brand_descr*"),
          covariate.labels = c("Total Price",
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
################ STEP 3: Counterfactual Exercise: PREDICTING AVERAGE SHEETS ####
################################################################################
# Load parameters
getCoefs <- function(income) {
  load(paste0("/home/upenn/hossaine/Nielsen/mlogit/newModel2016OnlyV4/mlogit", income, "2016.rda"))
  return(reg3)
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
  XDT <- as.data.table(X, keep.rownames = TRUE)[, .(rn, unitPrice)]
  eXB <- exp(XB)
  eXB[is.na(eXB), ] <- 0
  eXBDT <- as.data.table(eXB, keep.rownames = TRUE)
  unitPrice <- na.omit(merge(XDT, eXBDT, by = "rn"))
  unitPrice <- unitPrice[, weighted.mean(unitPrice, w = V1)]
  eXBDT[, c("trip_code_uc", "rn") := tstrsplit(rn, ".", fixed = TRUE)]
  eXBDT[, c("brand_descr", "rolls", "sheets") := tstrsplit(rn, "_", fixed = TRUE)]
  eXBDT[, "prob" := V1 / sum(V1), by = trip_code_uc]
  eXBDT[, "logsum" := log(sum(V1)), by = trip_code_uc]
  surplus <- unique(eXBDT[, .(trip_code_uc, logsum)])
  probs <- eXBDT[, .(predictedShare = mean(prob)), keyby = .(brand_descr, rolls, sheets)]
  probs[, "sheets" := as.integer(sheets)]
  return(list(probs = probs, surplus = surplus, unitPrice = unitPrice))
}

################################################################################
############ PREDICTED -> ALL REG -> NO STORAGE COSTS ##########################
################################################################################
# Getting predicted market shares of each product by income group ##############
getPS <- function(X) {
  getProbs(X, model.matrix(X))[["probs"]]
}
baseCase <- rbindlist(lapply(r, getPS), use.names = TRUE, idcol = "Income")
baseCase[, "Income" := factor(Income, levels = 1:4, ordered = TRUE,
                              labels = c("<25k", "25-50k", "50-100k", ">100k"))]
baseCaseSheets <- baseCase[, .(Base = sum(sheets * predictedShare)), by = Income]

# getPSSurplus <- function(X) {
#   getProbs(X, model.matrix(X))[["surplus"]]
# }
# baseCaseSurplus <- rbindlist(lapply(r, getPSSurplus), use.names = TRUE, idcol = "Income")
# setnames(baseCaseSurplus, "logsum", "baseLogSum")

getPSUnitPrice <- function(X) {
  getProbs(X, model.matrix(X))[["unitPrice"]]
}
baseCaseUnitPrice <- lapply(r, getPSUnitPrice)

# Comparing predictions
comp <- merge(actualShares, baseCase, by = c("brand_descr", "rolls", "sheets", "Income"))
comp[, "brandRollSheet" := NULL]
comp[, "diff" := actualShare - predictedShare]
comp[abs(diff) > 0.01]

# Add regulations
getAllRegs <- function(X) {
  allRegs <- model.matrix(X)
  allRegs[, "unitReg"] <- allRegs[, "unitPrice"]
  getProbs(X, allRegs)[["probs"]]
}
allRegs <- rbindlist(lapply(r, getAllRegs), use.names = TRUE, idcol = "Income")
allRegs[, "Income" := factor(Income, levels = 1:4, ordered = TRUE,
                             labels = c("<25k", "25-50k", "50-100k", ">100k"))]
allRegsSheets <- allRegs[, .(allRegs = sum(predictedShare * sheets)), by = Income]

# getAllRegsSurplus <- function(X) {
#   allRegs <- model.matrix(X)
#   allRegs[, "unitReg"] <- allRegs[, "unitPrice"]
#   getProbs(X, allRegs)[["surplus"]]
# }
# allRegsSurplus <- rbindlist(lapply(r, getAllRegsSurplus), use.names = TRUE, idcol = "Income")
# setnames(allRegsSurplus, "logsum", "allRegsLogSum")

getAllRegsUnitPrice <- function(X) {
  allRegs <- model.matrix(X)
  allRegs[, "unitReg"] <- allRegs[, "unitPrice"]
  getProbs(X, allRegs)[["unitPrice"]]
}
allRegsUnitPrice <- lapply(r, getAllRegsUnitPrice)

# fullSurplus <- merge(baseCaseSurplus, allRegsSurplus, by = c("Income", "trip_code_uc"))
# fullSurplus[Income == 1, "price" := coef(r[[1]])["price"]]
# fullSurplus[Income == 2, "price" := coef(r[[2]])["price"]]
# fullSurplus[Income == 3, "price" := coef(r[[3]])["price"]]
# fullSurplus[Income == 4, "price" := coef(r[[4]])["price"]]
# fullSurplus[, "baseSurplus" := - baseLogSum / price]
# fullSurplus[, "allRegSurplus" := - allRegsLogSum / price]
# fullSurplus[, "change" := allRegSurplus - baseSurplus]
# quantile(fullSurplus[Income == 1]$change)

# Same storage costs
richBeta <- as.data.table(summary(r[[4]])$CoefTable, keep.rownames = TRUE)
richBeta[`Pr(>|z|)` > 0.05, "Estimate" := 0]

getNS <- function(X) {
  betaNew <- as.data.table(summary(X)$CoefTable, keep.rownames = TRUE)
  betaNew[grepl("large*", rn), "Estimate"] <- richBeta[grepl("large*", rn), "Estimate"]
  betaNew[grepl("large*", rn), "Pr(>|z|)"] <- richBeta[grepl("large*", rn), "Pr(>|z|)"]
  betaNew[grepl("small*", rn), "Estimate"] <- richBeta[grepl("small*", rn), "Estimate"]
  betaNew[grepl("small*", rn), "Pr(>|z|)"] <- richBeta[grepl("small*", rn), "Pr(>|z|)"]
  getProbs(X, model.matrix(X), beta = betaNew)[["probs"]]
}
noStorage <- rbindlist(lapply(r, getNS), use.names = TRUE, idcol = "Income")
noStorage[, "Income" := factor(Income, levels = 1:4, ordered = TRUE,
                               labels = c("<25k", "25-50k", "50-100k", ">100k"))]
noStorageSheets <- noStorage[, .(noStorage = sum(predictedShare * sheets)), by = Income]

# getNSSurplus <- function(X) {
#   betaNew <- as.data.table(summary(X)$CoefTable, keep.rownames = TRUE)
#   betaNew[grepl("large*", rn), "Estimate"] <- richBeta[grepl("large*", rn), "Estimate"]
#   betaNew[grepl("small*", rn), "Estimate"] <- richBeta[grepl("small*", rn), "Estimate"]
#   getProbs(X, model.matrix(X), beta = betaNew)[["surplus"]]
# }
# noStorageSurplus <- rbindlist(lapply(r, getNSSurplus), use.names = TRUE, idcol = "Income")
# setnames(noStorageSurplus, "logsum", "noStorageLogSum")
# fullSurplus <- merge(fullSurplus, noStorageSurplus, by = c("Income", "trip_code_uc"))
# fullSurplus[, "noStorageSurplus" := - noStorageLogSum / price]
# fullSurplus[, "baseToNoStorage" := noStorageSurplus - baseSurplus]
# quantile(fullSurplus[Income == 1]$baseToNoStorage)

getNSUnitPrice <- function(X) {
  betaNew <- as.data.table(summary(X)$CoefTable, keep.rownames = TRUE)
  betaNew[grepl("large*", rn), "Estimate"] <- richBeta[grepl("large*", rn), "Estimate"]
  betaNew[grepl("large*", rn), "Pr(>|z|)"] <- richBeta[grepl("large*", rn), "Pr(>|z|)"]
  betaNew[grepl("small*", rn), "Estimate"] <- richBeta[grepl("small*", rn), "Estimate"]
  betaNew[grepl("small*", rn), "Pr(>|z|)"] <- richBeta[grepl("small*", rn), "Pr(>|z|)"]
  getProbs(X, model.matrix(X), beta = betaNew)[["unitPrice"]]
}
NSUnitPrice <- lapply(r, getNSUnitPrice)

# All regs and no storage
getNSAR <- function(X) {
  mat <- model.matrix(X)
  mat[, "unitReg"] <- mat[, "unitPrice"]
  betaNew <- as.data.table(summary(X)$CoefTable, keep.rownames = TRUE)
  betaNew[grepl("large*", rn), "Estimate"] <- richBeta[grepl("large*", rn), "Estimate"]
  betaNew[grepl("large*", rn), "Pr(>|z|)"] <- richBeta[grepl("large*", rn), "Pr(>|z|)"]
  betaNew[grepl("small*", rn), "Estimate"] <- richBeta[grepl("small*", rn), "Estimate"]
  betaNew[grepl("small*", rn), "Pr(>|z|)"] <- richBeta[grepl("small*", rn), "Pr(>|z|)"]
  getProbs(X, mat, beta = betaNew)[["probs"]]
}
noStorageAllRegs <- rbindlist(lapply(r, getNSAR), use.names = TRUE, idcol = "Income")
noStorageAllRegs[, "Income" := factor(Income, levels = 1:4, ordered = TRUE,
                                      labels = c("<25k", "25-50k", "50-100k", ">100k"))]
noStorageAllRegsSheets <- noStorageAllRegs[, .(noStorageAllRegs = sum(predictedShare * sheets)),
                                           by = Income]

getNSARUnitPrice <- function(X) {
  mat <- model.matrix(X)
  mat[, "unitReg"] <- mat[, "unitPrice"]
  betaNew <- as.data.table(summary(X)$CoefTable, keep.rownames = TRUE)
  betaNew[grepl("large*", rn), "Estimate"] <- richBeta[grepl("large*", rn), "Estimate"]
  betaNew[grepl("large*", rn), "Pr(>|z|)"] <- richBeta[grepl("large*", rn), "Pr(>|z|)"]
  betaNew[grepl("small*", rn), "Estimate"] <- richBeta[grepl("small*", rn), "Estimate"]
  betaNew[grepl("small*", rn), "Pr(>|z|)"] <- richBeta[grepl("small*", rn), "Pr(>|z|)"]
  getProbs(X, mat, beta = betaNew)[["unitPrice"]]
}
NSARUnitPrice <- lapply(r, getNSARUnitPrice)

# Summary Table
avgSheetTable <- merge(actualSheets, baseCaseSheets, by = "Income")
avgSheetTable <- merge(avgSheetTable, allRegsSheets, by = "Income")
# avgSheetTable <- merge(avgSheetTable, noStorageSheets, by = "Income")
avgSheetTable <- merge(avgSheetTable, noStorageAllRegsSheets, by = "Income")
setorder(avgSheetTable, Income)
stargazer(avgSheetTable, summary = FALSE, type = "text", digits = 0)
# Saving in counterfactualMNLDays.tex
# Save actual and base in modelFit.tex

# Summary of Unit prices
unitPriceTable <- rbindlist(list(baseCaseUnitPrice, allRegsUnitPrice, NSARUnitPrice))
setnames(unitPriceTable, c("<25k", "25-50k", "50-100k", ">100k"))
unitPriceTable <- as.data.table(t(unitPriceTable), keep.rownames = TRUE)
setnames(unitPriceTable, c("Income", "Base", "allRegs", "noStorageAllRegs"))
# Saving in counterfactualMNLPrice.tex


# Finding where people shift their purchases
purch <- merge(baseCase, allRegs, by = c("Income", "brand_descr", "rolls", "sheets"))
setnames(purch, c("predictedShare.x", "predictedShare.y"), c("baseCase", "allRegs"))
purch <- merge(purch, noStorage, by = c("Income", "brand_descr", "rolls", "sheets"))
setnames(purch, c("predictedShare"), c("noStorage"))
purch <- merge(purch, noStorageAllRegs, by = c("Income", "brand_descr", "rolls", "sheets"))
setnames(purch, c("predictedShare"), c("noStorageAllRegs"))






################################################################################
############ CLUB -> ALL REG -> NO STORAGE COSTS ###############################
################################################################################
getStore <- function(storeType) {
  tpData <- fread(paste0("/scratch/upenn/hossaine/tp", storeType, ".csv"))
  tpData[, "brand_descr" := factor(brand_descr,
                                   levels = c("ANGEL SOFT", "CHARMIN", "CTL BR",
                                              "KLEENEX COTTONELLE", "QUILTED NORTHERN",
                                              "SCOTT 1000"))]
  tpData[, "brand_descr" := relevel(brand_descr, ref = "SCOTT 1000")]
  fullMerge <- as.data.table(expand.grid(brandRollSheet = tpData$brandRollSheet,
                                         trip_code_uc = unique(tp[panel_year == 2016]$trip_code_uc)))
  fullMerge <- merge(fullMerge, tpData, by = "brandRollSheet")
  fullMerge <- merge(fullMerge, unique(tp[panel_year == 2016,
                                          .(household_code, household_income_coarse,
                                            adults, nChildren, married, age, dma_cd,
                                            college, lawInd, type_of_residence,
                                            panel_year, projection_factor, trip_code_uc)]),
                     by = "trip_code_uc")

  # Generating price interactions
  fullMerge[, "days" := totalSheet / (57 * 2)]
  fullMerge[, "lDays" := log(days)]
  fullMerge[, "unitPrice" := price / days]

  # Generating unit price interactions
  fullMerge[, "unitReg"      := unitPrice * lawInd]

  # Generating size interactions
  fullMerge[, "large"       := (rolls > 12)]
  fullMerge[, "largeHome"   := large * (type_of_residence == "Single-Family")]
  fullMerge[, "small"       := (rolls < 12)]
  fullMerge[, "smallHome"   := small * (type_of_residence == "Single-Family")]

  # Coding package sizes and brands
  fullMerge[, "brand_descr" := relevel(as.factor(brand_descr), ref = "SCOTT 1000")]

  fullMerge[, "choice" := 0L]
  fullMerge[, "household_income_coarse" := factor(household_income_coarse,
                                                  ordered = TRUE,
                                                  levels = c("<25k", "25-50k",
                                                             "50-100k", ">100k"))]
  setnames(fullMerge, "totalSheet", "sheets")
  return(fullMerge)
}

# Getting estimates on Club store assortments #######################
tpClub <- getStore("Club")
tpClub[, c("channel_type", "large12") := NULL]
tpClubFull <- rbindlist(list(tpClub, tp), use.names = TRUE)

getClub <- function(X) {
  mat <- model.matrix(~ price + unitPrice + unitReg + lDays + large +
                        largeHome + small + smallHome + brand_descr + 0,
                      data = tpClubFull)
  rownames(mat) <- paste(tpClubFull$trip_code_uc, tpClubFull$brandRollSheet, sep = ".")
  mat <- mat[, -5]
  getProbs(X, mat)[["probs"]]
}
clubAssort <- rbindlist(lapply(r, getClub), use.names = TRUE, idcol = "Income")
clubAssort[, "Income" := factor(Income, levels = 1:4, ordered = TRUE,
                                      labels = c("<25k", "25-50k", "50-100k", ">100k"))]
clubAssortSheets <- clubAssort[, .(clubAssort = sum(predictedShare * sheets)),
                               by = Income]



inc25DT <- tpClub[household_income_coarse == "<25k"]
Xds <- model.matrix(formula(r[[1]]), inc25DT)
rownames(Xds) <- with(inc25DT, paste0(household_code, ".", brandRollSheet))
ds1 <- getProbs(r[[1]], Xds)[["probs"]]
ds1[, "Income" := "<25k"]

inc2550DT <- tpClub[household_income_coarse == "25-50k"]
Xds <- model.matrix(formula(r[[2]]), inc2550DT)
rownames(Xds) <- with(inc2550DT, paste0(household_code, ".", brandRollSheet))
ds2 <- getProbs(r[[2]], Xds)[["probs"]]
ds2[, "Income" := "25-50k"]

inc50100DT <- tpClub[household_income_coarse == "50-100k"]
Xds <- model.matrix(formula(r[[3]]), inc50100DT)
rownames(Xds) <- with(inc50100DT, paste0(household_code, ".", brandRollSheet))
ds3 <- getProbs(r[[3]], Xds)[["probs"]]
ds3[, "Income" := "50-100k"]

inc100DT <- tpClub[household_income_coarse == ">100k"]
Xds <- model.matrix(formula(r[[4]]), inc100DT)
rownames(Xds) <- with(inc100DT, paste0(household_code, ".", brandRollSheet))
ds4 <- getProbs(r[[4]], Xds)[["probs"]]
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
