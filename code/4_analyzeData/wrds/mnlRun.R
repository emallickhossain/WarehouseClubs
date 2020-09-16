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
tp[, "sheetsPP" := sheets / (adults + nChildren)]
tp[, "days" := sheets / (57 * 2)]
tp[, "daysPP" := sheetsPP / (57 * 2)]
tp[, "lDays" := log(days)]
tp[, "lDaysPP" := log(daysPP)]

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
                lawInd, lDays, lDaysPP, daysPP, type_of_residence)]
tp[, "unitPrice" := price / days]
tp[, "unitPricePP" := price / daysPP]

# Generating unit price interactions
tp[, "unitReg"      := unitPrice * lawInd]
tp[, "unitRegPP"      := unitPricePP * lawInd]

# Generating size interactions
tp[, "large"       := as.integer(rolls > 12)]
tp[, "largeHome"   := large * (type_of_residence == "Single-Family")]
tp[, "small"       := as.integer(rolls < 12)]
tp[, "smallHome"   := small * (type_of_residence == "Single-Family")]

# Coding package sizes and brands
tp[, "brand_descr" := relevel(as.factor(brand_descr), ref = "SCOTT 1000")]

# Running in parallel on years
registerDoParallel()
getDoParWorkers()

set.seed(121)
# hhInc <- unique(tp[, .(household_code, panel_year, household_income_coarse)])
# ids <- hhInc[panel_year == 2016, .SD[sample(.N, 150)],
#              by = household_income_coarse]$household_code

r <- foreach(i = 2016) %:%
  foreach(incBin = c("<25k", "25-50k", "50-100k", ">100k")) %dopar% {
    print(c(i, incBin))
    # Running MNL model
    # Creating mlogit data for analysis
    tpSub <- tp[panel_year == i]
    tpML <- mlogit.data(tpSub, choice = "choice", shape = "long",
                        id.var = "household_code", alt.var = "brandRollSheet",
                        chid.var = "trip_code_uc", opposite = c("price", "unitPricePP"))
    # reg1 <- mlogit(choice ~ price + unitPricePP + lDaysPP + large + small + brand_descr + 0,
    #                data = tpML[tpML$household_income_coarse == incBin, ])
    # reg2 <- mlogit(choice ~ price +
    #                  unitPricePP + unitRegPP +
    #                  lDaysPP + large + small + brand_descr + 0,
    #                data = tpML[tpML$household_income_coarse == incBin, ])
    # reg3 <- mlogit(choice ~ price +
    #                  unitPricePP + unitRegPP +
    #                  lDaysPP + large + largeHome + small + smallHome + brand_descr + 0,
    #                data = tpML[tpML$household_income_coarse == incBin, ])
    # print(paste("Conditional Logit done for", incBin))
    # reg4 <- mlogit(choice ~ price +
    #                  unitPricePP + unitRegPP +
    #                  lDaysPP + large + largeHome + small + smallHome + brand_descr + 0,
    #                data = tpML[tpML$household_income_coarse == incBin, ],
    #                rpar = c(unitPricePP = "n"),
    #                R = 25, halton = NA, panel = TRUE)
    # reg5 <- mlogit(choice ~ price +
    #                  unitPricePP + unitRegPP +
    #                  lDaysPP + large + largeHome + small + smallHome + brand_descr + 0,
    #                data = tpML[tpML$household_income_coarse == incBin, ],
    #                rpar = c(unitPricePP = "n", lDaysPP = "n"),
    #                R = 25, halton = NA, panel = TRUE)
    # reg6 <- mlogit(choice ~ price +
    #                  unitPricePP + unitRegPP +
    #                  lDaysPP + large + largeHome + small + smallHome + brand_descr + 0,
    #                data = tpML[tpML$household_income_coarse == incBin, ],
    #                rpar = c(unitPricePP = "n", lDaysPP = "n", large = "n", small = "n"),
    #                R = 25, halton = NA, panel = TRUE)
    # print(paste("Mixed Logit done for", incBin))
    reg7 <- mlogit(choice ~ price +
                     unitPricePP + unitRegPP +
                     lDaysPP + large + largeHome + small + smallHome + brand_descr + 0,
                   data = tpML[tpML$household_income_coarse == incBin, ],
                   rpar = c(unitPricePP = "n", lDaysPP = "n", large = "n", small = "n"),
                   R = 25, halton = NA, panel = TRUE, correlation = TRUE)
    print(paste("Correlated mixed Logit done for", incBin))
    reg8 <- mlogit(choice ~ price +
                     unitPricePP + unitRegPP +
                     lDaysPP + large + largeHome + small + smallHome + brand_descr + 0,
                   data = tpML[tpML$household_income_coarse == incBin, ],
                   rpar = c(unitPricePP = "ln", lDaysPP = "n", large = "n", small = "n"),
                   R = 25, halton = NA, panel = TRUE, correlation = TRUE)
    print(paste("Lognormal done for", incBin))

    # stargazer(reg1, reg2, reg3, reg4, reg5, reg6, type = "text")
    # print(lrtest(reg1, reg2, reg3, reg4, reg5, reg6))
    stargazer(reg7, reg8, type = "text")
    # print(lrtest(reg3, reg6, reg7))
    # save(reg3, file = paste0("/home/upenn/hossaine/Nielsen/mlogit/MNLOnly/mlogit",
    #                          incBin, i, "reg3.rda"), compress = TRUE)
    # save(reg7, file = paste0("/home/upenn/hossaine/Nielsen/mlogit/MNLOnly/mlogit",
    #                          incBin, i, "reg7.rda"), compress = TRUE)
    save(reg8, file = paste0("/home/upenn/hossaine/Nielsen/mlogit/MNLOnly/mlogit",
                             incBin, i, "reg8.rda"), compress = TRUE)
  }

################################################################################
################ STEP 3: Counterfactual Exercise: PREDICTING AVERAGE SHEETS ####
################################################################################
# Load parameters
getCoefs <- function(income) {
  load(paste0("/home/upenn/hossaine/Nielsen/mlogit/MNLOnly/mlogit", income, "2016reg3.rda"))
  return(reg3)
}
r <- map(c("<25k", "25-50k", "50-100k", ">100k"), getCoefs)

# Coefficient table
stargazer(r, type = "text",
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
# Switching price and unit price coefficients to be negative. They were flipped
# for the mlogit estimation
getProbs <- function(reg, X, beta = NULL) {
  if (is.null(beta)) {
    beta <- as.data.table(summary(reg)$CoefTable, keep.rownames = TRUE)
  }
  beta[rn == "price", "Estimate" := -Estimate] # Flipping sign
  beta[`Pr(>|z|)` > 0.05, "Estimate" := 0]
  X[, "price"] <- -X[, "price"] # Flipping price sign
  XB <- X %*% beta$Estimate
  XDT <- as.data.table(X, keep.rownames = TRUE)[, .(rn, unitPricePP)]
  eXB <- exp(XB)
  eXB[is.na(eXB), ] <- 0
  eXBDT <- as.data.table(eXB, keep.rownames = TRUE)
  unitPrice <- na.omit(merge(XDT, eXBDT, by = "rn"))
  unitPrice <- unitPrice[, weighted.mean(unitPricePP, w = V1)]
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
# Biggest misses are overpredicting some Charmin 4-packs but under-predicting the 6-packs
# Also have some CTL BR misses, but this is likely due to differences in store-brands
# across stores. I'm assuming all store brands are the same
comp <- merge(actualShares, baseCase, by = c("brand_descr", "rolls", "sheets", "Income"))
comp[, "brandRollSheet" := NULL]
comp[, "diff" := actualShare - predictedShare]
comp[abs(diff) > 0.01]

# Add regulations
getAllRegs <- function(X) {
  allRegs <- model.matrix(X)
  allRegs[, "unitRegPP"] <- allRegs[, "unitPricePP"]
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
  allRegs[, "unitRegPP"] <- allRegs[, "unitPricePP"]
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
  mat[, "unitRegPP"] <- mat[, "unitPricePP"]
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
  mat[, "unitRegPP"] <- mat[, "unitPricePP"]
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
avgSheetTable / (57 * 2)
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

################# ELASTICITIES ############################################
# Function to change price, unit price, and price reg for each product one at a time
getNewPrice <- function(i, prices, nProds) {
  newPrice <- as.data.table(prices)
  newPrice[seq(i, nrow(prices), nProds), "price" := price * 1.01]
  newPrice[seq(i, nrow(prices), nProds), "unitPricePP" := unitPricePP * 1.01]
  newPrice[seq(i, nrow(prices), nProds), "unitRegPP" := unitRegPP * 1.01]
  return(newPrice)
}

# Getting regular choice probabilities
ownElast <- NULL

for (j in 1:4) {
  baseProbs <- getProbs(r[[j]], model.matrix(r[[j]]))$probs
  elast <- matrix(nrow = nrow(baseProbs), ncol = nrow(baseProbs))

  origPrices <- as.data.table(model.matrix(r[[j]]), keep.rownames = TRUE)[, .(rn, price, unitPricePP, unitRegPP)]
  origPrices[, c("trip", "product") := tstrsplit(rn, ".", fixed = TRUE)]
  nProds <- uniqueN(origPrices$product)

  for (i in 1:nrow(baseProbs)) {
    print(i)
    updatedPrice <- getNewPrice(i, origPrices, nProds)
    newModelMat <- model.matrix(r[[j]])
    newModelMat[, "price"] <- updatedPrice$price
    newModelMat[, "unitPricePP"] <- updatedPrice$unitPricePP
    newModelMat[, "unitRegPP"] <- updatedPrice$unitRegPP
    newProbs <- getProbs(r[[j]], newModelMat)$probs

    elast[i, ] <- (newProbs$predictedShare - baseProbs$predictedShare) /
      baseProbs$predictedShare * 100
  }
  diagElast <- data.table(elast = diag(elast), Income = j)
  ownElast <- rbindlist(list(ownElast, diagElast), use.names = TRUE)
}

ownElast[, "Income" := factor(Income, levels = 1:4, ordered = TRUE,
                              labels = c("<25k", "25-50k", "50-100k", ">100k"))]
ggplot(data = ownElast, aes(x = elast)) +
  geom_histogram(aes(y = ..density..), bins = 30) +
  geom_hline(yintercept = 0) +
  facet_grid(rows = vars(Income)) +
  labs(x = "Elasticity",
       y = "Density") +
  scale_x_continuous(limits = c(-6, 0)) +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom")
ggsave(filename = "./figures/elasticity2016.pdf", height = 4, width = 6)
