# Runs counterfactual simulations for random coefficients estimation
# obtained from mnlRun.R
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
library(parallel)
library(MASS)
threads <- 8
nDraws <- 1000

################################################################################
################ STEP 3: Counterfactual Exercise: PREDICTING AVERAGE SHEETS ####
################################################################################
# Load parameters
getCoefs <- function(income) {
  load(paste0("/home/upenn/hossaine/Nielsen/mlogit/MNLOnly/mlogit", income, "2016reg7.rda"))
  return(reg7)
}
r <- map(c("<25k", "25-50k", "50-100k", ">100k"), getCoefs)

# Coefficient table
stargazer(r, type = "text",
          add.lines = list(c("Brand FE's", "Y", "Y", "Y", "Y")),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("<25k", "25-50k", "50-100k", ">100k"),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          omit = c("brand_descr*", "chol"),
          covariate.labels = c("Total Price",
                               "Unit Price", ". : Reg",
                               "Log(Days)",
                               "Large Size", ". : Home",
                               "Small Size", ". : Home"),
          notes.align = "l",
          notes.append = TRUE,
          digits = 3,
          out = "tables/mlogit2016_Random.tex")

# Calculating SD's to add to the coefficient matrix
summary(vcov(r[[1]], what = "rpar", type = "cor"))
summary(vcov(r[[2]], what = "rpar", type = "cor"))
summary(vcov(r[[3]], what = "rpar", type = "cor"))
summary(vcov(r[[4]], what = "rpar", type = "cor"))

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
  beta <- beta[!grepl("chol", rn)]
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
  eXBDT[, ':=' (prob = V1 / sum(V1),
                logsum = log(sum(V1))), by = trip_code_uc]
  probs <- eXBDT[, .(predictedShare = mean(prob)), keyby = .(brand_descr, rolls, sheets)]
  probs[, "sheets" := as.integer(sheets)]
  return(list(probs = probs, unitPrice = unitPrice))
}

################################################################################
############ PREDICTED -> ALL REG -> NO STORAGE COSTS ##########################
################################################################################
# Getting beta draws that will be used in simulation
getProbsRand <- function(j, reg, mat, beta_eval, beta_draws) {
  beta_eval[rn == "unitPricePP", "Estimate" := beta_draws[j, 1]]
  beta_eval[rn == "lDaysPP", "Estimate" := beta_draws[j, 2]]
  beta_eval[rn == "large", "Estimate" := beta_draws[j, 3]]
  beta_eval[rn == "small", "Estimate" := beta_draws[j, 4]]
  # beta_eval <- as.data.table(summary(reg)$CoefTable, keep.rownames = TRUE)
  beta_eval <- beta_eval[!grepl("chol", rn)]
  return(getProbs(reg, mat, beta_eval))
}

getProbsInt <- function(reg, mat, beta = NULL) {
  set.seed(121)

  if (is.null(beta)) {
    coefs_inc <- as.data.table(summary(reg)$CoefTable, keep.rownames = TRUE)
  } else {
    coefs_inc <- beta
  }

  coefs_inc <- coefs_inc[!grepl("chol", rn)]
  coefs_inc[`Pr(>|z|)` > 0.05, "Estimate" := 0]

  # Drawing random betas
  randoms <- c("unitPricePP", "lDaysPP", "large", "small")
  beta_means <- coefs_inc[rn %in% randoms]$Estimate
  beta_cov <- vcov(reg, what = "rpar")
  beta_draws <- mvrnorm(nDraws, mu = beta_means, Sigma = beta_cov)

  probs_draws <- mclapply(1:nDraws, getProbsRand, reg = reg, mat = mat, mc.cores = 4L,
                        beta_eval = coefs_inc, beta_draws = beta_draws)

  fullProbs <- NULL
  fullUnitPrice <- NULL
  for (j in 1:nDraws) {
    fullProbs <- rbindlist(list(fullProbs, probs_draws[[j]]$probs))
    fullUnitPrice <- c(fullUnitPrice, probs_draws[[j]]$unitPrice)
  }
  finalProbs <- fullProbs[, .(predictedShare = mean(predictedShare)),
                          by = .(brand_descr, rolls, sheets)]
  finalUnitPrice <- mean(fullUnitPrice)
  # finalProbs[, sum(sheets * predictedShare)] / (57 * 2)

  return(list(probs = finalProbs, unitPrice = finalUnitPrice))
}

# Getting predicted market shares of each product by income group ##############
getPS <- function(X) {
  getProbsInt(X, model.matrix(X))[["probs"]]
}
baseCase <- rbindlist(lapply(r, getPS), use.names = TRUE, idcol = "Income")
baseCase[, "Income" := factor(Income, levels = 1:4, ordered = TRUE,
                              labels = c("<25k", "25-50k", "50-100k", ">100k"))]
baseCaseSheets <- baseCase[, .(Base = sum(sheets * predictedShare)), by = Income]

getPSUnitPrice <- function(X) {
  getProbsInt(X, model.matrix(X))[["unitPrice"]]
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
  allRegs[, "unitRegPP"] <- allRegs[, "unitPricePP"]
  getProbsInt(X, allRegs)[["probs"]]
}
allRegs <- rbindlist(lapply(r, getAllRegs), use.names = TRUE, idcol = "Income")
allRegs[, "Income" := factor(Income, levels = 1:4, ordered = TRUE,
                             labels = c("<25k", "25-50k", "50-100k", ">100k"))]
allRegsSheets <- allRegs[, .(allRegs = sum(predictedShare * sheets)), by = Income]

getAllRegsUnitPrice <- function(X) {
  allRegs <- model.matrix(X)
  allRegs[, "unitRegPP"] <- allRegs[, "unitPricePP"]
  getProbsInt(X, allRegs)[["unitPrice"]]
}
allRegsUnitPrice <- lapply(r, getAllRegsUnitPrice)

# Same storage costs
richBeta <- as.data.table(summary(r[[4]])$CoefTable, keep.rownames = TRUE)
richBeta[`Pr(>|z|)` > 0.05, "Estimate" := 0]

# getNS <- function(X) {
#   betaNew <- as.data.table(summary(X)$CoefTable, keep.rownames = TRUE)
#   betaNew[grepl("large*", rn), "Estimate"] <- richBeta[grepl("large*", rn), "Estimate"]
#   betaNew[grepl("large*", rn), "Pr(>|z|)"] <- richBeta[grepl("large*", rn), "Pr(>|z|)"]
#   betaNew[grepl("small*", rn), "Estimate"] <- richBeta[grepl("small*", rn), "Estimate"]
#   betaNew[grepl("small*", rn), "Pr(>|z|)"] <- richBeta[grepl("small*", rn), "Pr(>|z|)"]
#   getProbsInt(X, model.matrix(X), beta = betaNew)[["probs"]]
# }
# noStorage <- rbindlist(lapply(r, getNS), use.names = TRUE, idcol = "Income")
# noStorage[, "Income" := factor(Income, levels = 1:4, ordered = TRUE,
#                                labels = c("<25k", "25-50k", "50-100k", ">100k"))]
# noStorageSheets <- noStorage[, .(noStorage = sum(predictedShare * sheets)), by = Income]
#
# getNSUnitPrice <- function(X) {
#   betaNew <- as.data.table(summary(X)$CoefTable, keep.rownames = TRUE)
#   betaNew[grepl("large*", rn), "Estimate"] <- richBeta[grepl("large*", rn), "Estimate"]
#   betaNew[grepl("large*", rn), "Pr(>|z|)"] <- richBeta[grepl("large*", rn), "Pr(>|z|)"]
#   betaNew[grepl("small*", rn), "Estimate"] <- richBeta[grepl("small*", rn), "Estimate"]
#   betaNew[grepl("small*", rn), "Pr(>|z|)"] <- richBeta[grepl("small*", rn), "Pr(>|z|)"]
#   getProbsInt(X, model.matrix(X), beta = betaNew)[["unitPrice"]]
# }
# NSUnitPrice <- lapply(r, getNSUnitPrice)

# All regs and no storage
getNSAR <- function(X) {
  mat <- model.matrix(X)
  mat[, "unitRegPP"] <- mat[, "unitPricePP"]
  betaNew <- as.data.table(summary(X)$CoefTable, keep.rownames = TRUE)
  betaNew[grepl("large*", rn), "Estimate"] <- richBeta[grepl("large*", rn), "Estimate"]
  betaNew[grepl("large*", rn), "Pr(>|z|)"] <- richBeta[grepl("large*", rn), "Pr(>|z|)"]
  betaNew[grepl("small*", rn), "Estimate"] <- richBeta[grepl("small*", rn), "Estimate"]
  betaNew[grepl("small*", rn), "Pr(>|z|)"] <- richBeta[grepl("small*", rn), "Pr(>|z|)"]
  getProbsInt(X, mat, beta = betaNew)[["probs"]]
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
  getProbsInt(X, mat, beta = betaNew)[["unitPrice"]]
}
NSARUnitPrice <- lapply(r, getNSARUnitPrice)

# Summary Table
avgSheetTable <- merge(actualSheets, baseCaseSheets, by = "Income")
avgSheetTable <- merge(avgSheetTable, allRegsSheets, by = "Income")
# avgSheetTable <- merge(avgSheetTable, noStorageSheets, by = "Income")
avgSheetTable <- merge(avgSheetTable, noStorageAllRegsSheets, by = "Income")
setorder(avgSheetTable, Income)
stargazer(avgSheetTable, summary = FALSE, type = "text", digits = 0)
print(avgSheetTable / (57 * 2))
# Saving in counterfactualMNLDays_Random.tex
# Save actual and base in modelFit_Random.tex

# Summary of Unit prices
unitPriceTable <- rbindlist(list(baseCaseUnitPrice, allRegsUnitPrice, NSARUnitPrice))
setnames(unitPriceTable, c("<25k", "25-50k", "50-100k", ">100k"))
unitPriceTable <- as.data.table(t(unitPriceTable), keep.rownames = TRUE)
setnames(unitPriceTable, c("Income", "Base", "allRegs", "noStorageAllRegs"))
print(unitPriceTable)
# Saving in counterfactualMNLPrice_Random.tex


# Finding where people shift their purchases
purch <- merge(baseCase, allRegs, by = c("Income", "brand_descr", "rolls", "sheets"))
setnames(purch, c("predictedShare.x", "predictedShare.y"), c("baseCase", "allRegs"))
purch <- merge(purch, noStorage, by = c("Income", "brand_descr", "rolls", "sheets"))
setnames(purch, c("predictedShare"), c("noStorage"))
purch <- merge(purch, noStorageAllRegs, by = c("Income", "brand_descr", "rolls", "sheets"))
setnames(purch, c("predictedShare"), c("noStorageAllRegs"))


################# ELASTICITIES #################################################
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
ggsave(filename = "./figures/elasticity2016_Random.pdf", height = 4, width = 6)
