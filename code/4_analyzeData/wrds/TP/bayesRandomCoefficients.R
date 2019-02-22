# Estimates random coefficients with Bayesian framework
library(bayesm)
library(data.table)

fullChoice <- setDT(fread("/scratch/upenn/hossaine/TPMLogit.csv"))

# Counting number of trips with full matches
totalTrips <- fullChoice[, .(trips = sum(choice)), by = household_code]
quantile(totalTrips$trips, seq(0, 1, 0.01))

fullChoice[, "choiceid" := ifelse(choice == 1, id, 0)]
fullChoice[, "choiceid" := max(choiceid), by = trip_code_uc]

fullChoice <- dcast(fullChoice, household_code + panel_year + trip_code_uc + choiceid ~ alt,
                    value.var = c("unitCost", "pCents", "ply", "pkgSize"))
fullChoice <- merge(fullChoice, fullPanel, by = c("household_code", "panel_year"))

# Converting data to list of lists for bayesm
fullChoice <- fread("/scratch/upenn/hossaine/2010MLogit.csv")
ids <- unique(fullChoice$household_code)
fullChoiceHier <- vector(mode = "list", length = length(ids))

Z <- unique(fullChoice[, .(household_code, household_income, hispanic_origin,
                           college, white, child)])[1:testSize]
Z[, "household_code" := NULL]
Z <- Z[, lapply(.SD, function (x) x - mean(x))]
Z <- as.matrix(Z)

for (i in 1:length(ids)) {
  tmpDat <- fullChoice[household_code == ids[i]]
  fullChoiceHier[[i]]$y <- tmpDat$choiceid
  fullChoiceHier[[i]]$X <- createX(p = 42, na = 3, nd = NULL, Xd = NULL,
                                   Xa = tmpDat[, 47:172])
}

data <- list(lgtdata = fullChoiceHier[1:testSize], Z = Z[1:testSize, ], p = 42)
prior <- list(ncomp = 1)
mcmc <- list(R = 1e3, nprint = 1e2)
out <- rhierMnlRwMixture(data, prior, mcmc)

hist(apply(out$betadraw, 1:2, mean)[, 42], col = "dodgerblue4",
     xlab = "", ylab = "", yaxt="n", breaks = 20,
     main = "Histogram of Posterior Means For Individual Price Coefs")

