library(data.table)
library(bayesm)
data("cheese")
dat <- list(y = log(cheese$VOLUME), X = model.matrix(~ PRICE + DISP, data = cheese))
out <- runireg(Data = dat, Mcmc = list(R = 1e4, nprint = 1e3))
summary(out$betadraw)
plot.bayesm.mat(out$betadraw[, 2])

data("margarine")
marg <- merge(margarine$choicePrice, margarine$demos, by = "hhid")
y <- marg[, 2]
X1 <- createX(p = 10, na = 1, Xa = marg[, 3:12], nd = NULL, Xd = NULL, base = 1)
colnames(X1) <- c(names(marg)[3:11], "price")
X2 <- createX(p = 10, na = NULL, Xa = NULL, nd = 2, Xd = as.matrix(marg[, c(13, 16)]), base = 1)
X <- cbind(X1, X2[, 10:ncol(X2)])
out <- rmnlIndepMetrop(Data = list(y = y, X = X, p = 10), Mcmc = list(R = 1e4, nprint = 1e3))

data("camera")
data <- list(lgtdata = camera, p = 5)
prior <- list(ncomp = 1)
mcmc <- list(R = 1e4, nprint = 0)
out <- rhierMnlRwMixture(Data = data, Prior = prior, Mcmc = mcmc)

# Bank Example
data("bank")
choiceAtt <- setDT(bank$choiceAtt)
Z <- setDT(bank$demo)
Z[, "id" := 1]
Z[, "age" := age - mean(age)]
Z[, "income" := income - mean(income)]
Z[, "gender" := gender - mean(gender)]
Z <- as.matrix(Z)
hh <- unique(choiceAtt$id)
nhh <- uniqueN(choiceAtt$id)

lgtdata=NULL
for (i in 1:nhh) {
  y <- as.matrix(choiceAtt[id == hh[i]]$choice)
  nobs <- length(y)
  X <- as.matrix(choiceAtt[id == hh[i] , c(3:16)])
  lgtdata[[i]] <- list(y = y, X = X)
}

Data <- list(lgtdata = lgtdata, Z = Z)
Mcmc <- list(R = 1e3, sbeta = 0.2)
out <- rhierBinLogit(Data = Data, Mcmc = Mcmc)
