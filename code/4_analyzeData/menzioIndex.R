#THIS EXCLUDES ANY ANALYSIS USING NIELSEN'S PROJECTION FACTORS
##############################################################

library(data.table)
library(zoo)
library(purrr)
library(Hmisc)
library(ggplot2)
library(plotly)
library(forcats)
yr <- 2004:2009

getMenz <- function(yr) {
  print(yr)

  # Getting products
  purchase <- fread(paste0("/home/mallick/Desktop/Nielsen/Data/Consumer_Panel_Data",
                           "_2004-2016/nielsen_extracts/HMS/", yr, "/Annual_Files/",
                           "purchases_", yr, ".tsv"), key = c("upc", "upc_ver_uc"))
  purchase[, ':=' (p = (total_price_paid - coupon_value) / quantity,
                   coupShare = coupon_value / total_price_paid)]
  purchase <- purchase[p > 0 & coupShare <= coupon]
  purchase[, c("total_price_paid", "coupon_value", "deal_flag_uc", "coupShare") := NULL]
  purchase <- merge(purchase, prod, by = c("upc", "upc_ver_uc"))[, "department_code" := NULL]
  setkey(purchase, trip_code_uc)


  # Merging Everything
  menz <- merge(trip, panel, by = c("household_code", "panel_year"))
  setkey(menz, "trip_code_uc")
  menz <- merge(menz, purchase, by = "trip_code_uc")
  setcolorder(menz, c("upc", "upc_ver_uc", "trip_code_uc", "household_code",
                      "panel_year", "market", "quarter", "store_code_uc",
                      "retailer_code", "quantity", "p"))
  setkey(menz, quarter, market, upc, upc_ver_uc)

  # Excluding products with limited transactions
  menz[, "Ntrans" := .N, by = .(quarter, market, upc, upc_ver_uc)]
  menz <- menz[Ntrans >= minTrans][, "Ntrans" := NULL]

  # Constructing normalized prices
  menz[, "Pbar" := sum(p * quantity) / sum(quantity),
       by = .(quarter, market, upc, upc_ver_uc)]
  menz[, "normP" := p / Pbar]

  menz[, "CV" := sd(normP) / mean(normP), by = .(quarter, market, upc, upc_ver_uc)]
  menz <- menz[CV < CVMax]
  menz[, "CV" := NULL]
  return(menz)
}

menz <- rbindlist(map(yr, getMenz), use.names = TRUE, fill = TRUE)
rm(prod)

# Figure 1
mn <- menz[market == "Minneapolis" & quarter == "2007 Q1"]
heinz <- mn[upc == 1300000133]
hist(heinz$p, breaks = 30, xlim = c(0, 5))

# Table 2
tab2 <- menz[, .(sd = sd(normP),
                 q90 = quantile(normP, probs = 0.9),
                 q50 = quantile(normP, probs = 0.5),
                 q10 = quantile(normP, probs = 0.1),
                 expend = sum(p * quantity),
                 normP = mean(normP)),
             by = .(quarter, market, upc, upc_ver_uc)]
tab2[, ':=' (rat9010 = q90 / q10,
             rat9050 = q90 / q50,
             rat5010 = q50 / q10)]

tab2[, .(sd = weighted.mean(sd, w = expend),
         rat9010 = weighted.mean(rat9010, w = expend),
         rat9050 = weighted.mean(rat9050, w = expend),
         rat5010 = weighted.mean(rat5010, w = expend))]
tab2[normP > 2, "normP" := 2]

# Figure 2
histNormMean <- wtd.mean(tab2$normP, weights = tab2$expend)
histNormSD <- sqrt(wtd.var(tab2$normP, weights = tab2$expend))

fig2 <- ggplot(tab2, aes(normP, weight = expend / sum(expend))) + geom_histogram(binwidth = 0.02)
ggplotly(fig2) %>%
  add_lines(x = seq(0, 2, 0.01),
            y = dnorm(seq(0, 2, 0.01), mean = histNormMean, sd = histNormSD)) %>%
  layout(xaxis = list(range = c(0, 2)))
rm(fig2)

# Variance Decomp
menz[, "mu_jmt" := sum(normP * quantity) / sum(quantity),
     by = .(quarter, market, upc, upc_ver_uc)]
menz[, ':=' (mu_jst = sum(normP * quantity) / sum(quantity),
             R = sum(p * quantity)),
     by = .(quarter, store_code_uc, upc, upc_ver_uc)]
menz[, "mu_st" := sum(mu_jst * R) / sum(R),
     by = .(quarter, store_code_uc)]
menz[, ':=' (store = mu_st - mu_jmt,
             product = mu_jst - mu_st,
             trans = normP - mu_jst)]

prodVar <- menz[, .(pVar = wtd.var(normP, weights = quantity),
                    storeVar = wtd.var(store, weights = quantity),
                    prodVar = wtd.var(product, weights = quantity),
                    transVar = wtd.var(trans, weights = quantity),
                    storeProdCov = cov.wt(cbind(store, product), wt = quantity)[1, 2],
                    storeTransCov = cov.wt(cbind(store, trans), wt = quantity)[1, 2],
                    prodTransCov = cov.wt(cbind(product, trans), wt = quantity)[1, 2],
                    expend = sum(p * quantity)),
                by = .(quarter, market, upc, upc_ver_uc)]

prodVar[, ':=' (storeShare = storeVar / pVar * 100,
                         prodShare = prodVar / pVar * 100,
                         transShare = transVar / pVar * 100,
                         storeProdShare = storeProdCov / pVar * 100,
                         storeTransShare = storeTransCov / pVar * 100,
                         prodTransShare = prodTransCov / pVar * 100)]
prodVar[pVar != 0, .(store = weighted.mean(storeShare, w = expend),
            product = weighted.mean(prodShare, w = expend),
            trans = weighted.mean(transShare, w = expend),
            storeProd = weighted.mean(storeProdShare, w = expend),
            storeTrans = weighted.mean(storeTransShare, w = expend),
            prodTrans = weighted.mean(prodTransShare, w = expend))]


# Section 4: Household stuff
hh <- menz[, .(x = sum(p * quantity),
               xbar = sum(Pbar * quantity)),
           by = .(quarter, market, household_code, panel_year)]
hh[, "p" := x / xbar]

# Figure 7
mn <- hh[market == "Minneapolis" & quarter == "2007 Q1"]
plot_ly(data = mn, x = ~p, type = "histogram", histnorm = "probability",
        nbinsx = 30) %>%
  layout(xaxis = list(range = c(0.5, 1.5), dtick = 0.25),
         yaxis = list(range = c(0, 0.3)))

# Table 6
tab6 <- hh[, .(sd = sd(p),
               q90 = quantile(p, probs = 0.9),
               q50 = quantile(p, probs = 0.5),
               q10 = quantile(p, probs = 0.1),
               expend = sum(x)),
           by = .(quarter, market)]
tab6[, ':=' (rat9010 = q90 / q10,
             rat9050 = q90 / q50,
             rat5010 = q50 / q10)]
tab6[, .(sd = weighted.mean(sd, w = expend),
         rat9010 = weighted.mean(rat9010, w = expend),
         rat9050 = weighted.mean(rat9050, w = expend),
         rat5010 = weighted.mean(rat5010, w = expend))]

# HH Variance Decomp
hh <- menz[, .(household_code, panel_year, upc, upc_ver_uc, trip_code_uc,
               market, quarter, Pbar, quantity, mu_jmt, mu_st, mu_jst, normP, p)]
hh[, "xbar" := sum(Pbar * quantity), by = .(household_code, panel_year, quarter)]
hh[, "omega" := Pbar * quantity / xbar]
hhPI <- hh[, .(marketAvg = sum(mu_jmt * omega),
               store = sum((mu_st - mu_jmt) * omega),
               product = sum((mu_jst - mu_st) * omega),
               trans = sum((normP - mu_jst) * omega),
               expend = sum(p * quantity)),
           by = .(household_code, panel_year, quarter, market)]
hhPI[, "index" := .(marketAvg + store + product + trans)]

hhVar <- hhPI[, .(pVar = var(index),
                  storeVar = var(store),
                  productVar = var(product),
                  transVar = var(trans),
                  storeProdCov = cov(store, product),
                  storeTransCov = cov(store, trans),
                  prodTransCov = cov(product, trans),
                  expend = sum(expend)),
              by = .(quarter, market)]
hhVar[, .(store = weighted.mean(storeVar / pVar * 100, w = expend),
          product = weighted.mean(productVar / pVar * 100, w = expend),
          trans = weighted.mean(transVar / pVar * 100, w = expend),
          storeProd = weighted.mean(storeProdCov / pVar * 100, w = expend),
          storeTrans = weighted.mean(storeTransCov / pVar * 100, w = expend),
          prodTrans = weighted.mean(prodTransCov / pVar * 100, w = expend))]


# Figure 9
hhPI[, ':=' (male_head_age = panel_year - male_head_birth,
             female_head_age = panel_year - female_head_birth)]
hhPI[, ':=' (age = (male_head_age + female_head_age) / 2)]
hhPI[is.na(age), "age" := as.numeric(male_head_age)]
hhPI[is.na(age), "age" := as.numeric(female_head_age)]
hhPI[, "ageFactor" := cut(age, breaks = c(0, 24, 29, 34, 39, 44, 49, 54, 150),
                          labels = c("<25", "25-29", "30-34", "35-39", "40-44",
                                     "45-49", "50-54", "55+"),
                          ordered_result = TRUE)]
graphData <- hhPI[, .(index = weighted.mean(index, w = projection_factor)),
                  by = ageFactor]
graphData[, "base" := graphData[ageFactor == "<25"]$index]
graphData[, "normInd" := (index - base) / base]
plot_ly(data = graphData, x = ~ageFactor) %>%
  add_lines(y = ~normInd) %>%
  layout(yaxis = list(range = c(-0.05, 0)))

# Redoing Figure 9 by income
hhPI[, "incomeFactor" := fct_collapse(as.factor(household_income),
                                      "<25k" = paste(c(3, 4, 6, 8, 10, 11, 13)),
                                      "<60k" = paste(c(15, 16, 17, 18, 19, 21)),
                                      "<100k" = paste(c(23, 26)),
                                      ">100k" = paste(c(27, 28, 29, 30)))]
graphData <- hhPI[, .(index = weighted.mean(index, w = projection_factor)),
                  by = .(incomeFactor, quarter)]
plot_ly(data = graphData, x = ~quarter) %>%
  add_lines(y = ~index, split = ~incomeFactor)

