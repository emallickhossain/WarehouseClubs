# Step 1: Estimates mlogit model with random coefficients
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
                          "married", "college", "projection_factor", "law", "age"))
panel[, "lawInd" := (law >= 3)]
tp <- merge(tp, panel, by = c("household_code", "panel_year"))

# Getting product characteristics
tp[, c("brand_descr", "rolls", "sheets") := tstrsplit(brandRollSheet, "_", fixed = TRUE)]
tp[, "rolls" := as.integer(rolls)]
tp[, "sheets" := as.integer(sheets)]

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
                brand_descr, rolls, sheets, projection_factor, college, lawInd)]
tp[, "unitPrice" := price / sheets * (100 * 2)]

# Generating price interactions
tp[, "pAdults"   := unitPrice * adults]
tp[, "pChildren" := unitPrice * nChildren]
tp[, "pAge"      := unitPrice * age]
tp[, "pMarried"  := unitPrice * married]
tp[, "pCollege"  := unitPrice * college]
tp[, "pReg"      := unitPrice * lawInd]

# Generating size interactions
tp[, "large"       := as.integer(rolls > 12)]
tp[, "largeAdults"   := large * adults]
tp[, "largeChildren" := large * nChildren]
tp[, "largeAge"      := large * age]
tp[, "largeMarried"  := large * married]

tp[, "small"       := as.integer(rolls < 12)]
tp[, "smallAdults"   := small * adults]
tp[, "smallChildren" := small * nChildren]
tp[, "smallAge"      := small * age]
tp[, "smallMarried"  := small * married]

# Coding package sizes and brands
tp[, "brand_descr" := relevel(as.factor(brand_descr), ref = "SCOTT 1000")]

# Running in parallel on years
registerDoParallel()
getDoParWorkers()

# hhInc <- unique(tp[, .(household_code, panel_year, household_income_coarse)])
# ids <- hhInc[panel_year == 2016, .SD[sample(.N, 50)],
#              by = household_income_coarse]$household_code

tpSub <- tp[panel_year == 2016]
tpML <- mlogit.data(tpSub, choice = "choice", shape = "long",
                    alt.var = "brandRollSheet", chid.var = "trip_code_uc")

for(incBin in c("<25k", "25-50k", "50-100k", ">100k")) {
  print(incBin)
  # Running MNL model
  # Creating mlogit data for analysis
  reg1 <- mlogit(choice ~ unitPrice + pAdults + pChildren + pAge + pMarried +
                   pCollege + pReg +
                   large + largeAdults + largeChildren + largeAge + largeMarried +
                   small + smallAdults + smallChildren + smallAge + smallMarried +
                   brand_descr + 0,
                 data = tpML[tpML$household_income_coarse == incBin, ],
                 rpar = c(unitPrice = "n"),
                 R = 25, halton = NA)
  reg2 <- mlogit(choice ~ unitPrice + pAdults + pChildren + pAge + pMarried +
                   pCollege + pReg +
                   large + largeAdults + largeChildren + largeAge + largeMarried +
                   small + smallAdults + smallChildren + smallAge + smallMarried +
                   brand_descr + 0,
                 data = tpML[tpML$household_income_coarse == incBin, ],
                 rpar = c(unitPrice = "n", small = "n", large = "n"),
                 R = 25, halton = NA)
  reg3 <- mlogit(choice ~ unitPrice + pAdults + pChildren + pAge + pMarried +
                   pCollege + pReg +
                   large + largeAdults + largeChildren + largeAge + largeMarried +
                   small + smallAdults + smallChildren + smallAge + smallMarried +
                   brand_descr + 0,
                 data = tpML[tpML$household_income_coarse == incBin, ],
                 rpar = c(unitPrice = "n", large = "n", small = "n",
                          `brand_descrANGEL SOFT` = "n",
                          `brand_descrCHARMIN` = "n",
                          `brand_descrCTL BR` = "n",
                          `brand_descrKLEENEX COTTONELLE` = "n",
                          `brand_descrQUILTED NORTHERN` = "n"),
                 R = 25, halton = NA)
  stargazer(reg1, reg2, reg3, type = "text")
  print(lrtest(reg1, reg2, reg3))
  save(reg3, file = paste0("/scratch/upenn/hossaine/mlogit/newModel2016OnlyRand/",
                           "mlogit", incBin, "2016.rda"), compress = TRUE)
}
