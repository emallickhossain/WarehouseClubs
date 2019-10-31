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
# ids <- hhInc[panel_year == 2016, .SD[sample(.N, 50)],
#              by = household_income_coarse]$household_code

tpSub <- tp[panel_year == 2016]
tpML <- mlogit.data(tpSub, choice = "choice", shape = "long",
                    alt.var = "brandRollSheet", chid.var = "trip_code_uc")

for(incBin in c("<25k", "25-50k", "50-100k", ">100k")) {
  print(incBin)
  # Running MNL model
  # Creating mlogit data for analysis
  reg1 <- mlogit(choice ~ price +
                   unitPrice + unitReg +
                   lDays + large + largeHome + small + smallHome + brand_descr + 0,
                 data = tpML[tpML$household_income_coarse == incBin, ])
  stargazer(reg1, reg2, reg3, type = "text")
  print(lrtest(reg1, reg2, reg3))
  save(reg3, file = paste0("/scratch/upenn/hossaine/mlogit/newModel2016OnlyRand/",
                           "mlogit", incBin, "2016.rda"), compress = TRUE)
}
