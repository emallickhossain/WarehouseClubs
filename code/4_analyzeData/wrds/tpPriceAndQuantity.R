# Examines whether TP prices vary by quantity or size and the relative coefs
# This just uses fullTPAssortment data from randomCoefficientsPrepv2.R
library(data.table)
library(lfe)
library(stargazer)
threads <- 8

fullTP <- fread("/scratch/upenn/hossaine/fullChoice.csv", nThread = threads,
                select = c("store_code_uc", "week_end", "brand_code_uc",
                           "price", "rolls", "totalSheet"))

fullTP[, ':=' (logP = log(price),
               logQ = log(totalSheet),
               logRoll = log(rolls),
               storeWeekBrand = paste(store_code_uc, week_end,
                                      brand_code_uc, sep = "_"))]

reg1 <- felm(logP ~ logQ + logRoll, data = fullTP)
reg2 <- felm(logP ~ logQ + logRoll | store_code_uc, data = fullTP)
reg3 <- felm(logP ~ logQ + logRoll | store_code_uc + brand_code_uc, data = fullTP)
reg4 <- felm(logP ~ logQ + logRoll | storeWeekBrand, data = fullTP)

stargazer(reg1, reg2, reg3, reg4, type = "text")
