library(data.table)
library(lfe)
library(stargazer)
library(ggplot2)
library(ggthemes)
threads <- 8


################################################################################
# ANALYSIS OF TOILET PAPER SHOPPING ON CHOICE SUBSAMPLE
################################################################################
fullPurch <- fread("/scratch/upenn/hossaine/fullTPPurchases.csv", nThread = threads)
panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads)
prod <- fread("/scratch/upenn/hossaine/prodTP.csv")

# Isolating to grocery and discount stores
fullPurch <- merge(fullPurch, prod, by.x = c("upc_choice", "upc_ver_uc_choice"),
                   by.y = c("upc", "upc_ver_uc"))
fullPurch <- merge(fullPurch, panel, by = c("household_code", "panel_year"))
fullPurch[, "Household Income" := factor(household_income_coarse,
                                         levels = c("<25k", "25-50k", "50-100k", ">100k"),
                                         ordered = TRUE)]
fullPurch[, "retailYear" := paste(retailer_code, panel_year, sep = "_")]
fullPurch[, "brandRetailYear" := paste(brand_code_uc, retailer_code, panel_year, sep = "_")]
fullPurch[, "zipYear" := paste(zip_code, panel_year, sep = "_")]
fullPurch[, "brandZipYear" := paste(brand_code_uc, zip_code, panel_year, sep = "_")]
fullPurch[, "household_income" := as.factor(household_income)]

# Bulk buying by rolls
reg1 <- felm(log(totalAmount) ~ household_income + men + women + nChildren + age |
               married + child + college,
             data = fullPurch, weights = fullPurch$projection_factor)
reg2 <- felm(log(totalAmount) ~ household_income + men + women + nChildren + age |
               married + child + college + brand_code_uc + retailYear,
             data = fullPurch, weights = fullPurch$projection_factor)
reg3 <- felm(log(totalAmount) ~ household_income + men + women + nChildren + age |
               married + child + college + brand_code_uc + brandRetailYear,
             data = fullPurch, weights = fullPurch$projection_factor)
reg4 <- felm(log(totalAmount) ~ household_income + men + women + nChildren + age |
               married + child + college + brand_code_uc + zipYear,
             data = fullPurch, weights = fullPurch$projection_factor)
reg5 <- felm(log(totalAmount) ~ household_income + men + women + nChildren + age |
               married + child + college + brand_code_uc + brandZipYear,
             data = fullPurch, weights = fullPurch$projection_factor)
stargazer(reg1, reg2, reg3, reg4, reg5, type = "text", omit.stat = c("f", "ser"))

# Bulk buying by sheets
reg1 <- felm(log(totalSheet) ~ household_income + men + women + nChildren + age |
               married + child + college,
             data = fullPurch, weights = fullPurch$projection_factor)
reg2 <- felm(log(totalSheet) ~ household_income + men + women + nChildren + age |
               married + child + college + brand_code_uc + retailYear,
             data = fullPurch, weights = fullPurch$projection_factor)
reg3 <- felm(log(totalSheet) ~ household_income + men + women + nChildren + age |
               married + child + college + brand_code_uc + brandRetailYear,
             data = fullPurch, weights = fullPurch$projection_factor)
reg4 <- felm(log(totalSheet) ~ household_income + men + women + nChildren + age |
               married + child + college + brand_code_uc + zipYear,
             data = fullPurch, weights = fullPurch$projection_factor)
reg5 <- felm(log(totalSheet) ~ household_income + men + women + nChildren + age |
               married + child + college + brand_code_uc + brandZipYear,
             data = fullPurch, weights = fullPurch$projection_factor)
stargazer(reg1, reg2, reg3, reg4, reg5, type = "text", omit.stat = c("f", "ser"))

# Getting coefficients

# Graphing sheet disparity
# Organizing graph data
finalCoefs <- rbindlist(list(as.data.table(summary(reg1)$coefficients, keep.rownames = TRUE),
                             as.data.table(summary(reg2)$coefficients, keep.rownames = TRUE),
                             as.data.table(summary(reg3)$coefficients, keep.rownames = TRUE),
                             as.data.table(summary(reg4)$coefficients, keep.rownames = TRUE),
                             as.data.table(summary(reg5)$coefficients, keep.rownames = TRUE)))
graphData <- finalCoefs[grepl("household_income", rn)]
graphData[, "rn" := gsub("household_income", "", rn)]
graphData[, "rn" := factor(rn, levels = c(4, 6, 8, 10, 11, 13, 15, 16, 17, 18, 19, 21, 23, 26, 27),
                           labels = c(6.5, 9, 11, 13.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 55, 65, 85, 100),
                           ordered = TRUE)]
graphData[, "rn" := as.numeric(as.character(rn))]
setnames(graphData, c("rn", "beta", "se", "t", "p", "LCL", "UCL", "disc", "Product Type"))
