# Calculate price decomposition
library(data.table)
library(ggplot2)
library(ggthemes)
library(stringr)
library(lfe)
threads <- 8
yrs <- 2004:2017

# Getting CPI to deflate values to Jan-2010 base
cpi <- fread("/scratch/upenn/hossaine/cpi.csv")
cpi[, "base" := cpi[date == "2010-01-01"]$value]
cpi[, "newIndex" := value / base * 100]
cpi[, c("base", "value") := NULL]
cpi[, "yearMonth" := .(substr(date, 1, 7))]
cpi[, "date" := NULL]

# Loading Nielsen data (no years)
trips <- fread("/scratch/upenn/hossaine/fullTrips.csv", nThread = threads,
               select = c("retailer_code", "household_code", "panel_year",
                          "trip_code_uc", "purchase_date"),
               key = "trip_code_uc")
trips[, "yearMonth" := substr(purchase_date, 1, 7)][, "purchase_date" := NULL]

panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("household_code", "panel_year", "projection_factor",
                          "household_income", "household_income_coarse",
                          "household_size", "age", "child", "married"))

# Loading Nielsen with years
fullPurch <- NULL
for (i in yrs) {
  print(i)
  purch <- fread(paste0("/scratch/upenn/hossaine/fullPurch", i, ".csv"),
                 nThread = threads, key = "trip_code_uc",
                 select = c("trip_code_uc", "quantity", "packagePrice",
                            "product_module_code", "upc_descr", "brand_code_uc",
                            "totalAmount", "multi", "brand_descr",
                            "size1_amount"))[product_module_code == 7260]
  fullPurch <- rbindlist(list(fullPurch, purch), use.names = TRUE)
}

# Getting rolls and sheets for each product
fullPurch[, "rolls" := as.integer(multi * size1_amount)]
fullPurch[, "ply" := str_extract_all(upc_descr, "\\s\\dP\\s")]
fullPurch[, "ply" := as.integer(gsub("P", "", ply))]
fullPurch[, "sheet" := str_extract_all(upc_descr, "\\d{2,}S\\s")]
fullPurch[, "sheet" := as.integer(gsub("S", "", sheet)) * ply]
fullPurch[, "totalSheet" := sheet * rolls]
fullPurch[, c("multi", "size1_amount", "ply", "product_module_code",
              "upc_descr", "totalAmount", "rolls", "sheet") := NULL]

# Merging with trips and panel to get store, purchase date, and household info
mergedData <- merge(trips, fullPurch, by = "trip_code_uc")[, "trip_code_uc" := NULL]
mergedData <- merge(mergedData, panel, by = c("household_code", "panel_year"))
mergedData <- merge(mergedData, cpi, by = "yearMonth")[, "yearMonth" := NULL]
rm(purch)

# Deflating
mergedData[, ':=' (packagePriceReal = packagePrice / newIndex * 100)]
mergedData[, c("packagePrice", "newIndex") := NULL]
mergedData[, ':=' (unitPriceReal = packagePriceReal / totalSheet,
                   totalSpendReal = packagePriceReal * quantity)]

# Generating factors
# Reference group is the 2nd quantile generic brand sold at the
# most popular discount retailer
mergedData[, "brand_code_uc" := as.character(brand_code_uc)]
mergedData[brand_code_uc == "536746",
           "brand_code_uc" := paste0(retailer_code, brand_code_uc)]
mergedData[, "brand_code_uc" := relevel(as.factor(brand_code_uc), ref = "6920536746")]
mergedData[, "retailer_code" := relevel(as.factor(retailer_code), ref = "6920")]
mergedData[, c("lUnitPrice", "lQ") := .(log(unitPriceReal), log(totalSheet))]

# Decomposing unit prices
fullCoefs <- NULL
for (i in yrs) {
  print(i)
  reg <- lm(lUnitPrice ~ lQ + brand_code_uc + retailer_code,
            data = mergedData[panel_year == i])
  print(summary(reg)$adj.r)
  coefs <- as.data.table(summary(reg)$coefficients, keep.rownames = TRUE)
  coefs[, "panel_year" := i]
  fullCoefs <- rbindlist(list(fullCoefs, coefs), use.names = TRUE)
}

# Organizing effects
setnames(fullCoefs, c("rn", "beta", "se", "t", "p", "panel_year"))
fullCoefs[p > 0.05, "beta" := 0]

# Getting intercept
int <- fullCoefs[rn == "(Intercept)", .(int = beta, panel_year)]

# Getting slope
slope <- fullCoefs[rn == "lQ", .(slope = beta, panel_year)]

# Getting effects
fullCoefs[, c("type", "id") := tstrsplit(rn, "_code", fixed = TRUE)]
fullCoefs[, "id" := gsub("_uc", "", id)]

brandEffect <- fullCoefs[type == "brand"][, c("rn", "type", "se", "t", "p") := NULL]
setnames(brandEffect, c("brandBeta", "panel_year", "brand_code_uc"))

retailEffect <- fullCoefs[type == "retailer"][, c("rn", "type", "se", "t", "p") := NULL]
setnames(retailEffect, c("retailBeta", "panel_year", "retailer_code"))

# Combining everything into the data table
mergedData <- merge(mergedData, int, by = "panel_year")
mergedData <- merge(mergedData, slope, by = "panel_year")
mergedData[, "sizeBeta" := lQ * slope]
mergedData <- merge(mergedData, brandEffect, by = c("brand_code_uc", "panel_year"),
                    all.x = TRUE)
mergedData <- merge(mergedData, retailEffect, by = c("retailer_code", "panel_year"),
                    all.x = TRUE)
mergedData[is.na(brandBeta), "brandBeta" := 0]
mergedData[is.na(retailBeta), "retailBeta" := 0]

fwrite(mergedData, "/scratch/upenn/hossaine/mergedData.csv", nThread = threads)

# Regressing to get difference in components attributable to income
mergedData <- na.omit(fread("/scratch/upenn/hossaine/mergedData.csv", nThread = threads))
mergedData <- mergedData[, lapply(.SD, weighted.mean, w = totalSpendReal),
                         .SDcols = c("brandBeta", "sizeBeta", "retailBeta",
                                     "lUnitPrice", "int"),
                         by = .(household_code, panel_year, projection_factor,
                                household_income_coarse, household_size, age,
                                child, married)]
mergedData[, "residual" := lUnitPrice - (sizeBeta + brandBeta + retailBeta + int)]
reg0 <- felm(lUnitPrice ~ household_income_coarse +
               household_size + age + child + married | panel_year,
             data = mergedData,
             weights = mergedData$projection_factor)
reg1 <- felm(brandBeta ~ household_income_coarse +
               household_size + age + child + married | panel_year,
           data = mergedData,
           weights = mergedData$projection_factor)
reg2 <- felm(retailBeta ~ household_income_coarse +
               household_size + age + child + married | panel_year,
             data = mergedData,
             weights = mergedData$projection_factor)
reg3 <- felm(sizeBeta ~ household_income_coarse +
               household_size + age + child + married | panel_year,
             data = mergedData,
             weights = mergedData$projection_factor)
reg4 <- felm(residual ~ household_income_coarse +
               household_size + age + child + married | panel_year,
             data = mergedData,
             weights = mergedData$projection_factor)
stargazer(reg0, reg1, reg2, reg3, reg4, type = "text",
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("Unit Price", "Brand", "Retailer", "Size", "Residual"),
          column.separate = c(1, 1, 1, 1, 1),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          keep = c("household_income*"),
          order = c(2, 3, 1),
          covariate.labels = c("25-50k", "50-100k", ">100k"),
          notes.align = "l",
          digits = 3,
          notes.append = TRUE,
          label = "tab:tpPriceDecomp",
          out = "tables/tpPriceDecomp.tex")

# Getting coefficients and confidence intervals to plot
coef0 <- as.data.table(summary(reg0)$coefficients, keep.rownames = TRUE)
coef0SE <- as.data.table(confint(reg0), keep.rownames = TRUE)
coef0 <- merge(coef0, coef0SE, by = "rn")
setnames(coef0, c("rn", "beta", "se", "t", "p", "LCL", "UCL"))

coef1 <- as.data.table(summary(reg1)$coefficients, keep.rownames = TRUE)
coef1SE <- as.data.table(confint(reg1), keep.rownames = TRUE)
coef1 <- merge(coef1, coef1SE, by = "rn")
setnames(coef1, c("rn", "beta", "se", "t", "p", "LCL", "UCL"))
coef1[, "type" := "Brand"]

coef2 <- as.data.table(summary(reg2)$coefficients, keep.rownames = TRUE)
coef2SE <- as.data.table(confint(reg2), keep.rownames = TRUE)
coef2 <- merge(coef2, coef2SE, by = "rn")
setnames(coef2, c("rn", "beta", "se", "t", "p", "LCL", "UCL"))
coef2[, "type" := "Retailer"]

coef3 <- as.data.table(summary(reg3)$coefficients, keep.rownames = TRUE)
coef3SE <- as.data.table(confint(reg3), keep.rownames = TRUE)
coef3 <- merge(coef3, coef3SE, by = "rn")
setnames(coef3, c("rn", "beta", "se", "t", "p", "LCL", "UCL"))
coef3[, "type" := "Size"]

coef4 <- as.data.table(summary(reg4)$coefficients, keep.rownames = TRUE)
coef4SE <- as.data.table(confint(reg4), keep.rownames = TRUE)
coef4 <- merge(coef4, coef4SE, by = "rn")
setnames(coef4, c("rn", "beta", "se", "t", "p", "LCL", "UCL"))
coef4[, "type" := "Residual"]

graphData <- rbindlist(list(coef1, coef2, coef3, coef4), use.names = TRUE)
graphData <- graphData[grepl("household_income", rn)]
graphData[, "rn" := gsub("household_income_coarse", "", rn)]
graphData[, "rn" := factor(rn, levels = c("25-50k", "50-100k", ">100k"),
                           ordered = TRUE)]

coef0 <- coef0[grepl("household_income", rn)]
coef0[, "rn" := gsub("household_income_coarse", "", rn)]
coef0[, "rn" := factor(rn, levels = c("25-50k", "50-100k", ">100k"), ordered = TRUE)]

# Graphing
ggplot() +
  geom_bar(data = graphData, position = "stack", stat = "identity",
           aes(x = rn, y = beta * 100, fill = type)) +
  # geom_errorbar(data = graphData, aes(x = rn,
  #                                     ymin = LCL * 100,
  #                                     ymax = UCL * 100), width = 0.05) +
  geom_point(data = coef0, aes(x = rn, y = beta * 100)) +
  # geom_errorbar(data = coef0, aes(x = rn,
  #                                 ymin = LCL * 100,
  #                                 ymax = UCL * 100), width = 0.05) +
  geom_hline(yintercept = 0) +
  labs(x = "Household Income",
       y = "Unit Price Difference (%)",
       fill = "Component") +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom") +
  scale_fill_grey()
ggsave(filename = "./figures/tpPriceDecomp.png", height = 4, width = 6)
