# Calculate price decomposition
library(data.table)
library(ggplot2)
library(ggthemes)
critValue <- 3
threads <- 8
yr <- 2010

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
                          "household_size", "age", "child", "white"))

# Loading Nielsen with years
purch <- fread(paste0("/scratch/upenn/hossaine/fullPurch", yr, ".csv"),
               nThread = threads, key = "trip_code_uc")

# Merging with trips and panel to get store, purchase date, and household info
mergedData <- merge(trips, purch, by = "trip_code_uc")[, "trip_code_uc" := NULL]
mergedData <- merge(mergedData, panel, by = c("household_code", "panel_year"))
mergedData <- merge(mergedData, cpi, by = "yearMonth")[, "yearMonth" := NULL]
rm(purch)

# Deflating
mergedData[, ':=' (couponValueReal = coupon_value / newIndex * 100,
                   packagePriceReal = packagePrice / newIndex * 100)]
mergedData[, c("coupon_value", "packagePrice", "newIndex") := NULL]
mergedData[, ':=' (unitPriceReal = packagePriceReal / totalAmount,
                   unitCouponReal = couponValueReal / totalAmount,
                   totalSpendReal = (packagePriceReal - couponValueReal) * quantity)]
mergedData[, c("couponValueReal", "packagePriceReal", "totalAmount", "deal_flag_uc") := NULL]

# Generating factors
# Reference group is the 2nd quantile generic brand sold at the
# most popular discount retailer
mergedData[, "brand_code_uc" := as.character(brand_code_uc)]
mergedData[brand_code_uc == "536746", "brand_code_uc" := paste0(retailer_code, brand_code_uc)]
mergedData[, c("brand_code_uc", "retailer_code", "quartile") :=
             .(relevel(as.factor(brand_code_uc), ref = "6920536746"),
               relevel(as.factor(retailer_code), ref = "6920"),
               relevel(as.factor(quartile), ref = "1"))]

# Looping over all product modules which have a generic brand
fullData <- NULL
mods <- sort(unique(mergedData[brand_code_uc == "6920536746" &
                                 retailer_code == "6920"]$product_module_code))
mods <- mods[mods != 8410]
for (modCode in mods) {
  print(modCode)

  # Subsetting to specified module
  prodMod <- mergedData[product_module_code == modCode]

  # Running hedonic regression and organizing coefficients
  reg1 <- lm(unitPriceReal ~ brand_code_uc + retailer_code + quartile, data = prodMod)
  coefs <- as.data.table(summary(reg1)$coefficients, keep.rownames = TRUE)
  coefs[abs(`t value`) < critValue, "Estimate" := 0]

  brands <- coefs[grepl("brand_code_uc", rn), .(rn, Estimate)]
  brands[, c("brand_code_uc", "rn") := .(gsub("brand_code_uc", "", rn), NULL)]
  setnames(brands, "Estimate", "brandComp")

  retailers <- coefs[grepl("retailer_code", rn), .(rn, Estimate)]
  retailers[, c("retailer_code", "rn") := .(gsub("retailer_code", "", rn), NULL)]
  setnames(retailers, "Estimate", "retailComp")

  quartiles <- coefs[grepl("quartile", rn), .(rn, Estimate)]
  quartiles[, c("quartile", "rn") := .(gsub("quartile", "", rn), NULL)]
  setnames(quartiles, "Estimate", "quartComp")

  # Merging in coefficients with purchases
  prodMod[, "Intercept" := coefs[rn == "(Intercept)"]$Estimate]
  prodMod <- merge(prodMod, brands, by = "brand_code_uc", all.x = TRUE)
  prodMod <- merge(prodMod, retailers, by = "retailer_code", all.x = TRUE)
  prodMod <- merge(prodMod, quartiles, by = "quartile", all.x = TRUE)

  # Generating 0s
  prodMod[is.na(brandComp), "brandComp" := 0]
  prodMod[is.na(retailComp), "retailComp" := 0]
  prodMod[is.na(quartComp), "quartComp" := 0]
  prodMod[, "PrivateLabel" := ifelse(grepl("*536746", brand_code_uc), brandComp, 0)]
  prodMod[PrivateLabel > 0, "brandComp" := 0]
  prodMod[, "residual" := (unitPriceReal - Intercept - unitCouponReal -
                             brandComp - retailComp - quartComp - PrivateLabel)]
  prodMod[, "dot" := brandComp + retailComp + quartComp + PrivateLabel +
            residual - unitCouponReal]

  # Averaging components by household
  finalData <- prodMod[, .(brandComp = weighted.mean(brandComp / Intercept, w = totalSpendReal),
                           retailComp = weighted.mean(retailComp / Intercept, w = totalSpendReal),
                           quartComp = weighted.mean(quartComp / Intercept, w = totalSpendReal),
                           couponComp = weighted.mean(-unitCouponReal / Intercept, w = totalSpendReal),
                           PrivateLabel = weighted.mean(PrivateLabel / Intercept, w = totalSpendReal),
                           residual = weighted.mean(residual / Intercept, w = totalSpendReal),
                           dot = weighted.mean(dot / Intercept, w = totalSpendReal),
                           modSpending = sum(totalSpendReal)),
                       by = .(household_code, panel_year, household_income,
                              household_income_coarse, projection_factor,
                              product_module_code)]
  fullData <- rbindlist(list(fullData, finalData), use.names = TRUE)
}

fwrite(fullData, "/scratch/upenn/hossaine/priceDecomp.csv", nThread = threads)

# Computing shares
fullData <- fread("/scratch/upenn/hossaine/priceDecomp.csv", nThread = threads)
fullData[, "household_income_coarse" :=
           factor(household_income_coarse, levels = c("<25k", "25-50k", "50-100k", ">100k"),
                  ordered = TRUE)]
prod <- fread("/scratch/upenn/hossaine/fullProd.csv", nThread = threads)
prod <- unique(prod[, .(product_module_code, storable)])
fullData <- merge(fullData, prod, by = "product_module_code")

hhAvg <- fullData[!is.na(retailComp) & !is.infinite(retailComp),
                  .(brandComp = weighted.mean(brandComp, w = modSpending),
                    retailComp = weighted.mean(retailComp, w = modSpending),
                    quartComp = weighted.mean(quartComp, w = modSpending),
                    couponComp = weighted.mean(couponComp, w = modSpending),
                    PrivateLabel = weighted.mean(PrivateLabel, w = modSpending),
                    residual = weighted.mean(residual, w = modSpending),
                    dot = weighted.mean(dot, w = modSpending)),
                  by = .(household_code, panel_year, household_income,
                         household_income_coarse, projection_factor, storable)]

popAvg <- hhAvg[, .(brandComp = weighted.mean(brandComp, w = projection_factor),
                    retailComp = weighted.mean(retailComp, w = projection_factor),
                    quartComp = weighted.mean(quartComp, w = projection_factor),
                    couponComp = weighted.mean(couponComp, w = projection_factor),
                    PrivateLabel = weighted.mean(PrivateLabel, w = projection_factor),
                    residual = weighted.mean(residual, w = projection_factor),
                    dot = weighted.mean(dot, w = projection_factor)),
                by = .(storable)]

incAvg <- hhAvg[, .(brandComp = weighted.mean(brandComp, w = projection_factor),
                    retailComp = weighted.mean(retailComp, w = projection_factor),
                    quartComp = weighted.mean(quartComp, w = projection_factor),
                    couponComp = weighted.mean(couponComp, w = projection_factor),
                    PrivateLabel = weighted.mean(PrivateLabel, w = projection_factor),
                    residual = weighted.mean(residual, w = projection_factor),
                    dot = weighted.mean(dot, w = projection_factor)),
                keyby = .(household_income_coarse, storable)]
incAvgLong <- melt(incAvg, id.var = c("household_income_coarse", "dot", "storable"),
                   variable.name = "Component")

chartStore <- ggplot(incAvgLong, aes(x = household_income_coarse, y = value, fill = Component)) +
  geom_bar(stat = "identity") +
  geom_point(aes(y = dot)) +
  facet_wrap(vars(storable)) +
  theme_fivethirtyeight()
ggsave("./figures/priceDecomp.png")



plotMod <- function(moduleCode) {
  incAvg <- fullData[product_module_code == moduleCode &
                       !is.na(retailComp) & !is.infinite(retailComp),
                  .(brandComp = weighted.mean(brandComp, w = projection_factor),
                    retailComp = weighted.mean(retailComp, w = projection_factor),
                    quartComp = weighted.mean(quartComp, w = projection_factor),
                    couponComp = weighted.mean(couponComp, w = projection_factor),
                    residual = weighted.mean(residual, w = projection_factor),
                    dot = weighted.mean(dot, w = projection_factor)),
                  keyby = .(household_income_coarse)]
  incAvgLong <- melt(incAvg, id.var = c("household_income_coarse", "dot"),
                     variable.name = "Component")

  ggplot(incAvgLong, aes(x = household_income_coarse, y = value, fill = Component)) +
    geom_bar(stat = "identity") +
    geom_point(aes(y = dot)) +
    theme_fivethirtyeight()
}
