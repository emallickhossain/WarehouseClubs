# Calculate price decomposition
library(data.table)
library(ggplot2)
library(ggthemes)
library(stargazer)
library(purrr)
threads <- 8
yrs <- 2004:2017
critValue <- 1.96

# Getting CPI to deflate values to Jan-2010 base
cpi <- fread("/scratch/upenn/hossaine/cpi.csv")
cpi[, "base" := cpi[date == "2010-01-01"]$value]
cpi[, "newIndex" := value / base * 100]
cpi[, c("base", "value") := NULL]
cpi[, "yearMonth" := .(substr(date, 1, 7))]
cpi[, "date" := NULL]

# Loading Nielsen data (no years)
retailers <- fread("/scratch/upenn/hossaine/fullRetailers.csv", nThread = threads)
trips <- fread("/scratch/upenn/hossaine/fullTrips.csv", nThread = threads,
               select = c("retailer_code", "household_code", "panel_year",
                          "trip_code_uc", "purchase_date"),
               key = "trip_code_uc")
trips[, "yearMonth" := substr(purchase_date, 1, 7)][, "purchase_date" := NULL]
trips <- merge(trips, retailers, by = "retailer_code")[, "retailer_code" := NULL]

# Collapsing channels
trips[!channel_type %in% c("Dollar Store", "Discount Store",
                           "Grocery", "Warehouse Club"), "channel_type" := "Other"]
rm(retailers)

panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("household_code", "panel_year", "projection_factor",
                          "household_income", "household_income_coarse",
                          "household_size", "age", "child", "white"))

for (yr in yrs) {
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
                     totalSpendReal = packagePriceReal * quantity)]
  mergedData[, c("couponValueReal", "packagePriceReal", "totalAmount", "deal_flag_uc") := NULL]

  # Generating factors
  # Reference group is the 2nd quantile generic brand sold at the
  # most popular discount retailer
  mergedData[, "brand_code_uc" := as.character(brand_code_uc)]
  mergedData[brand_code_uc == "536746", "brand_code_uc" := paste0(channel_type, brand_code_uc)]
  mergedData[, c("brand_code_uc", "channel_type", "quintile") :=
               .(relevel(as.factor(brand_code_uc), ref = "Discount Store536746"),
                 relevel(as.factor(channel_type), ref = "Discount Store"),
                 relevel(as.factor(quintile), ref = "2"))]

  # Looping over all product modules which have a generic brand
  fullData <- NULL
  mods <- sort(unique(mergedData[brand_code_uc == "Discount Store536746" &
                                   channel_type == "Discount Store"]$product_module_code))
  mods <- mods[mods != 8410]
  for (modCode in mods) {
    print(modCode)

    # Subsetting to specified module
    prodMod <- mergedData[product_module_code == modCode]

    # Running hedonic regression and organizing coefficients
    reg1 <- lm(unitPriceReal ~ brand_code_uc + channel_type + quintile, data = prodMod)
    coefs <- as.data.table(summary(reg1)$coefficients, keep.rownames = TRUE)
    coefs[abs(`t value`) < critValue, "Estimate" := 0]

    brands <- coefs[grepl("brand_code_uc", rn), .(rn, Estimate)]
    brands[, c("brand_code_uc", "rn") := .(gsub("brand_code_uc", "", rn), NULL)]
    setnames(brands, "Estimate", "brandComp")

    retailers <- coefs[grepl("channel_type", rn), .(rn, Estimate)]
    retailers[, c("channel_type", "rn") := .(gsub("channel_type", "", rn), NULL)]
    setnames(retailers, "Estimate", "retailComp")

    quintiles <- coefs[grepl("quintile", rn), .(rn, Estimate)]
    quintiles[, c("quintile", "rn") := .(gsub("quintile", "", rn), NULL)]
    setnames(quintiles, "Estimate", "quintComp")

    # Merging in coefficients with purchases
    prodMod[, "Intercept" := coefs[rn == "(Intercept)"]$Estimate]
    prodMod <- merge(prodMod, brands, by = "brand_code_uc", all.x = TRUE)
    prodMod <- merge(prodMod, retailers, by = "channel_type", all.x = TRUE)
    prodMod <- merge(prodMod, quintiles, by = "quintile", all.x = TRUE)

    # Generating 0s
    prodMod[is.na(brandComp), "brandComp" := 0]
    prodMod[is.na(retailComp), "retailComp" := 0]
    prodMod[is.na(quintComp), "quintComp" := 0]

    # Generating channel specific factors
    prodMod[, "DollarComp" := ifelse(channel_type == "Dollar Store", retailComp, 0)]
    prodMod[, "WarehouseComp" := ifelse(channel_type == "Warehouse Club", retailComp, 0)]
    prodMod[, "GroceryComp" := ifelse(channel_type == "Grocery", retailComp, 0)]
    prodMod[, "OtherComp" := ifelse(channel_type == "Other", retailComp, 0)]
    prodMod[, "PrivateLabel" := ifelse(grepl("*536746", brand_code_uc), brandComp, 0)]
    prodMod[PrivateLabel > 0, "brandComp" := 0]
    prodMod[, "residual" := (unitPriceReal - Intercept - unitCouponReal -
                               brandComp - retailComp - quintComp - PrivateLabel)]
    prodMod[, "unitPriceEst" := brandComp + retailComp + quintComp + PrivateLabel +
              residual - unitCouponReal]

    # Averaging components by household
    finalData <- prodMod[, .(brandComp = weighted.mean(brandComp / Intercept, w = totalSpendReal),
                             DollarComp = weighted.mean(DollarComp / Intercept, w = totalSpendReal),
                             WarehouseComp = weighted.mean(WarehouseComp / Intercept, w = totalSpendReal),
                             GroceryComp = weighted.mean(GroceryComp / Intercept, w = totalSpendReal),
                             OtherComp = weighted.mean(OtherComp / Intercept, w = totalSpendReal),
                             retailComp = weighted.mean(retailComp / Intercept, w = totalSpendReal),
                             quintComp = weighted.mean(quintComp / Intercept, w = totalSpendReal),
                             PrivateLabel = weighted.mean(PrivateLabel / Intercept, w = totalSpendReal),
                             couponComp = weighted.mean(-unitCouponReal / Intercept, w = totalSpendReal),
                             residual = weighted.mean(residual / Intercept, w = totalSpendReal),
                             unitPriceEst = weighted.mean(unitPriceEst / Intercept, w = totalSpendReal),
                             modSpending = sum(totalSpendReal)),
                         by = .(household_code, panel_year, household_income,
                                household_income_coarse, projection_factor,
                                product_module_code)]
    fullData <- rbindlist(list(fullData, finalData), use.names = TRUE)
  }

  fwrite(fullData, paste0("/scratch/upenn/hossaine/priceDecompChannel", yr, ".csv"),
         nThread = threads)
}

# Computing shares
getData <- function(yr) {
  data <- fread(paste0("/scratch/upenn/hossaine/priceDecompChannel", yr, ".csv"),
                nThread = threads,
                select = c("household_code", "panel_year", "household_income",
                           "household_income_coarse", "projection_factor",
                           "product_module_code", "brandComp", "retailComp",
                           "quintComp", "PrivateLabel", "couponComp", "residual",
                           "unitPriceEst", "modSpending"))
  return(data)
}
fullData <- rbindlist(map(yrs, getData), use.names = TRUE)
panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("household_code", "panel_year", "household_size",
                          "age", "child", "white"))
fullData <- merge(fullData, panel, by = c("household_code", "panel_year"))
prod <- fread("/scratch/upenn/hossaine/fullProd.csv", nThread = threads)
prod <- unique(prod[, .(product_module_code, storable)])
fullData <- merge(fullData, prod, by = "product_module_code")

# Averaging over all products
hhAvg <- fullData[!is.na(retailComp) & !is.infinite(retailComp),
                  .(brandComp = weighted.mean(brandComp, w = modSpending),
                    retailComp = weighted.mean(retailComp, w = modSpending),
                    quintComp = weighted.mean(quintComp, w = modSpending),
                    PrivateLabel = weighted.mean(PrivateLabel, w = modSpending),
                    couponComp = weighted.mean(couponComp, w = modSpending),
                    residual = weighted.mean(residual, w = modSpending),
                    residual2 = weighted.mean(residual + couponComp, w = modSpending),
                    unitPriceEst = weighted.mean(unitPriceEst, w = modSpending),
                    storeAndPrivate = weighted.mean(retailComp + PrivateLabel, w = modSpending)),
                  by = .(household_code, panel_year, projection_factor,
                         household_income, household_income_coarse, storable,
                         household_size, age, child, white)]

# Demographic Adjustment
finalComponentsFull <- NULL
for (i in 0:1) {
  brandReg <- lm(brandComp ~ household_income_coarse + household_size + age + child + white,
                         data = hhAvg[storable == i], weights = projection_factor)
  brandComp <- as.data.table(summary(brandReg)$coefficients, keep.rownames = TRUE)
  brandComp[, "comp" := "Brand"]

  retailReg <- lm(storeAndPrivate ~ household_income_coarse + household_size + age + child + white,
                  data = hhAvg[storable == i], weights = projection_factor)
  retailComp <- as.data.table(summary(retailReg)$coefficients, keep.rownames = TRUE)
  retailComp[, "comp" := "Retailer"]

  sizeReg <- lm(quintComp ~ household_income_coarse + household_size + age + child + white,
                data = hhAvg[storable == i], weights = projection_factor)
  sizeComp <- as.data.table(summary(sizeReg)$coefficients, keep.rownames = TRUE)
  sizeComp[, "comp" := "Size"]

  finalComponents <- rbindlist(list(brandComp, retailComp, sizeComp), use.names = TRUE)
  finalComponents[, "storable" := i]
  setnames(finalComponents, c("rn", "beta", "se", "t", "p", "comp", "storable"))
  finalComponents[, c("t", "p") := NULL]

  finalComponentsFull <- rbindlist(list(finalComponentsFull, finalComponents), use.names = TRUE)
}

# Reference Table
finalComponentsFull <- finalComponentsFull[!rn %in% c("household_size", "age", "child", "white")]
finalComponentsFull[rn == "(Intercept)", "rn" := "household_income_coarse<25k"]
finalComponentsFull[, "rn" := gsub("household_income_coarse", "", rn)]
refTable <- dcast(finalComponentsFull, rn + storable ~ comp, value.var = c("beta", "se"))
refTable[, "beta_unitCost" := 1 + beta_Brand + beta_Retailer + beta_Size]
refTable[, "se_unitCost" := se_Brand + se_Retailer + se_Size]

refTable[rn != "<25k",
         "beta_unitCost" := refTable[rn == "<25k"]$beta_unitCost - 1 + beta_unitCost]
refTable[, "storable" := ifelse(storable == 0, "Non-Storable", "Storable")]
refTable[, "income" := factor(rn,
                              levels = c("<25k", "25-50k", "50-100k", ">100k"),
                              ordered = TRUE)]
ggplot(refTable, aes(x = income, y = beta_unitCost, fill = storable)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = beta_unitCost - 1.96 * se_unitCost,
                    ymax = beta_unitCost + 1.96 * se_unitCost),
                width = 0.2, position = position_dodge(0.9)) +
  theme_fivethirtyeight() +
  labs(title = "Unit Prices Paid by Income Group",
       x = "Household Income",
       y = "Unit Price (2nd Smallest Quintile, All Generic Basket = 1)",
       caption = paste0("Note: The unit price of a basket composed of the 2nd-smallest \n",
                        "quintile package of a private-label good offered at a discount \n",
                        "retailer is normalized to $1. Components are adjusted for \n",
                        "household demographics of size, age, presence of children, and race.")) +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0)) +
  scale_fill_grey()

ggsave(filename = "./figures/unitCostGap.png")
# ggsave(filename = "./figures/unitCostGapTP.png") #7260. Rerun hhAvg with product module specified (to save computation time)
# ggsave(filename = "./figures/unitCostGapMilk.png") #3625


# Plotting
refTable[, "beta_total" := beta_Brand + beta_Retailer + beta_Size]
refTable[, "beta_bias" := beta_Size / (beta_Brand + beta_Retailer)]
finalComponentsLong <- melt(refTable[income != "<25k"],
                            id.vars = c("income", "storable", "beta_total", "beta_bias"),
                            measure.vars = c("beta_Brand", "beta_Retailer", "beta_Size"),
                            variable.name = "Component")
finalComponentsLong[, "Component" := gsub("beta_", "", Component)]
finalComponentsLong[, "Component" := factor(Component,
                                            levels = c("Size", "Retailer", "Brand"),
                                            ordered = TRUE)]
finalComponentsLong[, "income" := factor(income,
                                         levels = c("25-50k", "50-100k", ">100k"),
                                         ordered = TRUE)]

# Estimating bias
biasTable <- dcast(refTable[rn != "<25k"], rn ~ storable, value.var = "beta_bias")
stargazer(biasTable, type = "text", summary = FALSE)

# Plot 1: No Size Component
ggplot(finalComponentsLong[Component != "Size"],
       aes(x = income, y = value, fill = Component)) +
  geom_bar(stat = "identity") +
  #scale_y_continuous(limits = c(-0.05, 0.20)) +
  theme_fivethirtyeight() +
  scale_color_fivethirtyeight() +
  facet_wrap(vars(storable)) +
  labs(title = "Contributors to Unit Price Inequality",
       x = "Household Income",
       y = "Gap Explained (Percentage Points)") +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0)) +
  # scale_fill_manual("legend", values = c("Brand" = "#FF2700",
  #                                        "Retailer" = "#008FD5",
  #                                        "Size" = "#77AB43")) +
  scale_fill_grey()

ggsave(filename = "./figures/priceDecomp1.png")
#ggsave(filename = "./figures/priceDecompTP1.png") # Change scale and title
#ggsave(filename = "./figures/priceDecompMilk1.png") # Change scale and title

# Plot 2: All Components
ggplot(finalComponentsLong,
       aes(x = income, y = value, fill = Component)) +
  geom_bar(stat = "identity") +
  #scale_y_continuous(limits = c(-0.05, 0.20)) +
  theme_fivethirtyeight() +
  scale_color_fivethirtyeight() +
  facet_wrap(vars(storable)) +
  labs(title = "Contributors to Unit Price Inequality",
       x = "Household Income",
       y = "Gap Explained (Percentage Points)") +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0)) +
  # scale_fill_manual("legend", values = c("Brand" = "#FF2700",
  #                                        "Retailer" = "#008FD5",
  #                                        "Size" = "#77AB43")) +
  scale_fill_grey()

ggsave(filename = "./figures/priceDecomp2.png")
#ggsave(filename = "./figures/priceDecompTP2.png") # Change scale and title
#ggsave(filename = "./figures/priceDecompMilk2.png") # Change scale and title
