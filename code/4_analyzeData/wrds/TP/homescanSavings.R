# Calculate savings behaviors
library(data.table)
library(lfe)
library(ggplot2)
library(ggthemes)
library(stargazer)
library(ggridges)
library(readxl)
threads <- 8
path <- "/scratch/upenn/hossaine/nielsen_extracts/HMS/"

# Classifying products as bulk/not and storable/not
storable <- fread(paste0("/scratch/upenn/hossaine/storableClassification.csv"))
retailers <- fread("/scratch/upenn/hossaine/retailers.csv")
panel <- fread("/scratch/upenn/hossaine/fullPanel.csv",
               select = c("panel_year", "fips", "household_code",
                          "projection_factor", "household_income", "household_size",
                          "age", "child", "market"))

# Getting products
prod <- na.omit(fread(paste0(path, "Master_Files/Latest/products.tsv"), quote = "",
                      nThread = threads, key = c("upc", "upc_ver_uc")))

# Keeping most common size categories for each module
prod[, "count" := .N, by = .(product_module_code, size1_units)]
prod[, "max" := max(count), by = product_module_code]
prod <- prod[count == max]
prod[, c("count", "max") := NULL]
prod[, "totalAmount" := multi * size1_amount]

# Excluding "deferred" modules per Nielsen's suggestion (while there are 164 of
# these, only 60 are actually in the products file)
deferred <- setDT(read_xlsx("/home/upenn/hossaine/Nielsen/Data/Product_Hierarchy.xlsx"))
deferred <- unique(deferred[, .(product_module_code, `Deferred (Please see documentation for explanation and notes)`)])
setnames(deferred, c("product_module_code", "deferred"))
prod <- merge(prod, deferred, by = "product_module_code")
prod <- prod[is.na(deferred)]
prod[, "deferred" := NULL]

# Excluding all alcohol purchases
prod <- prod[!product_module_code %in% c(5000:5060, 7806)]

# Classifying bulk sizes in product file
quartiles <- prod[, .(cutoff = quantile(totalAmount, c(0.25, 0.5, 0.75, 1))),
                  by = .(product_module_code)]
quartiles[, "quartile" := 1:4]
quarWide <- dcast(data = quartiles, product_module_code ~ quartile, value.var = "cutoff")
setnames(quarWide, c("product_module_code", "q1", "q2", "q3", "q4"))
prod <- merge(prod, quarWide, by = "product_module_code")
rm(quartiles, quarWide)
prod[totalAmount > q3 & totalAmount <= q4, "quartile" := 4L]
prod[totalAmount > q2 & totalAmount <= q3, "quartile" := 3L]
prod[totalAmount > q1 & totalAmount <= q2, "quartile" := 2L]
prod[totalAmount > 0 & totalAmount <= q1, "quartile" := 1L]
prod <- merge(prod, storable, by = c("product_group_code", "product_group_descr"))
prod <- prod[, .(upc, upc_ver_uc, product_module_code, brand_code_uc,
                 totalAmount, size1_units, quartile, storable)]

# Getting all purchases by year
for (i in 2004:2017) {
  print(i)
  purch <- fread(paste0(path, i, "/Annual_Files/purchases_", i, ".tsv"), nThread = threads)
  purch <- merge(purch, prod, by = c("upc", "upc_ver_uc"))
  purch[, ':=' (coupon = ifelse(coupon_value > 0, 1L, 0L),
                sale = ifelse(deal_flag_uc > 0 & coupon_value == 0, 1L, 0L),
                generic = ifelse(brand_code_uc == 536746, 1L, 0L),
                price_paid_wCoupon = total_price_paid - coupon_value)]
  purch[, "packagePrice" := price_paid_wCoupon / quantity]
  purch[, c("upc_ver_uc", "quantity", "total_price_paid", "coupon_value", "deal_flag_uc") := NULL]

  trips <- fread(paste0(path, i, "/Annual_Files/trips_", i, ".tsv"), nThread = threads,
                 select = c("trip_code_uc", "household_code", "panel_year", "retailer_code"))
  purch <- merge(purch, trips, by = "trip_code_uc")
  purch <- merge(purch, retailers, by = "retailer_code")
  purch <- merge(purch, panel, by = c("household_code", "panel_year"))

  # Only keeping modules with more than 100 recorded purchases
  purch[, "modCount" := .N, by = product_module_code]
  purch <- purch[modCount > 100]

  # Only keeping modules with more than 3 unique sizes purchased
  purch[, "uniqueSizes" := uniqueN(totalAmount), by = product_module_code]
  purch <- purch[uniqueSizes > 3]
  purch[, c("modCount", "uniqueSizes", "trip_code_uc") := NULL]

  purch[, ':=' (club = ifelse(channel_type == "Warehouse Club", 1L, 0L),
                dollar = ifelse(channel_type == "Dollar Store", 1L, 0L),
                bulk = ifelse(quartile >= 4, 1L, 0L))]
  purch[, "unitCost" := packagePrice / totalAmount]
  purch <- purch[unitCost > 0.01]
  purch[, "lUnitCost" := log(unitCost)]

  # Generating some fixed effect interactions
  purch[, "retailerModule" := paste0(retailer_code, product_module_code)]
  purch[, "retailerBrand" := paste0(retailer_code, product_module_code, brand_code_uc)]
  purch[, "retailerUPC" := paste0(retailer_code, upc)]

  # Computing savings overall
  graphData <- NULL
  graphData2 <- NULL
  graphData3 <- NULL
  print("Overall savings")
  regAll <- felm(data = purch, lUnitCost ~ coupon + sale + generic + as.factor(quartile) |
                   retailerModule + market + panel_year)
  regAll2 <- felm(data = purch, lUnitCost ~ coupon + sale + as.factor(quartile) |
                   retailerBrand + market + panel_year)
  regAll3 <- felm(data = purch, lUnitCost ~ coupon + sale |
                    retailerUPC + market + panel_year)
  stargazer(regAll, regAll2, regAll3, type = "text",
            add.lines = list(c("Fixed Effect", "Retailer-Module", "Retailer-Brand", "Retailer-UPC")),
            single.row = TRUE, no.space = TRUE, omit.stat = c("ser", "rsq"),
            out.header = FALSE,
            column.labels = c("Log(Unit Price)"), column.separate = c(3),
            dep.var.caption = "", dep.var.labels.include = FALSE,
            notes.align = "l",
            digits = 2,
            label = "tab:overallSavings",
            out = paste0("/home/upenn/hossaine/tables/overallSavings", i, ".tex"))

  # Storing coefficients for graphing
  coefs <- as.data.table(summary(regAll)$coefficients, keep.rownames = TRUE)
  coefs2 <- as.data.table(summary(regAll2)$coefficients, keep.rownames = TRUE)
  coefs3 <- as.data.table(summary(regAll3)$coefficients, keep.rownames = TRUE)
  graphData <- rbindlist(list(graphData, data.table(coefs, type = "All")))
  graphData2 <- rbindlist(list(graphData2, data.table(coefs2, type = "All")))
  graphData3 <- rbindlist(list(graphData3, data.table(coefs3, type = "All")))
  rm(regAll, regAll2, regAll3)

  # Computing savings on storable items
  print("Storable savings")
  regStorable <- felm(data = purch[storable == 1], lUnitCost ~ coupon + sale + generic +
                       as.factor(quartile) | retailerModule + market + panel_year)
  regStorable2 <- felm(data = purch[storable == 1], lUnitCost ~ coupon + sale + as.factor(quartile) |
                    retailerBrand + market + panel_year)
  regStorable3 <- felm(data = purch[storable == 1], lUnitCost ~ coupon + sale |
                         retailerUPC + market + panel_year)
  stargazer(regStorable, regStorable2, regStorable3, type = "text",
            add.lines = list(c("Fixed Effect", "Retailer-Module", "Retailer-Brand", "Retailer-UPC")),
            single.row = TRUE, no.space = TRUE, omit.stat = c("ser", "rsq"),
            out.header = FALSE,
            column.labels = c("Log(Unit Price)"), column.separate = c(3),
            dep.var.caption = "", dep.var.labels.include = FALSE,
            notes.align = "l",
            digits = 2,
            label = "tab:overallSavingsStorable",
            out = paste0("/home/upenn/hossaine/tables/overallSavingsStorable", i, ".tex"))

  # Storing coefficients for graphing
  coefs <- as.data.table(summary(regStorable)$coefficients, keep.rownames = TRUE)
  coefs2 <- as.data.table(summary(regStorable2)$coefficients, keep.rownames = TRUE)
  coefs3 <- as.data.table(summary(regStorable3)$coefficients, keep.rownames = TRUE)
  graphData <- rbindlist(list(graphData, data.table(coefs, type = "Storable")))
  graphData2 <- rbindlist(list(graphData2, data.table(coefs2, type = "Storable")))
  graphData3 <- rbindlist(list(graphData3, data.table(coefs3, type = "Storable")))
  rm(regStorable, regStorable2, regStorable3)

  # Computing savings on non-storable items
  print("Non-storable savings")
  regNonStorable <- felm(data = purch[storable == 0], lUnitCost ~ coupon + sale + generic +
                        as.factor(quartile) | retailerModule + market + panel_year)
  regNonStorable2 <- felm(data = purch[storable == 0], lUnitCost ~ coupon + sale + as.factor(quartile) |
                         retailerBrand + market + panel_year)
  regNonStorable3 <- felm(data = purch[storable == 0], lUnitCost ~ coupon + sale |
                            retailerUPC + market + panel_year)
  stargazer(regNonStorable, regNonStorable2, regNonStorable3, type = "text",
            add.lines = list(c("Fixed Effect", "Retailer-Module", "Retailer-Brand", "Retailer-UPC")),
            single.row = TRUE, no.space = TRUE, omit.stat = c("ser", "rsq"),
            out.header = FALSE,
            column.labels = c("Log(Unit Price)"), column.separate = c(3),
            dep.var.caption = "", dep.var.labels.include = FALSE,
            notes.align = "l",
            digits = 2,
            label = "tab:overallSavingsNonStorable",
            out = paste0("/home/upenn/hossaine/tables/overallSavingsNonStorable", i, ".tex"))

  coefs <- as.data.table(summary(regNonStorable)$coefficients, keep.rownames = TRUE)
  coefs2 <- as.data.table(summary(regNonStorable2)$coefficients, keep.rownames = TRUE)
  coefs3 <- as.data.table(summary(regNonStorable3)$coefficients, keep.rownames = TRUE)

  graphData <- rbindlist(list(graphData, data.table(coefs, type = "Non-Storable")))
  graphData2 <- rbindlist(list(graphData2, data.table(coefs2, type = "Non-Storable")))
  graphData3 <- rbindlist(list(graphData3, data.table(coefs3, type = "Non-Storable")))
  rm(regNonStorable, regNonStorable2, regNonStorable3)

  # Housekeeping graphing data
  graphData[, "rn" := gsub("as\\.factor\\(quartile\\)", "Quartile ", rn)]
  graphData[rn == "coupon", "rn" := "Coupon"]
  graphData[rn == "sale", "rn" := "Sale"]
  graphData[rn == "generic", "rn" := "Generic"]
  setnames(graphData, c("rn", "beta", "se", "t", "p", "type"))
  graphData[, "year" := i]

  graphData2[, "rn" := gsub("as\\.factor\\(quartile\\)", "Quartile ", rn)]
  graphData2[rn == "coupon", "rn" := "Coupon"]
  graphData2[rn == "sale", "rn" := "Sale"]
  graphData2[rn == "generic", "rn" := "Generic"]
  setnames(graphData2, c("rn", "beta", "se", "t", "p", "type"))
  graphData2[, "year" := i]

  graphData3[, "rn" := gsub("as\\.factor\\(quartile\\)", "Quartile ", rn)]
  graphData3[rn == "coupon", "rn" := "Coupon"]
  graphData3[rn == "sale", "rn" := "Sale"]
  graphData3[rn == "generic", "rn" := "Generic"]
  setnames(graphData3, c("rn", "beta", "se", "t", "p", "type"))
  graphData3[, "year" := i]

  fwrite(graphData, paste0("/home/upenn/hossaine/graphData-", i, ".csv"))
  fwrite(graphData2, paste0("/home/upenn/hossaine/graphData2-", i, ".csv"))
  fwrite(graphData3, paste0("/home/upenn/hossaine/graphData3-", i, ".csv"))

  # Savings by product module
  print("Savings by Module")
  savingsBeta <- NULL
  mods <- sort(unique(purch$product_module_code))
  excludeMods <- c(8410)
  mods <- mods[!mods %in% excludeMods]
  for (j in mods) {
    print(j)
    storable <- unique(purch[product_module_code == j]$storable)
    regAll <- felm(data = purch[product_module_code == j],
                   lUnitCost ~ coupon + sale + as.factor(quartile) |
                     retailerBrand + market)
    coefs <- as.data.table(summary(regAll)$coefficients, keep.rownames = TRUE)
    savingsBeta <- rbindlist(list(savingsBeta,
                                  data.table(coefs, product_module_code = j, storable = storable)))
  }
  savingsBeta[, "storable" := factor(storable, levels = c(0, 1),
                                     labels = c("Non-Storable", "Storable"))]
  savingsBeta[, "rn" := gsub("as\\.factor\\(quartile\\)", "Quartile ", rn)]
  savingsBeta[rn == "coupon", "rn" := "Coupon"]
  savingsBeta[rn == "sale", "rn" := "Sale"]
  savingsBeta[rn == "generic", "rn" := "Generic"]
  setnames(savingsBeta, c("rn", "beta", "se", "t", "p", "module", "Storable"))
  savingsBeta[, "year" := i]

  fwrite(savingsBeta, paste0("/home/upenn/hossaine/savingsBeta", i, ".csv"))

  # Getting overall savings behavior
  print("Savings Behavior")
  savingsAll <- purch[, .(coupon = weighted.mean(coupon, w = price_paid_wCoupon),
                              sale = weighted.mean(sale, w = price_paid_wCoupon),
                              generic = weighted.mean(generic, w = price_paid_wCoupon),
                              bulk = weighted.mean(bulk, w = price_paid_wCoupon),
                              club = weighted.mean(club, w = price_paid_wCoupon),
                              dollar = weighted.mean(dollar, w = price_paid_wCoupon)),
                          by = .(household_code, panel_year, projection_factor,
                                 household_income, household_size, age, child)]
  savingsAll[, "household_income" := factor(household_income)]

  savingsStorable <- purch[storable == 1,
                               .(coupon = weighted.mean(coupon, w = price_paid_wCoupon),
                                 sale = weighted.mean(sale, w = price_paid_wCoupon),
                                 generic = weighted.mean(generic, w = price_paid_wCoupon),
                                 bulk = weighted.mean(bulk, w = price_paid_wCoupon),
                                 club = weighted.mean(club, w = price_paid_wCoupon),
                                 dollar = weighted.mean(dollar, w = price_paid_wCoupon)),
                               by = .(household_code, panel_year, projection_factor,
                                      household_income, household_size, age, child)]
  savingsStorable[, "household_income" := factor(household_income)]

  savingsNonStorable <- purch[storable == 0,
                                  .(coupon = weighted.mean(coupon, w = price_paid_wCoupon),
                                    sale = weighted.mean(sale, w = price_paid_wCoupon),
                                    generic = weighted.mean(generic, w = price_paid_wCoupon),
                                    bulk = weighted.mean(bulk, w = price_paid_wCoupon),
                                    club = weighted.mean(club, w = price_paid_wCoupon),
                                    dollar = weighted.mean(dollar, w = price_paid_wCoupon)),
                                  by = .(household_code, panel_year, projection_factor,
                                         household_income, household_size, age, child)]
  savingsNonStorable[, "household_income" := factor(household_income)]

  # Savings across the board
  # Could include correlations with other savings devices by adding in regressors,
  # but based on 2017 data, about 87\% of purchases have no deals or 1, so there
  # isn't much combo-ing of deals. We'll just stick with a single one.
  graphData <- NULL
  regAll <- felm(data = savingsAll, bulk ~ household_income +
                   age + household_size + child + 0, weights = savingsAll$projection_factor)
  coefs <- as.data.table(summary(regAll)$coefficients, keep.rownames = TRUE)
  graphData <- rbindlist(list(graphData, data.table(coefs, type = "Bulk", product = "All")))

  regAll <- felm(data = savingsAll, coupon ~ household_income +
                   age + household_size + child + 0, weights = savingsAll$projection_factor)
  coefs <- as.data.table(summary(regAll)$coefficients, keep.rownames = TRUE)
  graphData <- rbindlist(list(graphData, data.table(coefs, type = "Coupon", product = "All")))

  regAll <- felm(data = savingsAll, sale ~ household_income +
                   age + household_size + child + 0, weights = savingsAll$projection_factor)
  coefs <- as.data.table(summary(regAll)$coefficients, keep.rownames = TRUE)
  graphData <- rbindlist(list(graphData, data.table(coefs, type = "Sale", product = "All")))

  regAll <- felm(data = savingsAll, generic ~ household_income +
                   age + household_size + child + 0, weights = savingsAll$projection_factor)
  coefs <- as.data.table(summary(regAll)$coefficients, keep.rownames = TRUE)
  graphData <- rbindlist(list(graphData, data.table(coefs, type = "Generic", product = "All")))

  # Savings on storables
  regStorable <- felm(data = savingsStorable, bulk ~ household_income +
                        age + household_size + child + 0,
                      weights = savingsStorable$projection_factor)
  coefs <- as.data.table(summary(regStorable)$coefficients, keep.rownames = TRUE)
  graphData <- rbindlist(list(graphData, data.table(coefs, type = "Bulk", product = "Storable")))

  regStorable <- felm(data = savingsStorable, coupon ~ household_income +
                        age + household_size + child + 0,
                      weights = savingsStorable$projection_factor)
  coefs <- as.data.table(summary(regStorable)$coefficients, keep.rownames = TRUE)
  graphData <- rbindlist(list(graphData, data.table(coefs, type = "Coupon", product = "Storable")))

  regStorable <- felm(data = savingsStorable, sale ~ household_income +
                        age + household_size + child + 0,
                      weights = savingsStorable$projection_factor)
  coefs <- as.data.table(summary(regStorable)$coefficients, keep.rownames = TRUE)
  graphData <- rbindlist(list(graphData, data.table(coefs, type = "Sale", product = "Storable")))

  regStorable <- felm(data = savingsStorable, generic ~ household_income +
                        age + household_size + child + 0,
                      weights = savingsStorable$projection_factor)
  coefs <- as.data.table(summary(regStorable)$coefficients, keep.rownames = TRUE)
  graphData <- rbindlist(list(graphData, data.table(coefs, type = "Generic", product = "Storable")))

  # Savings on non-storables
  regNonStorable <- felm(data = savingsNonStorable, bulk ~ household_income +
                           age + household_size + child + 0,
                         weights = savingsNonStorable$projection_factor)
  coefs <- as.data.table(summary(regNonStorable)$coefficients, keep.rownames = TRUE)
  graphData <- rbindlist(list(graphData, data.table(coefs, type = "Bulk", product = "Non-Storable")))

  regNonStorable <- felm(data = savingsNonStorable, coupon ~ household_income +
                           age + household_size + child + 0,
                         weights = savingsNonStorable$projection_factor)
  coefs <- as.data.table(summary(regNonStorable)$coefficients, keep.rownames = TRUE)
  graphData <- rbindlist(list(graphData, data.table(coefs, type = "Coupon", product = "Non-Storable")))

  regNonStorable <- felm(data = savingsNonStorable, sale ~ household_income +
                           age + household_size + child + 0,
                         weights = savingsNonStorable$projection_factor)
  coefs <- as.data.table(summary(regNonStorable)$coefficients, keep.rownames = TRUE)
  graphData <- rbindlist(list(graphData, data.table(coefs, type = "Sale", product = "Non-Storable")))

  regNonStorable <- felm(data = savingsNonStorable, generic ~ household_income +
                           age + household_size + child + 0,
                         weights = savingsNonStorable$projection_factor)
  coefs <- as.data.table(summary(regNonStorable)$coefficients, keep.rownames = TRUE)
  graphData <- rbindlist(list(graphData, data.table(coefs, type = "Generic", product = "Non-Storable")))

  # Organizing graph data
  graphData <- graphData[!rn %in% c("age", "household_size", "child")]
  graphData[, "rn" := gsub("household_income", "", rn)]
  graphData[, "rn" := factor(rn, levels = c(8, 10, 11, 13, 15, 16, 17, 18, 19, 21, 23, 26, 27),
                             labels = c(11, 13.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 55, 65, 85, 100),
                             ordered = TRUE)]
  graphData[, "rn" := as.numeric(as.character(rn))]
  setnames(graphData, c("rn", "beta", "se", "t", "p", "type", "product"))
  graphData[, "year" := i]

  fwrite(graphData, paste0("/home/upenn/hossaine/graphData4-", i, ".csv"))

  # Violin plots for each beta by product module
  # Getting overall savings behavior
  print("Savings Behavior by Module")
  savingsAll <- purch[, .(coupon = weighted.mean(coupon, w = price_paid_wCoupon),
                              sale = weighted.mean(sale, w = price_paid_wCoupon),
                              generic = weighted.mean(generic, w = price_paid_wCoupon),
                              bulk = weighted.mean(bulk, w = price_paid_wCoupon),
                              club = weighted.mean(club, w = price_paid_wCoupon),
                              dollar = weighted.mean(dollar, w = price_paid_wCoupon)),
                          by = .(household_code, panel_year, projection_factor,
                                 household_income, household_size, age, child,
                                 product_module_code, storable)]
  savingsAll[, "household_income" := factor(household_income)]

  # Savings across the board
  graphData <- NULL
  topMods <- purch[, .N, by = product_module_code]
  setorder(topMods, -N)
  mods <- c(topMods$product_module_code[1:25], 8444, 7270)
  for (j in mods) {
    print(j)
    storable <- unique(purch[product_module_code == j]$storable)
    regAll <- felm(data = savingsAll[product_module_code == j],
                   bulk ~ household_income + age + household_size + child + 0,
                   weights = savingsAll[product_module_code == j]$projection_factor)
    coefs <- as.data.table(summary(regAll)$coefficients, keep.rownames = TRUE)
    graphData <- rbindlist(list(graphData, data.table(coefs, type = "Bulk",
                                                      module = j, storable = storable)))

    regAll <- felm(data = savingsAll[product_module_code == j],
                   coupon ~ household_income + age + household_size + child + 0,
                   weights = savingsAll[product_module_code == j]$projection_factor)
    coefs <- as.data.table(summary(regAll)$coefficients, keep.rownames = TRUE)
    graphData <- rbindlist(list(graphData, data.table(coefs, type = "Coupon",
                                                      module = j, storable = storable)))

    regAll <- felm(data = savingsAll[product_module_code == j],
                   sale ~ household_income + age + household_size + child + 0,
                   weights = savingsAll[product_module_code == j]$projection_factor)
    coefs <- as.data.table(summary(regAll)$coefficients, keep.rownames = TRUE)
    graphData <- rbindlist(list(graphData, data.table(coefs, type = "Sale",
                                                      module = j, storable = storable)))

    regAll <- felm(data = savingsAll[product_module_code == j],
                   generic ~ household_income + age + household_size + child + 0,
                   weights = savingsAll[product_module_code == j]$projection_factor)
    coefs <- as.data.table(summary(regAll)$coefficients, keep.rownames = TRUE)
    graphData <- rbindlist(list(graphData, data.table(coefs, type = "Generic",
                                                      module = j, storable = storable)))
  }
  # Organizing graph data
  graphData <- graphData[!rn %in% c("age", "household_size", "child")]
  graphData[, "rn" := gsub("household_income", "", rn)]
  graphData[, "rn" := factor(rn, levels = c(8, 10, 11, 13, 15, 16, 17, 18, 19, 21, 23, 26, 27),
                             labels = c(11, 13.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 55, 65, 85, 100),
                             ordered = TRUE)]
  graphData[, "rn" := as.numeric(as.character(rn))]
  setnames(graphData, c("rn", "beta", "se", "t", "p", "type", "module", "storable"))
  graphData[, "year" := i]

  fwrite(graphData, paste0("/home/upenn/hossaine/graphData5-", i, ".csv"))
}


# Making graphs
# scp hossaine@wrds-cloud.wharton.upenn.edu:/home/upenn/hossaine/*.csv /home/mallick/Downloads
fullGraphData <- NULL
fullGraphData2 <- NULL
fullGraphData3 <- NULL
for (i in 2004:2017) {
  # Plotting savings
  graphData <- fread(paste0("/home/mallick/Downloads/graphData-", i, ".csv"))
  fullGraphData <- rbindlist(list(fullGraphData, graphData), use.names = TRUE)

  graphData2 <- fread(paste0("/home/mallick/Downloads/graphData2-", i, ".csv"))
  fullGraphData2 <- rbindlist(list(fullGraphData2, graphData2), use.names = TRUE)

  graphData3 <- fread(paste0("/home/mallick/Downloads/graphData3-", i, ".csv"))
  fullGraphData3 <- rbindlist(list(fullGraphData3, graphData3), use.names = TRUE)
}
fullGraphData <- fullGraphData[, .(beta = mean(beta), se = mean(se)), by = .(rn, type)]
fullGraphData2 <- fullGraphData2[, .(beta = mean(beta), se = mean(se)), by = .(rn, type)]
fullGraphData3 <- fullGraphData3[, .(beta = mean(beta), se = mean(se)), by = .(rn, type)]

discounts <- c("Sale", "Coupon", "Generic", "Quartile 2", "Quartile 3", "Quartile 4")
ggplot(data = fullGraphData, aes(x = as.factor(rn), y = beta, fill = type)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = beta - 1.96 * se, ymax = beta + 1.96 * se),
                width = 0.2, position = position_dodge(0.9)) +
  scale_x_discrete(limits = discounts) +
  scale_y_continuous(limits = c(-1, 0)) +
  labs(title = "Bulk Discounts Provide Largest Savings",
       subtitle = "Storable Items Offer Larger Quantity Discounts than Non-Storable Items",
       x = NULL,
       y = "Unit Price Savings",
       caption = paste0("Note: Black bars denote standard errors. Bars denote estimate ",
                        "of unit price discount \nafter controlling for year-month, market, ",
                        "and retailer-module fixed effects. ")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0)) +
  scale_fill_grey()

ggsave(filename = "./code/5_figures/savingsStoreCategory.png")

discounts <- c("Sale", "Coupon", "Quartile 2", "Quartile 3", "Quartile 4")
ggplot(data = fullGraphData2, aes(x = as.factor(rn), y = beta, fill = type)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = beta - 1.96 * se, ymax = beta + 1.96 * se),
                width = 0.2, position = position_dodge(0.9)) +
  scale_x_discrete(limits = discounts) +
  scale_y_continuous(limits = c(-1, 0)) +
  labs(title = "Bulk Discounts And Coupons Provide Largest Savings",
       subtitle = "Storable Items Offer Larger Quantity Discounts than Non-Storable Items",
       x = NULL,
       y = "Unit Price Savings",
       caption = paste0("Note: Black bars denote standard errors. Bars denote estimate ",
                        "of unit price discount \nafter controlling for year-month, market, ",
                        "and retailer-brand fixed effects. ")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0)) +
  scale_fill_grey()

ggsave(filename = "./code/5_figures/savingsStoreBrand.png")

discounts <- c("Sale", "Coupon")
ggplot(data = fullGraphData3, aes(x = as.factor(rn), y = beta, fill = type)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = beta - 1.96 * se, ymax = beta + 1.96 * se),
                width = 0.2, position = position_dodge(0.9)) +
  scale_x_discrete(limits = discounts) +
  scale_y_continuous(limits = c(-1, 0)) +
  labs(title = "Bulk Discounts And Coupons Provide Largest Savings",
       subtitle = "Storable Items Offer Larger Quantity Discounts than Non-Storable Items",
       x = NULL,
       y = "Unit Price Savings",
       caption = paste0("Note: Black bars denote standard errors. Bars denote estimate ",
                        "of unit price discount \nafter controlling for year-month, market, ",
                        "and retailer-UPC fixed effects. ")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0)) +
  scale_fill_grey()

ggsave(filename = "/home/upenn/hossaine/figures/savingsStoreUPC.png")

fullSavingsBeta <- NULL
for (i in 2004:2017) {
  savingsBeta <- fread(paste0("/home/mallick/Downloads/savingsBeta", i, ".csv"))
  fullSavingsBeta <- rbindlist(list(fullSavingsBeta, savingsBeta), use.names = TRUE)
}

fullSavingsBeta <- fullSavingsBeta[, .(beta = mean(beta)), by = .(rn, module, Storable)]
ggplot(data = fullSavingsBeta[beta > -1 & beta < 1],
       aes(x = beta, y = Storable, fill = Storable)) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2) +
  facet_wrap(vars(rn)) +
  labs(title = "Storable Items Have Deeper Quantity Discounts",
       x = "Unit Price Discount",
       y = "Density",
       caption = paste0("Source: Author calulations from Nielsen Consumer Panel. \n",
                        "Note: Discount denotes percent reduction in ",
                        "unit price if product was purchased using specified discount \n",
                        "controlling for brand-retailer and market effects.")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0)) +
  scale_fill_grey()

ggsave(filename = "./code/5_figures/discountSavingsbyModule.png")

fullGraphData <- NULL
for (i in 2004:2017) {
  graphData <- fread(paste0("/home/mallick/Downloads/graphData4-", i, ".csv"))
  fullGraphData <- rbindlist(list(fullGraphData, graphData), use.names = TRUE)
}

fullGraphData <- fullGraphData[, .(beta = mean(beta), se = mean(se)), by = .(product, rn, type)]
setnames(fullGraphData, "product", "Product")
ggplot(data = fullGraphData, aes(x = rn, y = beta, color = Product)) +
  geom_errorbar(aes(ymin = beta - 1.96 * se, ymax = beta + 1.96 * se), width = 0.2) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  facet_wrap(vars(type), scales = "fixed") +
  labs(title = "Rich Households Bulk Buy More",
       x = "Household Income", y = "Indicator",
       caption = paste0("Source: Author calulations from Nielsen Consumer Panel. \n",
                        "Note: Demographic adjustments control for household size, \n",
                        "age, and presence of children.")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0)) +
  scale_color_grey()
ggsave("./code/5_figures/savingsBehavior.png", height = 6, width = 6)

fullGraphData <- NULL
for (i in 2004:2017) {
  graphData <- fread(paste0("/home/mallick/Downloads/graphData5-", i, ".csv"))
  fullGraphData <- rbindlist(list(fullGraphData, graphData), use.names = TRUE)
}

fullGraphData <- fullGraphData[, .(beta = mean(beta), se = mean(se)), by = .(module, rn, type)]
for (mod in sort(unique(fullGraphData$module))) {
  ggplot(fullGraphData[module == mod], aes(x = rn, y = beta)) +
    geom_errorbar(aes(ymin = beta - 1.96 * se, ymax = beta + 1.96 * se), width = 0.2) +
    geom_point() +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    facet_wrap(vars(type), scales = "free") +
    labs(title = "Savings Behavior by Product and Discount Type",
         x = "Household Income", y = "Indicator",
         caption = paste0("Source: Author calulations. \n ",
                          "Note: Demographic adjustments control for household size, \n",
                          "age, and presence of children. Midpoints of household \n",
                          "income bins are plotted above.")) +
    theme_fivethirtyeight() +
    theme(axis.title = element_text())
  ggsave(filename = paste0("./code/5_figures/savingsBehavior-", mod, ".png"))
}
