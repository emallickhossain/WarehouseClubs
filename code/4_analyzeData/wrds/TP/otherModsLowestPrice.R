# Getting the lowest possible price for product categories (milk, eggs, tampon, diapers, paper towels)
# The over-arching idea is that I want to get the savings a household
# could have obtained if they had purchased at the unit price of the largest
# brand available during that shopping trip.
# I take the following steps:
# Step 1: From the Scanner data for the module, find the lowest unit price on a
# bigger package given the store-brand-week.
# Step 2: Combine this "theoretical" lowest unit price with actual shopping
# behavior recorded in the Homescan data and compute the possible savings that could
# have been realized.
# Step 3: Compute the expenditure-weighted average savings for each household
# Step 4: Compute the projection-weighted average savings for each income group
library(data.table)
library(lubridate)
library(stargazer)
library(lfe)
library(knitr)
library(ggplot2)
library(ggthemes)
yrs <- 2006:2016
threads <- 8
path <- "/scratch/upenn/hossaine/nielsen_extracts/RMS/"

# Download from Globus and then run this
# tar -xzvf *.tgz

getSales <- function(moduleCode, moduleName, fileNum) {

  # Getting number of units per package
  prod <- fread(paste0(path, "Master_Files/Latest/products.tsv"), nThread = threads,
                select = c("upc", "upc_ver_uc", "product_module_code", "brand_code_uc",
                           "multi", "size1_amount"),
                quote = "")[product_module_code == moduleCode]
  prod[, "product_module_code" := NULL]
  prod[, "units" := multi * size1_amount][, c("multi", "size1_amount") := NULL]

  # Getting annual selection of products for each store
  # I compute the average annual price by taking the sales-weighted average price
  fullSales <- NULL
  for (yr in yrs) {
    print(yr)
    sales <- fread(paste0(path, yr, "/Movement_Files/", fileNum, "_", yr, "/", moduleCode, "_", yr, ".tsv"),
                   nThread = threads, select = c("store_code_uc", "upc", "week_end", "price"))
    upcVer <- fread(paste0(path, yr, "/Annual_Files/rms_versions_", yr, ".tsv"))
    sales <- merge(sales, upcVer, by = "upc")
    sales <- merge(sales, prod, by = c("upc", "upc_ver_uc"))
    sales[, "unitPrice" := price / units]

    # Getting min unit price and recording size of min unit price
    sales[, "minUnitPrice" := min(unitPrice),
          by = .(store_code_uc, brand_code_uc, week_end, units)]
    sales <- sales[unitPrice == minUnitPrice]
    sales <- unique(sales[, .(store_code_uc, week_end, brand_code_uc, units, minUnitPrice)])
    fullSales <- rbindlist(list(fullSales, sales), use.names = TRUE)
  }

  fwrite(fullSales, paste0("/scratch/upenn/hossaine/full", moduleName, ".csv"),
         nThread = threads)
}

getSales(8444, "diaper", 4502)
getSales(4100, "eggs", 2505)
getSales(3625, "milk", 2506)
getSales(7270, "tampon", 6015)
getSales(7734, "paperTowel", 4507)

# Getting trips and adding in week_end date
trips <- fread("/scratch/upenn/hossaine/fullTrips.csv", nThread = threads,
               select = c("store_code_uc", "trip_code_uc", "purchase_date",
                          "household_code", "panel_year"))

moduleCodes <- c(8444, 4100, 3625, 7270, 7734)

fullPurch <- NULL
for (yr in yrs) {
  print(yr)
  # Getting purchase data and computing unit price paid
  purch <- fread(paste0("/scratch/upenn/hossaine/fullPurch", yr, ".csv"),
                 nThread = threads,
                 select = c("trip_code_uc", "brand_code_uc", "product_module_code",
                            "packagePrice", "quantity", "totalAmount"),
                 key = "trip_code_uc")[product_module_code %in% moduleCodes]
  purch <- merge(purch, trips, by = "trip_code_uc")
  purch[, ':=' (unitPriceChosen = packagePrice / totalAmount,
                totalExp = quantity * packagePrice)]
  purch[, "week_end" := ceiling_date(as.Date(purchase_date), "week", week_start = 6)]
  purch[, c("purchase_date") := NULL]
  purch[, "week_end" := as.integer(gsub("-", "", week_end))]
  fullPurch <- rbindlist(list(fullPurch, purch), use.names = TRUE)
}

# Computing total expenditures so I can properly extrapolate savings to all purchases
fullPurch[, "totalHouseholdBasket" := sum(totalExp),
          by = .(household_code, panel_year, product_module_code)]

# Combining with Homescan data
modKey <- data.table(modName = c("diaper", "eggs", "milk", "tampon", "paperTowel"),
                     product_module_code = c(8444, 4100, 3625, 7270, 7734))
fullChoices <- NULL
for (mod in c("diaper", "eggs", "milk", "tampon", "paperTowel")) {
  print(mod)
  fullSales <- fread(paste0("/scratch/upenn/hossaine/full", mod, ".csv"),
                     nThread = threads)
  fullSales[, "product_module_code" := modKey[modName == mod]$product_module_code]
  choices <- merge(fullSales, fullPurch,
                   by = c("store_code_uc", "week_end",
                          "brand_code_uc", "product_module_code"))
  choices[, "mod" := mod]
  fullChoices <- rbindlist(list(fullChoices, choices), use.names = TRUE)
}

# Finding the lowest price available in a bigger size
bestPrice <- fullChoices[units >= totalAmount, .(bestPrice = min(minUnitPrice)),
                         by = .(store_code_uc, week_end, trip_code_uc, quantity,
                                totalAmount, household_code, panel_year,
                                unitPriceChosen, totalHouseholdBasket, mod)]
bestPrice[, "totalSavings" := unitPriceChosen - bestPrice]
bestPrice[totalSavings < 0, "totalSavings" := 0]

hhSavings <- bestPrice[, .(matchedBasket = sum(unitPriceChosen * quantity * totalAmount),
                           totalSavings = sum(totalSavings * quantity * totalAmount)),
                           by = .(household_code, panel_year,
                                  totalHouseholdBasket, mod)]
hhSavings[, "extrapolatedSavings" := totalHouseholdBasket / matchedBasket * totalSavings]
for (m in c("diaper", "eggs", "milk", "tampon", "paperTowel")) {
  print(m)
  print(quantile(hhSavings[mod == m]$extrapolatedSavings))
}
# Combining with household characteristics
panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("panel_year", "household_code", "projection_factor",
                          "household_income_coarse", "household_size", "age",
                          "child", "dma_cd", "household_income"),
               key = c("household_code", "panel_year"))
panel[, "household_income" := as.factor(household_income)]
hhSavings <- merge(hhSavings, panel, by = c("household_code", "panel_year"))

kable(hhSavings[household_size == 4, median(extrapolatedSavings),
                keyby = .(mod, household_income_coarse)])
kable(hhSavings[household_size == 4, median(extrapolatedSavings / totalHouseholdBasket),
                keyby = .(mod, household_income_coarse)])

# Computing savings by income group for each module
reg1 <- felm(data = hhSavings[mod == "diaper"],
             weekSavings ~ household_income_coarse + age + child +
               household_size | as.factor(dma_cd) * as.factor(panel_year))
reg2 <- felm(data = hhSavings[mod == "tampon"],
             weekSavings ~ household_income_coarse + age + child +
               household_size | as.factor(dma_cd) * as.factor(panel_year))
reg3 <- felm(data = hhSavings[mod == "eggs"],
             extrapolatedSavings ~ household_income_coarse + age + child +
               household_size | as.factor(dma_cd) * as.factor(panel_year))
reg4 <- felm(data = hhSavings[mod == "milk"],
             weekSavings ~ household_income_coarse + age + child +
               household_size | as.factor(dma_cd) * as.factor(panel_year))

stargazer(reg1, reg2, reg3, reg4, type = "text",
          add.lines = list(c("Demographics", "Y", "Y", "Y", "Y"),
                           c("Market-Year FE", "Y", "Y", "Y", "Y")),
          omit = c("age", "child", "household_size"),
          order = c(2, 3, 1),
          covariate.labels = c("25-50k", "50-100k", ">100k"),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("Diapers", "Tampons", "Eggs", "Milk"),
          column.separate = c(1, 1, 1, 1),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          notes.align = "l",
          digits = 3,
          label = "tab:lowestPrice",
          title = "Rich Households Miss Out on Less Savings",
          out = "./tables/lowestPriceReg.tex")
# Manually add TP regs to this table later.
