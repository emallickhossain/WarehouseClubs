# This estimates the amount low-income households could save if they closed
# the gap with high-income households. This takes the bulk discount from the
# Scanner estimation and multiplies it by the difference in average package
# size purchased. Then, the expenditure weighted average is taken
library(data.table)
library(lfe)
library(stargazer)
library(purrr)
threads <- 8

# Getting bulk discount for each product module
getCoefs <- function(i) {
  data <- fread(paste0("/home/upenn/hossaine/Nielsen/Data/",
                       "scannerBulkDiscountBetas", i, ".csv"),
                select = c("Estimate", "Pr(>|t|)", "reg", "mod"))
  return(data)
}
beta <- rbindlist(map(c("1a", "1b", "1c1501",
                        #"1c1503", "1c1505", # Missing carb. bevs and cookies
                        "1c1506", "1c1507", "1c1508", "2", "3", "4", "5", "6", "7", "7b"),
                      getCoefs), use.names = TRUE)
beta <- beta[reg == "Store-Week-Brand FE"]
beta[`Pr(>|t|)` > 0.05, "Estimate" := 0]
beta <- beta[, .(beta = Estimate, mod = mod)]

# Getting average package size by income group
# Getting trips and panel data for demographics and to deflate expenditures
trips <- fread("/scratch/upenn/hossaine/fullTrips.csv", nThread = threads,
               select = c("trip_code_uc", "household_code", "panel_year"))
panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("panel_year", "household_code", "projection_factor",
                          "household_income", "age", "dma_cd", "type_of_residence",
                          "household_income_coarse", "married", "carShare",
                          "law", "zip_code", "college", "men", "women",
                          "nChildren"),
               key = c("household_code", "panel_year"))
panel[, "household_income" := as.factor(household_income)]
panel[, "adults" := men + women]

# Getting all purchases and computing the average package size weighted by the
# total amount purchased (i.e. size-weighted package size) for that trip.
fullPurch <- NULL
for (yr in 2004:2017) {
  print(yr)
  purch <- fread(paste0("/scratch/upenn/hossaine/fullPurch", yr, ".csv"),
                 nThread = threads,
                 select = c("trip_code_uc", "product_module_code", "food",
                            "quantity", "totalAmount", "packagePrice"),
                 key = "trip_code_uc")
  purch[, "totalSize" := totalAmount * quantity]
  purch[, "totalExpenditure" := packagePrice * quantity]
  setnames(purch, "totalAmount", "packageSize")
  purch[, c("quantity", "packagePrice") := NULL]
  purch <- purch[, .(packageSize = weighted.mean(packageSize, w = totalSize),
                     totalSize = sum(totalSize),
                     totalExpenditure = sum(totalExpenditure)),
                 by = .(trip_code_uc, product_module_code)]
  fullPurch <- rbindlist(list(fullPurch, purch), use.names = TRUE)
}
fwrite(fullPurch, "/scratch/upenn/hossaine/fullPurchEstSavings.csv", nThread = threads)

# Getting average package size purchased by each household
# Compute the size-weighted average package size purchased for each household
# and the total expenditures for each household.
fullPurch <- fread("/scratch/upenn/hossaine/fullPurchEstSavings.csv", nThread = threads)

fullData <- merge(fullPurch, trips, by = "trip_code_uc")
avgSize <- fullData[, .(lSize = log(weighted.mean(packageSize, w = totalSize)),
                        totalExpenditure = sum(totalExpenditure)),
                    by = .(household_code, panel_year, product_module_code)]
avgSize <- merge(avgSize, panel, by = c("household_code", "panel_year"))

# Getting average size difference by module after adjusting for various
# household characteristics
getDiff <- function(mod) {
  tryCatch({
    print(mod)
    reg1 <- felm(lSize ~ household_income_coarse,
                data = avgSize[product_module_code == mod],
                weights = avgSize[product_module_code == mod]$projection_factor)
    reg2 <- felm(lSize ~ household_income_coarse + married + age + adults +
                   nChildren + type_of_residence + carShare + college,
                data = avgSize[product_module_code == mod],
                weights = avgSize[product_module_code == mod]$projection_factor)
    reg3 <- felm(lSize ~ household_income_coarse + married + age + adults +
                   nChildren + type_of_residence + carShare + college |
                   panel_year + dma_cd,
                data = avgSize[product_module_code == mod],
                weights = avgSize[product_module_code == mod]$projection_factor)
    coefs <- as.data.table(summary(reg3)$coefficients, keep.rownames = TRUE)
    coefs[, "mod" := mod]
    return(coefs)
  }, error = function(e){})
}

mods <- sort(unique(avgSize$product_module_code))
hhDiffs <- rbindlist(map(mods, getDiff), use.names = TRUE)
fwrite(hhDiffs, file = "/scratch/upenn/hossaine/bulkDiscountSavingsEstimated.csv",
       nThread = threads)

# Getting differences in package purchased between richest and poorest households
# Multiplying this difference by bulk discount beta gives % savings for each category
hhDiffs <- fread("/scratch/upenn/hossaine/bulkDiscountSavingsEstimated.csv",
                 nThread = threads)
richDiffs <- hhDiffs[rn == "household_income_coarse>100k"]
richDiffs[`Pr(>|t|)` > 0.05, "Estimate" := 0]
richDiffs <- merge(richDiffs, beta, by = "mod")
richDiffs[, "savings" := Estimate * beta]

# Getting CPI
cpi <- fread("/scratch/upenn/hossaine/cpi.csv")
cpi[, "date" := as.Date(date)]
cpi <- cpi[, .(cpi = mean(value)), by = .(panel_year = year(date))]
cpi2017 <- cpi[panel_year == 2017]$cpi
cpi[, "cpiNew" := cpi / cpi2017 * 100]
avgSize <- merge(avgSize, cpi, by = "panel_year")
avgSize[, "realExp" := totalExpenditure / cpiNew * 100]

# Getting spending weights of each category for the poorest households
poorWeights <- avgSize[household_income_coarse == "<25k",
                       .(realExp = weighted.mean(realExp, w = projection_factor)),
                       by = .(mod = product_module_code)]

# Merge the % savings by module with the spending weights for poor households
# Taking the weighted average of the % savings gives the overall savings
# that poor households could obtain.
# Could save 4.8% on overall basket (~$222 in 2017 dollars) if they bought in bulk like rich
# If they bought exactly like rich (less on some items and more on others),
# savings would be 2.1% (~$146).
finalSavings <- merge(richDiffs, poorWeights, by = "mod")
finalSavings[, .(weighted.mean(savings, w = realExp), sum(realExp * savings))]
finalSavings[savings < 0, .(weighted.mean(savings, w = realExp), sum(realExp * savings))]

# Getting annual expenditures by income group
annualSpend <- avgSize[, .(annualSpend = sum(realExp)),
                       by = .(household_code, panel_year, household_income_coarse,
                              projection_factor)]
annualSpend[, weighted.mean(annualSpend), keyby = household_income_coarse]
