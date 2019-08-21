# Getting the lowest possible price for toilet paper
# The over-arching idea is that I want to get the savings a household
# could have obtained if they had purchased at the unit price of the largest
# brand available during that shopping trip.
# I take the following steps:
# Step 1: From the Scanner data for TP, find the biggest size available for each
# store-brand-week and compute the unit price. If there are multiple large sizes,
# take the minimum unit price. Repeat for each store-brand-week.
# Step 2: Combine this "theoretical" lowest unit price with actual shopping
# behavior recorded in the Homescan data and compute the possible savings that could
# have been realized.
# Step 3: Compute the expenditure-weighted average savings for each household
# Step 4: Compute the projection-weighted average savings for each income group
# Notable limitations: Scanner data is only from a subset of stores that report
# to Nielsen. If this subset is not representative, we may run into a problem
# and I think there is a severe one in this instance. The most popular channels
# to purchase toilet paper are discount stores, grocery stores, warehouse clubs,
# drug stores, and dollar stores. The Scanner data only captures 23% of total TP
# purchase volume in Homescan and has severe holes depending on the store type.
# For discount stores, it only captures 18% of purchases (5% of total)
# For Drug stores, it captures 47% of purchases (2% of total)
# For grocery stores, it captures 41% of purchases (15% of total)
# For warehouse clubs and dollar stores, it captures 0% of purchases
# For these reasons, I will compute savings using the Homescan data. An alternative
# approach would be to find another product for which the Scanner data is
# a better representation of overall spending
library(data.table)
library(lubridate)
library(stargazer)
library(lfe)
library(knitr)
library(ggplot2)
library(ggthemes)
yrs <- 2006:2016
threads <- 8
moduleCode <- 7260 #tp
path <- "/scratch/upenn/hossaine/nielsen_extracts/RMS/"

# Getting number of rolls per package
prod <- fread("/scratch/upenn/hossaine/fullProd.csv")[product_module_code == moduleCode]
prod[, "rolls" := as.integer(multi * size1_amount)]
prod[, "ply" := str_extract_all(upc_descr, "\\s\\dP\\s")]
prod[, "ply" := as.integer(gsub("P", "", ply))]
prod[, "sheet" := str_extract_all(upc_descr, "\\d{2,}S\\s")]
prod[, "sheet" := as.integer(gsub("S", "", sheet))]
prod[, "sheets" := ply * sheet * rolls]
prod[, c("upc_descr", "multi", "size1_amount", "ply", "sheet") := NULL]

# Getting annual selection of products for each store
# I compute the average annual price by taking the sales-weighted average price
fullTP <- NULL
for (yr in yrs) {
  print(yr)
  tp <- fread(paste0(path, yr, "/Movement_Files/4507_", yr, "/", moduleCode, "_", yr, ".tsv"),
              nThread = threads,
              select = c("store_code_uc", "upc", "week_end", "price"))
  upcVer <- fread(paste0(path, yr, "/Annual_Files/rms_versions_", yr, ".tsv"))
  tp <- merge(tp, upcVer, by = "upc")
  tp <- merge(tp, prod, by = c("upc", "upc_ver_uc"))
  tp[, "unitPrice" := price / rolls]

  # Getting min unit price and recording size of min unit price
  tp[, "minUnitPrice" := min(unitPrice), by = .(store_code_uc, brand_code_uc, week_end)]
  tp <- tp[unitPrice == minUnitPrice]
  tp <- unique(tp[, .(store_code_uc, week_end, brand_code_uc, rolls, minUnitPrice)])
  fullTP <- rbindlist(list(fullTP, tp), use.names = TRUE)
}

fwrite(fullTP, "/scratch/upenn/hossaine/fullTP.csv", nThread = threads)

# Getting trips and adding in week_end date
trips <- fread("/scratch/upenn/hossaine/fullTrips.csv", nThread = threads,
               select = c("store_code_uc", "trip_code_uc", "purchase_date",
                          "household_code", "panel_year"))[store_code_uc != 0]

fullPurch <- NULL
for (yr in yrs) {
  print(yr)
  # Getting purchase data and computing unit price paid
  purch <- fread(paste0("/scratch/upenn/hossaine/fullPurch", yr, ".csv"),
                 nThread = threads,
                 select = c("trip_code_uc", "brand_code_uc", "product_module_code",
                            "packagePrice", "quantity", "totalAmount"),
                 key = "trip_code_uc")[product_module_code == moduleCode]
  purch <- merge(purch, trips, by = "trip_code_uc")
  purch[, ':=' (unitPriceChosen = packagePrice / totalAmount,
                totalExp = quantity * packagePrice)]
  purch[, c("packagePrice", "quantity", "product_module_code") := NULL]
  purch[, "week_end" := ceiling_date(as.Date(purchase_date), "week", week_start = 6)]
  purch[, c("purchase_date", "trip_code_uc") := NULL]
  purch[, "week_end" := as.integer(gsub("-", "", week_end))]
  fullPurch <- rbindlist(list(fullPurch, purch), use.names = TRUE)
}
fullPurch[, "totalAnnualSpending" := sum(totalExp), by = .(household_code, panel_year)]

# Combining with Homescan data
fullTP <- fread("/scratch/upenn/hossaine/fullTP.csv", nThread = threads)
choices <- merge(fullTP, fullPurch, by = c("store_code_uc", "week_end", "brand_code_uc"))

# Computing savings and focusing on those associated with larger purchases
choices[, "savings" := (unitPriceChosen - minUnitPrice) / unitPriceChosen]
choices[, "savingsLevel" := (unitPriceChosen - minUnitPrice) * totalAmount]
choices[, "bigger" := (totalAmount < rolls)]

# Combining with household characteristics
panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("panel_year", "household_code", "projection_factor",
                          "household_income_coarse", "men", "women", "age",
                          "nChildren", "married", "college", "dma_cd",
                          "household_income"),
               key = c("household_code", "panel_year"))
panel[, "household_income" := as.factor(household_income)]
choices <- merge(choices, panel, by = c("household_code", "panel_year"))

# Computing missed savings by household
hhSavings <- choices[bigger == TRUE & savings >= 0,
                     .(savings = weighted.mean(savings, w = totalExp),
                       savingsLevel = sum(savingsLevel),
                       totalExp = sum(totalExp)),
                     by = .(household_code, panel_year, household_income_coarse,
                            men, women, age, nChildren, dma_cd, projection_factor,
                            household_income, totalAnnualSpending, married,
                            college)]

kable(hhSavings[men == 1 & women == 1 & nChildren == 2, mean(savings),
                by = household_income_coarse])
# Put this into lowestPrice.tex

# Getting savings levels (inflated to match full annual expenditures)
hhSavings[, "fullSavingsLevel" := savingsLevel * totalAnnualSpending / totalExp]
kable(hhSavings[men == 1 & women == 1 & nChildren == 2,
                mean(fullSavingsLevel), by = household_income_coarse])


reg <- felm(data = hhSavings,
            savings ~ household_income_coarse + age + nChildren + married +
              men + women | as.factor(dma_cd) + as.factor(panel_year),
            weights = hhSavings$projection_factor)
stargazer(reg, type = "text",
          add.lines = list(c("Demographics", "Y"),
                           c("Market-Year FE", "Y")),
          omit = c("age", "nChildren", "men", "women"),
          order = c(2, 3, 1), digits = 3,
          covariate.labels = c("25-50k", "50-100k", ">100k"))

# ===============================================
#   Dependent variable:
#   ---------------------------
#   weekSavings
# -----------------------------------------------
#   25-50k                       -0.005***
#   (0.001)
#
# 50-100k                      -0.013***
#   (0.001)
#
# >100k                        -0.017***
#   (0.002)
#
# -----------------------------------------------
#   Demographics                     Y
# Market-Year FE                   Y
# Observations                  182,415
# R2                             0.071
# Adjusted R2                    0.059
# Residual Std. Error     0.181 (df = 180168)
# ===============================================
#   Note:               *p<0.1; **p<0.05; ***p<0.01





# Computing savings by income group
fullCoefs <- NULL
for (yr in 2006:2016) {
  reg <- felm(data = hhSavings[panel_year < 2014], savings ~ household_income + age +
                nChildren + men + women + as.factor(dma_cd) + as.factor(panel_year) + 0)
  coefs <- as.data.table(summary(reg)$coefficients, keep.rownames = TRUE)
  coefs[, "panel_year" := yr]
  fullCoefs <- rbindlist(list(fullCoefs, coefs))
}


graphData <- fullCoefs[grepl("household_income", rn)]
graphData[, "rn" := gsub("household_income", "", rn)]
graphData[, "rn" := factor(rn, levels = c(8, 10, 11, 13, 15, 16, 17, 18, 19, 21, 23, 26, 27),
                           labels = c(11, 13.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 55, 65, 85, 100),
                           ordered = TRUE)]
graphData[, "rn" := as.numeric(as.character(rn))]
setnames(graphData, c("rn", "beta", "se", "t", "p", "year"))

# Graphing
ggplot(data = graphData, aes(x = rn, y = beta)) +
  geom_errorbar(aes(ymin = beta - 1.96 * se, ymax = beta + 1.96 * se), width = 0.2) +
  geom_point() +
  # geom_hline(yintercept = 0) +
  # geom_vline(xintercept = 0) +
  facet_wrap(vars(year)) +
  labs(title = "Rich Households Bulk Buy More",
       x = "Household Income", y = "Share of Purchases",
       caption = paste0("Source: Author calulations from Nielsen Consumer Panel. \n",
                        "Note: Demographic adjustments control for household size, \n",
                        "age, and presence of children. Income-specific time trends \n",
                        "are also controlled for.")) +
  theme_tufte() +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0)) +
  scale_color_grey()
