# Gets unit costs for top products
library(data.table)
library(purrr)
library(furrr)
library(plotly)
library(lfe)
library(stargazer)
plan(multiprocess)
path <- "/home/mallick/Desktop/Nielsen/Data/Clean/"
yr <- 2004

# 7260: Toilet tissue
# 1344: Cereal
# 1290: Soup
# 1484: Carbonated drinks
# 1493: Candy (Chocolate)
# 1323: Potato chips
# 7734 (not sure the difference with 7255 and 7256): Paper towels
# 8404: Tooth cleaners
# 1498: Candy (non-chocolate)
# 7880: Lamps (Incandescent)
# 8449: Deodorant (personal)
## 7008: Detergents-Light duty
## 1487: Bottled water
## 7270: Tampons
## 8444: Diapers
mods <- c(1487, 7003, 7008, 7060, 7242, 7245, 7250, 7255, 7256, 7260, 7265, 7270, 8444)

prod <- fread(paste0(path, "prod.csv"),
              select = c("upc", "upc_ver_uc", "product_module_code", "multi",
                         "size1_amount", "size1_units", "brand_code_uc", "brand_descr"))
prod <- prod[product_module_code %in% mods]
prod[, "size" := multi * size1_amount]
prod[, c("multi", "size1_amount") := NULL]

panel <- fread(paste0(path, "fullPanel.csv"))
panel[household_income == "<25k", "income" := 1]
panel[household_income == "25-50k", "income" := 2]
panel[household_income == "50-100k", "income" := 3]
panel[household_income == ">100k", "income" := 4]

getSales <- function(yr) {
  purch <- fread(paste0(path, "Purchases/purchase", yr, ".csv"),
                 select = c("upc", "upc_ver_uc", "total_price_paid",
                            "trip_code_uc", "quantity"))
  purch <- merge(purch, prod, by = c("upc", "upc_ver_uc"))
  purch[, ':=' (unitCost = total_price_paid / (quantity * size),
                unitsSold = quantity * size)]
  purch[, c("total_price_paid", "quantity", "upc", "upc_ver_uc", "size") := NULL]

  trips <- fread(paste0(path, "Trips/trips", yr, ".csv"),
                 select = c("trip_code_uc", "household_code", "panel_year", "retailer_code"))
  purch <- merge(purch, trips, by = "trip_code_uc")
  return(purch)
}

fullData <- rbindlist(map(yr, getSales))
fullData <- merge(fullData, panel, by = c("household_code", "panel_year"))
retailers <- fread(paste0(path, "Master_Files/Latest/retailers.tsv"))
fullData <- merge(fullData, retailers, by = c("retailer_code"))
fullData[, "market" := as.factor(market)]
fullData[, "cents" := unitCost * 100]

# Regressions ###########
getReg <- function(mod, size, prodName) {
  print(prodName)
  regData <- fullData[product_module_code == mod & size1_units == size]
  reg1 <- felm(data = regData, formula = cents ~ as.factor(income) |
                 0 | 0 | market, weights = regData$projection_factor)

  reg2 <- felm(data = regData, formula = cents ~ as.factor(income) |
                 as.factor(household_size) + as.factor(marital_status) +
                 as.factor(race) + as.factor(hispanic_origin) +
                 as.factor(age) + as.factor(panel_year) | 0 | market, weights = regData$projection_factor)

  reg3 <- felm(data = regData, formula = cents ~ as.factor(income) |
                 as.factor(household_size) +  as.factor(marital_status) +
                 as.factor(race) + as.factor(hispanic_origin) +
                 as.factor(age) + as.factor(panel_year) +
                 as.factor(brand_code_uc) |
                 0 | market, weights = regData$projection_factor)

  reg4 <- felm(data = regData, formula = cents ~ as.factor(income) |
                 as.factor(household_size) + as.factor(marital_status) +
                 as.factor(race) + as.factor(hispanic_origin) +
                 as.factor(age) + as.factor(panel_year) +
                 as.factor(retailer_code) |
                 0 | market, weights = regData$projection_factor)

  reg5 <- felm(data = regData, formula = cents ~ as.factor(income) |
                 as.factor(household_size) + as.factor(marital_status) +
                 as.factor(race) + as.factor(hispanic_origin) +
                 as.factor(age) + as.factor(panel_year) +
                 as.factor(brand_code_uc) + as.factor(retailer_code) |
                 0 | market, weights = regData$projection_factor)
  stargazer(reg1, reg2, reg3, reg4, reg5,
            add.lines = list(c("Household Demographics", "N", "Y", "Y", "Y", "Y"),
                             c("Brand FE", "N", "N", "Y", "N", "Y"),
                             c("Store FE", "N", "N", "N", "Y", "Y")),
            single.row = FALSE, no.space = FALSE, type = "latex",
            dep.var.labels.include = FALSE,
            dep.var.caption = "Unit Costs (cents/unit)",
            covariate.labels = c("25-50k", "50-100k", ">100k"),
            notes.align = "l",
            omit.stat = c("ser", "rsq"),
            digits = 3,
            out = paste0("./code/6_paper/tables/", prodName, ".tex"))
}

getReg(1487, "OZ", "water")
getReg(7003, "OZ", "detergentPackaged")
getReg(7008, "OZ", "detergentLight")
getReg(7245, "CT", "tissue")
getReg(7270, "CT", "tampon")
getReg(8444, "CT", "diaper")
getReg(7260, "CT", "tp")


# Quantities ###########
getQuant <- function(mod, size, prodName) {
  print(prodName)
  regData <- fullData[product_module_code == mod & size1_units == size]
  reg1 <- felm(data = regData, formula = unitsSold ~ as.factor(income) |
                 0 | 0 | market, weights = regData$projection_factor)

  reg2 <- felm(data = regData, formula = unitsSold ~ as.factor(income) |
                 as.factor(household_size) + as.factor(marital_status) +
                 as.factor(race) + as.factor(hispanic_origin) +
                 as.factor(age) + as.factor(panel_year) | 0 | market, weights = regData$projection_factor)

  reg3 <- felm(data = regData, formula = unitsSold ~ as.factor(income) |
                 as.factor(household_size) +  as.factor(brand_code_uc) +
                 as.factor(marital_status) + as.factor(race) + as.factor(hispanic_origin) +
                 as.factor(age) + as.factor(panel_year)|
                 0 | market, weights = regData$projection_factor)

  reg4 <- felm(data = regData, formula = unitsSold ~ as.factor(income) |
                 as.factor(household_size) +
                 as.factor(marital_status) + as.factor(race) + as.factor(hispanic_origin) +
                 as.factor(age) + as.factor(panel_year) + as.factor(retailer_code) |
                 0 | market, weights = regData$projection_factor)

  reg5 <- felm(data = regData, formula = unitsSold ~ as.factor(income) |
                 as.factor(brand_code_uc) + as.factor(household_size) +
                 as.factor(marital_status) + as.factor(race) + as.factor(hispanic_origin) +
                 as.factor(age) + as.factor(panel_year) + as.factor(retailer_code) |
                 0 | market, weights = regData$projection_factor)
  stargazer(reg1, reg2, reg3, reg4, reg5,
            add.lines = list(c("Household Demographics", "N", "Y", "Y", "Y", "Y"),
                             c("Brand FE", "N", "N", "Y", "N", "Y"),
                             c("Store FE", "N", "N", "N", "Y", "Y")),
            single.row = FALSE, no.space = FALSE, type = "latex",
            dep.var.labels.include = FALSE,
            dep.var.caption = "Units Sold",
            covariate.labels = c("25-50k", "50-100k", ">100k"),
            notes.align = "l",
            omit.stat = c("ser", "rsq"),
            digits = 3,
            out = paste0("./code/6_paper/tables/", prodName, "Q.tex"))
}

getQuant(1487, "OZ", "water")
getQuant(7003, "OZ", "detergentPackaged")
getQuant(7008, "OZ", "detergentLight")
getQuant(7245, "CT", "tissue")
getQuant(7270, "CT", "tampon")
getQuant(8444, "CT", "diaper")
getQuant(7260, "CT", "tp")
