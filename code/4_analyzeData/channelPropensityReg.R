library(data.table)
library(purrr)
library(furrr)
library(lfe)
library(stargazer)
library(plotly)
plan(multiprocess)
path <- "/home/mallick/Desktop/Nielsen/Data/Clean/"
yr <- 2004:2016
#stores <- c("Grocery", "Warehouse Club", "Discount Store", "Drug/Dollar Store")
stores <- c("Grocery", "Warehouse Club", "Discount Store", "Dollar Store")

# Getting retailers
retailers <- fread(paste0(path, "retailers.tsv"))
#retailers[channel_type %in% c("Drug Store", "Dollar Store"), "channel_type" := "Drug/Dollar Store"]
retailers <- retailers[channel_type %in% stores]

panel <- fread(paste0(path, "fullPanel.csv"),
               select = c("household_code", "panel_year", "projection_factor",
                          "household_income", "household_size", "marital_status",
                          "race", "hispanic_origin", "age", "market"))

# Getting panel demographic data
getData <- function(yr) {
  print(yr)
  panel <- panel[panel_year == yr]
  channels <- setDT(expand.grid(household_code = unique(panel$household_code),
                                panel_year = unique(panel$panel_year),
                                channel_type = unique(stores)))
  panel <- merge(channels, panel, by = c("household_code", "panel_year"), all.x = TRUE)

  # Getting Nielsen purchases by trip
  purch <- fread(paste0(path, "Purchases/purchase", yr, ".csv"),
                 select = c("trip_code_uc", "total_price_paid"))
  purch <- purch[, .(total_price_paid = sum(total_price_paid)), by = .(trip_code_uc)]

  # Getting trips
  trips <- fread(paste0(path, "Trips/trips", yr, ".csv"),
                 select = c("trip_code_uc", "household_code", "retailer_code", "panel_year"))

  # Merging with purchases
  trips <- merge(trips, purch, by = "trip_code_uc")
  trips <- merge(trips, retailers, by = "retailer_code")

  # Getting monthly spending by channel and household
  trips <- trips[, .(count = .N), by = .(household_code, panel_year, channel_type)]
  fullData <- merge(trips, panel, all.y = TRUE,
                    by = c("household_code", "panel_year", "channel_type"))
  fullData[is.na(count), "count" := 0]
  fullData[, "visit" := ifelse(count > 0, 1L, 0L)]
  return(fullData)
}

fullData <- rbindlist(future_map(yr, getData))
fullData[, "market" := as.factor(market)]

# Warehouse Clubs
regWare <- felm(data = fullData[channel_type == "Warehouse Club"],
             visit ~ household_income,
             weights = fullData[channel_type == "Warehouse Club"]$projection_factor)

regWareFull <- felm(data = fullData[channel_type == "Warehouse Club"],
             visit ~ household_income | as.factor(panel_year) : market +
                     as.factor(household_size) + as.factor(marital_status) +
                     as.factor(race) + as.factor(hispanic_origin) + as.factor(age) | 0 | market,
             weights = fullData[channel_type == "Warehouse Club"]$projection_factor)

stargazer(regWare, regWareFull, type = "text")

# Discount Stores
regDisc <- felm(data = fullData[channel_type == "Discount Store"],
             visit ~ household_income,
             weights = fullData[channel_type == "Discount Store"]$projection_factor)

regDiscFull <- felm(data = fullData[channel_type == "Discount Store"],
             visit ~ household_income | as.factor(panel_year) : market +
               as.factor(household_size) + as.factor(marital_status) +
               as.factor(race) + as.factor(hispanic_origin) + as.factor(age) | 0 | market,
             weights = fullData[channel_type == "Discount Store"]$projection_factor)

stargazer(regDisc, regDiscFull, type = "text")

# Grocery
regGroc <- felm(data = fullData[channel_type == "Grocery"],
             visit ~ household_income,
             weights = fullData[channel_type == "Grocery"]$projection_factor)

regGrocFull <- felm(data = fullData[channel_type == "Grocery"],
             visit ~ household_income | as.factor(panel_year) : market +
               as.factor(household_size) + as.factor(marital_status) +
               as.factor(race) + as.factor(hispanic_origin) + as.factor(age) | 0 | market,
             weights = fullData[channel_type == "Grocery"]$projection_factor)

stargazer(regGroc, regGrocFull, type = "text")

# Dollar Stores
regDoll <- felm(data = fullData[channel_type == "Dollar Store"],
             visit ~ household_income,
             weights = fullData[channel_type == "Dollar Store"]$projection_factor)

regDollFull <- felm(data = fullData[channel_type == "Dollar Store"],
             visit ~ household_income | as.factor(panel_year) : market +
               as.factor(household_size) + as.factor(marital_status) +
               as.factor(race) + as.factor(hispanic_origin) + as.factor(age) | 0 | market,
             weights = fullData[channel_type == "Dollar Store"]$projection_factor)

stargazer(regDoll, regDollFull, type = "text")

stargazer(regGrocFull, regDiscFull, regDollFull, regWareFull,
          column.labels = c("Grocery", "Discount", "Dollar", "Warehouse"),
          add.lines = list(c("Household Demographics", "Y", "Y", "Y", "Y"),
                           c("Year-Market FE", "Y", "Y", "Y", "Y")),
          single.row = FALSE, no.space = FALSE, type = "latex",
          dep.var.labels.include = FALSE,
          dep.var.caption = "Visit",
          covariate.labels = c("25-50k", "50-100k", ">100k"),
          order = c(2, 3, 1),
          notes.align = "l",
          omit.stat = c("ser", "rsq"),
          digits = 3,
          out = "./code/6_paper/tables/channelPropensity.tex")

# Getting correlation matrix for counts
fullDataWide <- dcast(fullData, ... ~ channel_type, value.var = c("count", "visit"))
corTable <- cor(fullDataWide[, .(count_Grocery, `count_Discount Store`,
                                 `count_Dollar Store`, `count_Warehouse Club`)])
dimnames(corTable) <- list(c("Grocery", "Discount", "Dollar", "Warehouse"),
                           c("Grocery", "Discount", "Dollar", "Warehouse"))
stargazer(corTable, type = "latex", out = "./code/6_paper/tables/channelCor.tex")

# Making visit histogram
fullDataWide[, "code" := as.factor(paste0(visit_Grocery, `visit_Discount Store`,
                                          `visit_Dollar Store`, `visit_Warehouse Club`))]
plot_ly(data = fullDataWide[household_income == ">100k"]) %>%
  add_histogram(x = ~code, histnorm = "probability")

