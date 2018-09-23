# Looks at spending changes by department_code
library(data.table)
library(plotly)
library(fredr)
library(zoo)
library(purrr)
fredr_set_key(fredAPI)
yr <- 2004:2016

pce <- setDT(fredr("PCEPI", observation_start = as.Date("2004-01-01"),
                   frequency = "q", aggregation_method = "eop"))
pce[, c("date", "series_id") := .(as.yearqtr(date), NULL)]
setnames(pce, "date", "purchase_date")

getSpend <- function(yr) {
  print(yr)
  fullData <- fread(paste0("/home/mallick/Desktop/Nielsen/Data/Clean/FullData/",
                           "fullData", yr, ".csv"),
                    select = c("household_code", "panel_year", "projection_factor",
                               "household_income", "department_code", "purchase_date",
                               "p", "quantity"))
  fullData[, "purchase_date" := as.yearqtr(purchase_date)]
  fullData[, "year" := year(purchase_date)]
  hhSpend <- fullData[year == yr, .(spend = sum(p * quantity)),
                      by = .(household_code, panel_year, projection_factor,
                             household_income, department_code, purchase_date)]
  return(hhSpend)
}
hhSpend <- rbindlist(map(yr, getSpend))
hhSpend <- merge(hhSpend, pce, by = "purchase_date")
hhSpend[, "realSpend" := spend / value * 100]

# Aggregate chart
graphData <- hhSpend[, .(realSpend = sum(realSpend)),
                     by = .(household_code, panel_year, projection_factor,
                            household_income, purchase_date)]
graphData <- graphData[, .(realSpend = weighted.mean(realSpend, w = projection_factor)),
                       by = .(household_income, purchase_date)]
plot_ly(data = graphData, x = ~purchase_date) %>%
  add_lines(y = ~realSpend, split = ~household_income)

# Split by department
graphData <- hhSpend[, .(spend = weighted.mean(spend, w = projection_factor)),
                     by = .(household_income, department_code, purchase_date)]
getDept <- function(dept) {
  chart <- plot_ly(data = graphData[department_code == dept], x = ~purchase_date) %>%
    add_lines(y = ~spend, split = ~household_income)
  return(chart)
}
charts <- rbindlist(map(0:9, getDept))
subplot(charts, ncol = 3)
