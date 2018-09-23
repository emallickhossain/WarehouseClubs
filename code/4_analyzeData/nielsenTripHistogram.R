library(data.table)
library(plotly)
library(purrr)
library(lubridate)
library(fredr)
library(zoo)
fredr_set_key(fredAPI)
years <- 2004:2016

# Getting cpi
cpi <- setDT(fredr("CPIAUCSL", observation_start = as.Date("2004-01-01")))[, "series_id" := NULL]

getHist <- function(year) {
  # Getting shopping trips
  trips <- fread(paste0("/home/mallick/Desktop/Nielsen/Data/Consumer_Panel_Data_",
                        "2004-2016/nielsen_extracts/HMS/", year,
                        "/Annual_Files/trips_", year, ".tsv"))
  trips[, "purchase_date" := parse_date_time(purchase_date, "%Y-%m-%d")]
  trips[, c("year", "month") := .(year(purchase_date), month(purchase_date))]

  panel <- fread(paste0("/home/mallick/Desktop/Nielsen/Data/Consumer_Panel_Data_",
                        "2004-2016/nielsen_extracts/HMS/", year,
                        "/Annual_Files/panelists_", year, ".tsv"),
                 select = c("Household_Cd", "Panel_Year", "Projection_Factor",
                            "Household_Income", "Household_Size", "Marital_Status",
                            "Household_Internet_Connection"))
  setnames(panel, tolower(names(panel)))
  setnames(panel, "household_cd", "household_code")
  panel[household_income %in% 1:13, "income" := "<$25k"]
  panel[household_income %in% 15:23, "income" := "<$70k"]
  panel[household_income > 23, "income" := ">$70k"]
  panel[, "size" := ifelse(household_size > 4, "4+", as.character(household_size))]
  panel[, c("household_income", "household_size") := NULL]

  # Real spending
  trips[, "date" := as.Date(paste0(year, "-", month, "-01"))]
  trips <- merge(trips, cpi, by = "date")
  trips[, "realSpending" := total_spent / value * 100]

  # Weighted histograph of spending by year and income group
  trips <- merge(trips, panel, by = c("household_code", "panel_year"))
  chart <- ggplot(trips, aes(x = realSpending, weight = projection_factor)) +
    geom_histogram(data = trips[income == "<$25k"], fill = "blue", alpha = 0.3) +
    geom_histogram(data = trips[income == "<$70k"], fill = "orange", alpha = 0.3) +
    geom_histogram(data = trips[income == ">$70k"], fill = "green", alpha = 0.3)
  ggplotly(chart)
}
