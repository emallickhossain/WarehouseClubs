library(data.table)
library(lfe)
library(stargazer)
library(ggplot2)
library(ggthemes)
library(stringr)
library(zoo)
threads <- 8

################################################################################
# ANALYSIS OF TOILET PAPER PURCHASES TO VALIDATE MODEL PREDICTIONS
################################################################################
fullPurch <- fread("/scratch/upenn/hossaine/fullTPPurchases.csv", nThread = threads)
panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("household_code", "panel_year", "projection_factor",
                          "household_income_coarse", "type_of_residence", "dma_cd",
                          "age", "men", "women", "nChildren", "college", "married",
                          "carShare", "law", "fips"))
prod <- fread("/scratch/upenn/hossaine/prodTP.csv")

# Adjusting unit price regulations
panel[, "lawInd" := (law >= 3)]
panel[, "fips" := str_pad(fips, 5, "left", "0")]
panel[, "state" := as.integer(substr(fips, 1, 2))]
panel[lawInd == FALSE, "mandatory" := "None"]
panel[lawInd == TRUE, "mandatory" := ifelse(state %in%
                                              c(9, 11, 24, 25, 33, 34, 36, 41, 44, 50),
                                            "Mandatory", "Voluntary")]
panel[lawInd == FALSE, "display" := "None"]
panel[lawInd == TRUE, "display" := mandatory]
panel[lawInd == TRUE & state %in% c(6, 9, 25, 34, 36, 44), "display" := "Display"]
panel[, "mandatory" := relevel(as.factor(mandatory), ref = "None")]
panel[, "display" := relevel(as.factor(display), ref = "None")]

# Isolating to grocery and discount stores
fullPurch <- merge(fullPurch, prod, by.x = c("upc_choice", "upc_ver_uc_choice"),
                   by.y = c("upc", "upc_ver_uc"))
fullPurch <- fullPurch[, .(totalSheet, household_code, panel_year, channel_type)]
fullPurch <- merge(fullPurch, panel, by = c("household_code", "panel_year"))

# Generating regulation indicator and days' supply
fullPurch[, "days" := totalSheet / (57 * 2)]
fullPurch[state == 6, "display" := "Voluntary"]
fullPurch[, "hhInc" := paste(household_income_coarse, household_code, sep = "_")]
fullPurch[, "adults" := men + women]

# Adding mover information
setorder(fullPurch, household_code, panel_year)
fullPurch[, "lagLaw" := shift(lawInd, 1, type = "lag"),
                    by = .(household_code)]
fullPurch[, "move" := lawInd - lagLaw]
fullPurch[!move %in% c(-1, 1), "move" := NA]
fullPurch[, "move" := na.locf(move), by = household_code] #filling all NAs with previous non-na value
fullPurch[move == 1 & lawInd == FALSE, "move" := 0]
fullPurch[move == -1 & lawInd == TRUE, "move" := 0]
fullPurch[, "move" := relevel(as.factor(move), ref = "0")]

# Getting regression table
# Movers (symmetric effects, no difference between move to and move away from regs)
reg1 <- felm(log(days) ~ lawInd | household_code + panel_year | 0 | household_code,
             data = fullPurch)
reg2 <- felm(log(days) ~ lawInd + household_income_coarse + men + women + nChildren +
               married + carShare + type_of_residence + college + age |
               household_code + panel_year | 0 | household_code,
             data = fullPurch)
stargazer(reg1, reg2, type = "text",
          add.lines = list(c("Household FE", "Y", "Y"),
                           c("Year FE", "Y", "Y"),
                           c("Demographics", "N", "Y")),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          dep.var.caption = "", dep.var.labels.include = FALSE,
          keep = c("lawInd", "type_of_residence"),
          covariate.labels = c("Regulation", "Single-Family Home"),
          notes.align = "l",
          digits = 3,
          out = "tables/unitPriceLawMoversTP.tex")
