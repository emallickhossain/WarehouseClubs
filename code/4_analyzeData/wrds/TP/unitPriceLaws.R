# Examines effects of unit price regulations on bulk buying
library(data.table)
library(ggplot2)
library(ggthemes)
library(ggridges)
library(lfe)
library(purrr)
library(stringr)
library(stargazer)
library(forcats)
threads <- 8

# Making map of unit price laws in 2017
states_map <- setDT(map_data("state"))
graphData <- fread("./code/0_data/nist130.csv", select = c("state", "type"))
graphData[type == "", "type" := NA]
graphData[type == "mandatory", "type" := "Mandatory"]
graphData[type == "voluntary", "type" := "Voluntary"]

stateRef <- data.table(state = state.abb, region = tolower(state.name))
graphData <- merge(graphData, stateRef, by = "state")

states_map <- merge(states_map, graphData, by = "region")

ggplot(states_map, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = type), color = "gray") +
  labs(fill = "Regulation Status") +
  theme_tufte() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom") +
  scale_fill_grey(na.value = gray(0.9))
ggsave(filename = "./code/5_figures/unitPriceLaws.png", height = 4, width = 6)

# Getting data from discountingBehavior.R
# Adding demographics
discBehaviorAll <- fread("/scratch/upenn/hossaine/discBehaviorAll.csv", nThread = threads)
discBehaviorFood <- fread("/scratch/upenn/hossaine/discBehaviorFood.csv", nThread = threads)

panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("panel_year", "household_code", "projection_factor",
                          "household_income", "household_size", "age", "child",
                          "dma_cd", "household_income_coarse", "married",
                          "carShare", "law", "fips", "zip_code"),
               key = c("household_code", "panel_year"))
panel[, "fips" := str_pad(fips, width = 5, side = "left", pad = "0")]
panel[, "state" := as.integer(substr(fips, 1, 2))]
panel[, "mandatory" := ifelse(state %in% c(9, 11, 24, 25, 33, 34, 36, 41, 44, 50), 1L, 0L)]
panel[, "household_income" := as.factor(household_income)]
panel[, "household_income_coarse" := as.factor(household_income_coarse)]
panel[, "lawInd" := (law >= 3)]
panel[, "lawCoarse" := fct_collapse(as.factor(law),
                                    yes = c("1", "2"),
                                    yesStar = "3",
                                    no = c("4", "5"))]
panel[, "type" := "None"]
panel[lawInd == TRUE, "type" := "Voluntary"]
panel[mandatory == 1, "type" := "Mandatory"]
panel[, "type" := relevel(as.factor(type), ref = "None")]

discBehaviorAll <- merge(discBehaviorAll, panel, by = c("household_code", "panel_year"))
discBehaviorFood <- merge(discBehaviorFood, panel, by = c("household_code", "panel_year"))

# Cross section
reg1 <- felm(bulk ~ lawInd + household_income_coarse + married + age + household_size + child |
               dma_cd + panel_year,
             data = discBehaviorAll)
reg2 <- felm(bulk ~ lawInd * household_income_coarse + married + age + household_size + child |
               dma_cd + panel_year,
             data = discBehaviorAll)
reg3 <- felm(bulk ~ lawInd * household_income_coarse + married + age + household_size + child |
               dma_cd + panel_year,
             data = discBehaviorFood[food == 0])
reg4 <- felm(bulk ~ lawInd * household_income_coarse + married + age + household_size + child |
               dma_cd + panel_year,
             data = discBehaviorFood[food == 1])
stargazer(reg1, reg2, reg3, reg4, type = "text",
          add.lines = list(c("Demographic Controls", "Y", "Y", "Y", "Y"),
                           c("Market FE's", "Y", "Y", "Y", "Y"),
                           c("Year FE's", "Y", "Y", "Y", "Y")),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("All", "Food", "Non-Food"),
          column.separate = c(2, 1, 1),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          keep = c("law*", "*income"),
          order = c(1, 10, 11, 9, 3, 4, 2),
          covariate.labels = c("Law",
                               " . : 25-50k", " . : 50-100k", " . : >100k",
                               "25-50k", "50-100k", ">100k"),
          notes.align = "l",
          notes.append = TRUE,
          digits = 3,
          out = "tables/unitPriceLaw.tex")

regType <- felm(bulk ~ type * household_income_coarse + married + age + household_size + child |
               dma_cd + panel_year,
             data = discBehaviorAll)
stargazer(regType, type = "text",
          add.lines = list(c("Demographic Controls", "Y"),
                           c("Market FE's", "Y", "Y"),
                           c("Year FE's", "Y", "Y")),
          single.row = TRUE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          dep.var.caption = "", dep.var.labels.include = FALSE,
          keep = c("type*", "*income"),
          order = c(1, 12, 14, 10, 2, 13, 15, 11, 4, 5, 3),
          covariate.labels = c("Mandatory",
                               " . : 25-50k", " . : 50-100k", " . : >100k",
                               "Voluntary",
                               " . : 25-50k", " . : 50-100k", " . : >100k",
                               "25-50k", "50-100k", ">100k"),
          notes.align = "l",
          notes.append = TRUE,
          digits = 3,
          out = "tables/unitPriceLawType.tex")














# Looking at price law transitions
movers <- unique(discBehaviorAll[, .(household_code, panel_year, lawInd)])
setorder(movers, household_code, panel_year, lawInd)
movers[, "lawCount" := uniqueN(lawInd), by = household_code]
movers[, "lagLaw" := shift(lawInd, 1, type = "lag"), by = household_code]
with(movers[!is.na(lagLaw)], table(lawInd, lagLaw))

# Regressions using unit price laws
discBehaviorFood[, "hhInc" := paste(household_code, household_income_coarse, sep = "_")]
discBehaviorFood[, "hhIncZip" := paste(household_code,
                                       household_income_coarse, zip_code, sep = "_")]

discBehaviorAll[, "hhInc" := paste(household_code, household_income_coarse, sep = "_")]

# Cross-sectional
reg1 <- felm(data = discBehaviorFood[food == 1],
             bulk ~ lawInd * household_income_coarse + married + age +
               household_size + child | dma_cd + panel_year | 0 | dma_cd,
             weights = discBehaviorFood[food == 1]$projection_factor)
reg2 <- felm(data = discBehaviorFood[food == 0],
             bulk ~ lawInd * household_income_coarse + married + age +
               household_size + child | dma_cd + panel_year | 0 | dma_cd,
             weights = discBehaviorFood[food == 0]$projection_factor)

# Movers
reg3 <- felm(data = discBehaviorFood[food == 0],
             bulk ~ lawInd + married + age +
               household_size + child | hhInc + panel_year)
reg3a <- felm(data = discBehaviorFood[food == 0],
              bulk ~ lawInd + married + age +
                household_size + child | hhIncZip + panel_year)
reg4 <- felm(data = discBehaviorFood[food == 1],
             bulk ~ lawInd * household_income_coarse + married + age +
               household_size + child | hhInc + panel_year)
reg5 <- felm(data = discBehaviorFood[food == 0],
             bulk ~ lawInd + married + age +
               household_size + child | hhInc + panel_year)
reg6 <- felm(data = discBehaviorFood[food == 0],
             bulk ~ lawInd * household_income_coarse + married + age +
               household_size + child | hhInc + panel_year)

stargazer(reg1, reg2, reg3, reg4, reg5, reg6, type = "text",
          add.lines = list(c("Market FE", "Y", "Y", "N", "N"),
                           c("Year FE", "Y", "Y", "Y", "Y"),
                           c("Household-Income FE", "N", "N", "Y", "Y")),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("Full Sample", "Movers"),
          column.separate = c(2, 4),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          keep = c("lawInd*"),
          order = c(1, 10, 11, 9),
          covariate.labels = c("Law", "25-50k : Law", "50-100k : Law", ">100k : Law"),
          notes.align = "l",
          digits = 3,
          notes = c("Full sample standard errors are clustered at the DMA level."),
          notes.append = TRUE,
          label = "tab:unitPriceLaw",
          title = "Unit Price Laws and Bulk Buying")
,
          out = "tables/unitPriceLaw.tex")

# Creating and plotting event study
setorder(discBehaviorAll, household_code, panel_year)
discBehaviorAll[, "lagLaw" := shift(lawInd, 1, type = "lag"),
                 by = .(household_code)]
move <- unique(discBehaviorAll[lagLaw != lawInd,
                                .(household_code, change_year = panel_year,
                                  lawInd)])
move[, "changeType" := ifelse(lawInd, "Add Law", "No Law")]
move[, "lawInd" := NULL]
discBehaviorAll <- merge(discBehaviorAll, move, by = "household_code")
discBehaviorAll[, "tChange" := panel_year - change_year]
discBehaviorAll[, "tChange" := relevel(factor(tChange), ref = "-1")]
regEvent1 <- felm(data = discBehaviorAll[tChange %in% paste(-2:2) &
                                           changeType == "No Law"],
                  bulk ~ tChange + married + age + household_size + child |
                    hhInc + panel_year)
regEvent2 <- felm(data = discBehaviorAll[tChange %in% paste(-2:2) &
                                           changeType == "Add Law"],
                  bulk ~ tChange + married + age + household_size + child |
                    hhInc + panel_year)
coefs1 <- as.data.table(summary(regEvent1)$coefficients,
                        keep.rownames = TRUE)[, "type" := "Without Law"]
confInt1 <- as.data.table(confint(regEvent1), keep.rownames = TRUE)
coefs1 <- merge(coefs1, confInt1, by = "rn")
coefs2 <- as.data.table(summary(regEvent2)$coefficients,
                        keep.rownames = TRUE)[, "type" := "With Law"]
confInt2 <- as.data.table(confint(regEvent2), keep.rownames = TRUE)
coefs2 <- merge(coefs2, confInt2, by = "rn")
finalCoefs <- rbindlist(list(coefs1, coefs2), use.names = TRUE)
finalCoefs <- finalCoefs[grepl("tChange", rn)]
finalCoefs[, "rn" := as.integer(gsub("tChange", "", rn))]
setnames(finalCoefs, c("rn", "beta", "se", "t", "p", "law", "LCL", "UCL"))
finalCoefs <- rbindlist(list(finalCoefs,
                             list(-1, 0, 0, 0, 0, "Without Law", 0, 0),
                             list(-1, 0, 0, 0, 0, "With Law", 0, 0)))

ggplot(data = finalCoefs, aes(x = rn, y = beta, color = law)) +
  geom_errorbar(aes(ymin = LCL,
                    ymax = UCL), width = 0.2) +
  geom_line() +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  labs(x = "Years After Moving",
       y = "Difference in Bulk Purchasing (Percentage Points)",
       color = "Law Status After Household Move") +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom") +
  scale_color_grey()

ggsave(filename = "./figures/unitPriceEventStudy.png", height = 4, width = 6)
