# Tests hypotheses about transportation costs
# 1. Vehicle access
# 2. Average number of products purchased on each shopping trip
# 3. Average shopping distance
library(data.table)
library(lfe)
library(stargazer)
library(ggplot2)
library(ggthemes)
library(Hmisc)
yr <- 2004:2017
threads <- 8

# Car shares
panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("panel_year", "household_code", "household_income",
                          "men", "women", "age", "nChildren", "fips", "projection_factor",
                          "household_income_coarse", "married", "carShare",
                          "law", "college", "type_of_residence"),
               key = c("household_code", "panel_year"))

round(wtd.quantile(panel$carShare, weights = panel$projection_factor,
                   seq(0, 1, 0.01)), 2)

reg1 <- felm(data = panel, carShare ~ household_income_coarse,
             weights = panel$projection_factor)
reg2 <- felm(data = panel, carShare ~ household_income_coarse + age + men + women +
               nChildren + married + college + type_of_residence | fips + panel_year,
             weights = panel$projection_factor)
stargazer(reg1, reg2, type = "text")

# Tallying number of products purchased on each shopping trip
fullTally <- NULL
for (i in yr) {
  purch <- fread(paste0("/scratch/upenn/hossaine/fullPurch", i, ".csv"),
                 nThread = threads, select = c("trip_code_uc", "quantity", "multi"))
  tally <- purch[, .(nProds = sum(quantity * multi)), by = trip_code_uc]
  fullTally <- rbindlist(list(fullTally, tally), use.names = TRUE)
}

trips <- fread("/scratch/upenn/hossaine/fullTrips.csv", nThread = threads,
               select = c("trip_code_uc", "household_code", "panel_year"))
panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("panel_year", "household_code", "household_income",
                          "men", "women", "age", "nChildren", "fips", "projection_factor",
                          "household_income_coarse", "married", "carShare",
                          "law", "college", "type_of_residence"),
               key = c("household_code", "panel_year"))

fullData <- merge(fullTally, trips, by = "trip_code_uc")
fullData <- merge(fullData, panel, by = c("household_code", "panel_year"))
fullData[, "adults" := men + women]
fullData[, c("men", "women") := NULL]
fwrite(fullData, file = "/scratch/upenn/hossaine/shoppingProducts.csv", nThread = threads)

# Running regressions
fullData <- fread("/scratch/upenn/hossaine/shoppingProducts.csv", nThread = threads)
fullData[, "household_income" := as.factor(household_income)]
reg1 <- felm(nProds ~ household_income,
             data = fullData, weights = fullData$projection_factor)
reg2 <- felm(nProds ~ household_income + adults + nChildren + age + married + college +
               type_of_residence + carShare, data = fullData,
             weights = fullData$projection_factor)
reg3 <- felm(nProds ~ household_income + adults + nChildren + age + married + college +
               type_of_residence + carShare | panel_year + fips,
             data = fullData, weights = fullData$projection_factor)

stargazer(reg1, reg2, reg3, type = "text",
          add.lines = list(c("Market FE's", "N", "N", "Y"),
                           c("Year FE's",   "N", "N", "Y")),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          dep.var.caption = "", dep.var.labels.include = FALSE,
          omit = c("Constant"),
          covariate.labels = c("8-10k", "10-12k", "12-15k", "15-20k", "20-25k",
                               "25-30k", "30-35k", "35-40k", "40-45k", "45-50k",
                               "50-60k", "60-70k", "70-100k", ">100k", "Adults",
                               "Children", "Age", "Married", "College",
                               "Single-Family Home", "% Car Access"),
          notes.align = "l",
          notes.append = TRUE,
          digits = 3,
          notes = c("Source: Author calulations from Nielsen Consumer Panel."),
          label = "tab:transportationCostsItem",
          title = "Correlation of Number of Products Purchased and Demographics",
          out = "tables/AppendixTransportationCostsItem.tex")

# Graphing coefs
reg1Coefs <- as.data.table(summary(reg1)$coefficients, keep.rownames = TRUE)
reg1CI <- as.data.table(confint(reg1), keep.rownames = TRUE)
reg1Total <- merge(reg1Coefs, reg1CI, by = "rn")
reg1Total[, "reg" := "Income Only"]

reg2Coefs <- as.data.table(summary(reg2)$coefficients, keep.rownames = TRUE)
reg2CI <- as.data.table(confint(reg2), keep.rownames = TRUE)
reg2Total <- merge(reg2Coefs, reg2CI, by = "rn")
reg2Total[, "reg" := "Income + Demographics"]

reg3Coefs <- as.data.table(summary(reg3)$coefficients, keep.rownames = TRUE)
reg3CI <- as.data.table(confint(reg3), keep.rownames = TRUE)
reg3Total <- merge(reg3Coefs, reg3CI, by = "rn")
reg3Total[, "reg" := "Income + Demographics + FEs"]

finalCoefs <- rbindlist(list(reg1Total, reg2Total, reg3Total), use.names = TRUE)
graphData <- finalCoefs[grepl("household_income", rn)]
graphData[, "rn" := gsub("household_income", "", rn)]
graphData[, "rn" := factor(rn, levels = c(4, 6, 8, 10, 11, 13, 15, 16, 17, 18, 19, 21, 23, 26, 27),
                           labels = c(6.5, 9, 11, 13.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 55, 65, 85, 100),
                           ordered = TRUE)]
graphData[, "rn" := as.numeric(as.character(rn))]
setnames(graphData, c("household_income", "beta", "se", "t", "p", "LCL", "UCL", "reg"))

ggplot(data = graphData[reg == "Income + Demographics + FEs"],
       aes(x = household_income, y = beta, color = reg, shape = reg)) +
  geom_line() +
  geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.2) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  labs(x = "Annual Household Income ($000s)",
       y = "Diff in Product Purchases",
       color = "Reg",
       shape = "Reg") +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom",
        text = element_text(size = 14),
        axis.ticks.length = unit(0.25, "cm")) +
  scale_color_fivethirtyeight()
ggsave(filename = "./figures/transportationCostsItem.pdf", height = 4, width = 6)

# Distance
trips <- fread("/scratch/upenn/hossaine/fullTrips.csv", nThread = threads,
               select = c("household_code", "panel_year", "dist", "store_code_uc"))
trips <- trips[store_code_uc != 0]
trips[, "dist" := dist / 1000]
panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("panel_year", "household_code", "household_income",
                          "men", "women", "age", "nChildren", "fips", "projection_factor",
                          "household_income_coarse", "married", "carShare",
                          "law", "college", "type_of_residence"),
               key = c("household_code", "panel_year"))
fullData <- merge(trips, panel, by = c("household_code", "panel_year"))
fullData[, "adults" := men + women]
fullData[, c("men", "women") := NULL]

# Running regressions and dropping any trips further than 100km
reg1 <- felm(dist ~ household_income_coarse, data = fullData[dist < 100],
             weights = fullData[dist < 100]$projection_factor)
reg2 <- felm(dist ~ household_income_coarse + adults + nChildren +
               age + married + college + type_of_residence + carShare,
             data = fullData[dist < 100],
             weights = fullData[dist < 100]$projection_factor)
reg3 <- felm(dist ~ household_income_coarse + adults + nChildren +
               age + married + college + type_of_residence + carShare |
               panel_year + fips, data = fullData[dist < 100],
             weights = fullData[dist < 100]$projection_factor)
stargazer(reg1, reg2, reg3, type = "text",
          add.lines = list(c("Demographics", "N", "Y", "Y"),
                           c("Market FE's", "N", "N", "Y"),
                           c("Year FE's",   "N", "N", "Y")),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          dep.var.caption = "", dep.var.labels.include = FALSE,
          keep = c("household_income_coarse"),
          order = c(2, 3, 1),
          covariate.labels = c("25-50k", "50-100k", ">100k"),
          notes.align = "l",
          notes.append = TRUE,
          digits = 3,
          notes = c("Source: Author calulations from Nielsen Consumer Panel."),
          label = "tab:transportationCostsDist",
          title = "Correlation of Distance to Store and Demographics",
          out = "tables/AppendixTransportationCostsDist.tex")

