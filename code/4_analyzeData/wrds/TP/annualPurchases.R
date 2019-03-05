# Looks at contributors to annual consumption
# Gets package size purchase differences between rich and poor households after
# controlling for household characteristics.
library(data.table)
library(lfe)
library(stargazer)
library(glmnet)
library(doMC)
library(ggplot2)
library(ggthemes)
registerDoMC(cores = 4)
threads <- 8
source("./Nielsen/runReg.R")
path <- "/scratch/upenn/hossaine/"

tpPurch <- fread(paste0(path, "7260Purch.csv"), nThread = threads)[drop == 0]
cols <- c("panel_year", "household_income", "household_size", "type_of_residence",
          "marital_status", "hispanic_origin", "market", "college",
          "urban", "white", "child")
tpPurch[, (cols) := lapply(.SD, as.factor), .SDcols = cols]
tpPurch[, "unitCost" := total_price_paid / totVol]
tpPurch <- na.omit(tpPurch, cols = "unitCost")
tpPurch <- tpPurch[unitCost > 0]

# Sarah's regression looking directly at unit cost by size and income.
reg1 <- felm(data = tpPurch, log(unitCost) ~ log(size) + log(size) * household_income |
              type_of_residence + household_size + marital_status + white + child +
              hispanic_origin + age + urban + college + panel_year + market | 0 | market,
            weights = tpPurch$projection_factor)
reg2 <- felm(data = tpPurch, log(unitCost) ~ log(size) + log(size) * household_income + rate |
               type_of_residence + household_size + marital_status + white + child +
               hispanic_origin + age + urban + college + panel_year + market | 0 | market,
             weights = tpPurch$projection_factor)
reg3 <- felm(data = tpPurch, log(unitCost) ~ log(size) + log(size) * household_income + rate |
              type_of_residence + household_size + marital_status + white + child +
              hispanic_origin + age + urban + college + panel_year + market +
               brand_code_uc | 0 | market,
            weights = tpPurch$projection_factor)
reg4 <- felm(data = tpPurch, log(unitCost) ~ log(size) + log(size) * household_income + rate |
              type_of_residence + household_size + marital_status + white + child +
              hispanic_origin + age + urban + college + panel_year + market +
               + brand_code_uc + retailer_code | 0 | market,
            weights = tpPurch$projection_factor)
stargazer(reg1, reg2, reg3, reg4, type = "text",
          single.row = TRUE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("Log(Unit Cost)"),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          covariate.labels = c("Log(Size)",
                               "5-8k", "8-10k", "10-12k", "12-15k", "15-20k",
                               "20-25k", "25-30k", "30-35k", "35-40k", "40-45k",
                               "45-50k", "50-60k", "60-70k", "70-100k", ">100k",
                               "Cons.Rate",
                               "Log(Size) : 5-8k", "Log(Size) : 8-10k",
                               "Log(Size) : 10-12k", "Log(Size) : 12-15k",
                               "Log(Size) : 15-20k", "Log(Size) : 20-25k",
                               "Log(Size) : 25-30k", "Log(Size) : 30-35k",
                               "Log(Size) : 35-40k", "Log(Size) : 40-45k",
                               "Log(Size) : 45-50k", "Log(Size) : 50-60k",
                               "Log(Size) : 60-70k", "Log(Size) : 70-100k",
                               "Log(Size) : >100k"),
          add.lines = list(c("Time/MSA/Demog. FE", "Y", "Y", "Y", "Y"),
                           c("Brand FE", "N", "N", "Y", "Y"),
                           c("Retailer FE", "N", "N", "N", "Y")),
          notes.align = "l",
          notes = c("Market and year fixed effects included. Standard errors are ",
                    "clustered at the market level."),
          digits = 2,
          label = "tab:IncUnitCost",
          title = "Richer Households Can Obtain Same Sizes for Less",
          out = "./tables/IncUnitCost.tex")

incomes <- c(2.5, 6.5, 9, 11, 13.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 55, 65, 85, 100)
betas <- c(0, reg4$coefficients[18:32])
se <- c(0, reg4$cse[18:32])
graphData <- data.table(income = incomes, beta = betas, se = se)
ggplot(graphData, aes(x = incomes, y = betas)) +
  geom_errorbar(aes(ymin = betas - 1.96 * se, ymax = betas + 1.96 * se), width = 1) +
  geom_line() +
  geom_hline(yintercept = 0) +
  labs(title = "Rich Households Obtain Same Sizes for Cheaper",
       x = "Household Income",
       y = "Log Unit Cost",
       caption = paste0("Source: Author calulations using Nielsen Consumer Panel.\n",
                        "Midpoints of household income bins are plotted above.\n",
                        "Figure plots coefficients of package size interacted with \n",
                        "household income after controlling for household demographics \n",
                        "as well as brand, store, market, and year fixed effects.")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text())
ggsave(filename = "./figures/IncUnitCost.png")

# Identifying predictors of usage
rateData <- unique(tpPurch[, .(household_code, panel_year, projection_factor,
                               household_income, household_size, type_of_residence,
                               marital_status, hispanic_origin, market, market, age,
                               college, white, child, urban, rate)])
rateData <- na.omit(rateData, cols = "rate")
rateData <- rateData[rate < 1 & rate > 0.01]

reg <- felm(data = rateData, rate ~ household_income + type_of_residence +
              household_size + marital_status + white + child +
              hispanic_origin + age + urban + college | market + panel_year,
            weights = rateData$projection_factor)
summary(reg)


stargazer(reg, type = "text",
          single.row = TRUE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("Average Daily Consumption"),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          covariate.labels = c("5-8k", "8-10k", "10-12k", "12-15k", "15-20k",
                               "20-25k", "25-30k", "30-35k", "35-40k", "40-45k",
                               "45-50k", "50-60k", "60-70k", "70-100k", ">100k",
                               "Multi-Family Home", "Single-Family Home",
                               "2 people", "3 people", "4 people", "5 people",
                               "6 people", "7 people", "8 people", "9+ people",
                               "Widowed", "Divorced", "Single",
                               "White", "Child Present", "Not Hispanic",
                               "Age", "Urban", "College"),
          notes.align = "l",
          notes = c("Market and year fixed effects included. ",
                    "Standard errors are clustered",
                    "at the market level. Omitted ",
                    "categories are the following: ",
                    "<5k income, mobile homes, ",
                    "1 person households, married ",
                    "couples, non-whites, ",
                    "households without children, ",
                    "Hispanics, rural areas, and non-college"),
          digits = 2,
          label = "tab:annualTP",
          title = "Annual TP Purchases",
          out = "./tables/annualTP.tex")

# Using elastic net
x <- sparse.model.matrix(~ -1 + as.factor(panel_year) + as.factor(household_income) +
                           as.factor(household_size) + as.factor(type_of_residence) +
                           as.factor(marital_status) + as.factor(hispanic_origin) +
                           as.factor(market) + age + as.factor(college) + as.factor(urban) +
                           as.factor(white) + as.factor(child), data = rateData)
y <- rateData$rate
regLasso <- cv.glmnet(x, y, weights = rateData$projection_factor, alpha = 1,
                      parallel = TRUE, nfolds = 20)
coef(regLasso, lambda = "lambda.1se")
