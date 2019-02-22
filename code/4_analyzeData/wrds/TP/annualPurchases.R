# Looks at contributors to annual consumption
# Gets package size purchase differences between rich and poor households after
# controlling for household characteristics.
library(data.table)
library(lfe)
library(stargazer)
library(glmnet)
library(doMC)
registerDoMC(cores = 4)
source("./Nielsen/runReg.R")
path <- "/scratch/upenn/hossaine/"

tpPurch <- fread(paste0(path, "7260Purch.csv"))
cols <- c("panel_year", "household_income", "household_size", "type_of_residence",
          "marital_status", "hispanic_origin", "market", "age", "college",
          "urban", "white", "child")
tpPurch[, (cols) := lapply(.SD, as.factor), .SDcols = cols]
tpPurch <- tpPurch[type_of_residence != ""]

reg <- felm(data = tpPurch, log(unitCost) ~ log(size) + log(size) * household_income |
              type_of_residence + household_size + marital_status + white + child +
              hispanic_origin + age + urban + college + brand_code_uc + retailer_code +
              panel_year + market | 0 | market,
            weights = tpPurch$projection_factor)
summary(reg)

tpPurch <- tpPurch[, .(avgCost = weighted.mean(unitCost, w = size)),
                   by = .(panel_year, household_code, projection_factor,
                          household_income, household_size, type_of_residence,
                          marital_status, hispanic_origin, market, age,
                          college, urban, white, child, total)]

reg <- felm(data = tpPurch, total ~ household_income +
              type_of_residence + household_size + marital_status + white + child +
              hispanic_origin + age + urban + college |
              panel_year + market | 0 | market,
            weights = tpPurch$projection_factor)
stargazer(reg, type = "text",
          single.row = TRUE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("Annual Purchases"),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          covariate.labels = c("12-15k", "15-20k", "20-25k", "25-30k", "30-35k",
                               "35-40k", "40-45k", "45-50k", "50-60k", "60-70k",
                               "70-100k", ">100k", "Home", "Mobile Home", "2 people",
                               "3 people", "4 people", "5+ people", "Widowed", "Divorced",
                               "Single", "White", "Child Present", "Hispanic",
                               "Age 45-64", "Age 65+", "Urban", "College Degree"),
          notes.align = "l",
          notes = c("Standard errors are clustered",
                    "at the market level. Omitted ",
                    "categories are the following: ",
                    "10-12k income, apartments, ",
                    "1 person households, married ",
                    "couples, and 18-44 year olds"),
          digits = 2,
          label = "tab:annualTP",
          title = "Annual TP Purchases",
          out = "./tables/annualTP.tex")

# Using elastic net
x <- sparse.model.matrix(~ -1 + as.factor(panel_year) + as.factor(household_income) +
                           as.factor(household_size) + as.factor(type_of_residence) +
                           as.factor(marital_status) + as.factor(hispanic_origin) +
                           as.factor(market) + as.factor(household_composition) +
                           as.factor(age) + as.factor(college) + as.factor(urban) +
                           as.factor(white) + as.factor(child), data = tpPurch)
y <- tpPurch$total
regLasso <- cv.glmnet(x, y, weights = tpPurch$projection_factor, alpha = 1, parallel = TRUE)
apple <- as.matrix(coef(regLasso, lambda = "lambda.1se"))

# Coefficients:
# Estimate Cluster s.e. t value Pr(>|t|)
# household_income10      -7.342e-03    1.732e-02  -0.424  0.67173
# household_income11       7.913e-03    1.947e-02   0.406  0.68439
# household_income13       1.005e-02    1.779e-02   0.565  0.57213
# household_income15       3.239e-03    1.591e-02   0.204  0.83869
# household_income16      -2.610e-02    1.602e-02  -1.629  0.10323
# household_income17      -1.151e-02    1.790e-02  -0.643  0.52018
# household_income18       3.721e-05    1.630e-02   0.002  0.99818
# household_income19      -4.806e-03    1.715e-02  -0.280  0.77930
# household_income21       1.460e-03    1.630e-02   0.090  0.92863
# household_income23       1.469e-02    1.721e-02   0.853  0.39339
# household_income26      -9.877e-03    1.755e-02  -0.563  0.57353
# household_income27      -9.896e-03    1.669e-02  -0.593  0.55322
# type_of_residenceHome    4.895e-02    8.739e-03   5.601 2.13e-08 ***
# type_of_residenceMobile  3.704e-02    1.373e-02   2.698  0.00698 **
# household_size2          4.357e-01    1.179e-02  36.956  < 2e-16 ***
# household_size3          6.022e-01    1.421e-02  42.368  < 2e-16 ***
# household_size4          7.031e-01    1.376e-02  51.092  < 2e-16 ***
# household_size5+         7.962e-01    1.657e-02  48.041  < 2e-16 ***
# marital_status2         -3.257e-02    1.146e-02  -2.842  0.00449 **
# marital_status3         -8.039e-02    7.550e-03 -10.647  < 2e-16 ***
# marital_status4         -1.351e-01    9.268e-03 -14.576  < 2e-16 ***
# white1                  -5.997e-04    7.505e-03  -0.080  0.93631
# child1                  -9.998e-02    7.991e-03 -12.512  < 2e-16 ***
# hispanic_origin2        -8.521e-02    1.158e-02  -7.361 1.83e-13 ***
# age45-64                 1.478e-01    6.901e-03  21.411  < 2e-16 ***
# age65+                   1.176e-01    8.707e-03  13.503  < 2e-16 ***
# urban1                   3.023e-04    8.281e-03   0.037  0.97088
# college1                -1.155e-01    5.971e-03 -19.342  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 33.24 on 574355 degrees of freedom
# Multiple R-squared(full model): 0.1358   Adjusted R-squared: 0.1356
# Multiple R-squared(proj model): 0.1267   Adjusted R-squared: 0.1265
# F-statistic(full model, *iid*):716.2 on 126 and 574355 DF, p-value: < 2.2e-16
# F-statistic(proj model): 509.9 on 28 and 86 DF, p-value: < 2.2e-16
