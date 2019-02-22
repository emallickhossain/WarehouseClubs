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
cols <- c("panel_year", "household_code", "projection_factor", "household_income",
          "household_size", "type_of_residence", "marital_status", "hispanic_origin", "market",
          "household_composition", "age", "college", "urban", "white", "child", "total")

tamponPurch <- na.omit(unique(fread(paste0(path, "7270Purch.csv"), select = cols)))

cols <- c("household_income", "type_of_residence", "household_size",
          "marital_status", "white", "child", "hispanic_origin", "age",
          "urban", "college", "household_composition", "panel_year", "market")
tamponPurch[, (cols) := lapply(.SD, as.factor), .SDcols = cols]
tamponPurch <- tamponPurch[type_of_residence != ""]

reg <- felm(data = tamponPurch, total ~ household_income +
              type_of_residence + household_size + marital_status + white + child +
              hispanic_origin + age + urban + college |
              panel_year + market | 0 | market,
            weights = tamponPurch$projection_factor)
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
          notes = c("Standard errors are clustered at the market level."),
          digits = 2,
          label = "tab:annualTampon",
          title = "Annual Tampon Purchases",
          out = "./tables/annualTampon.tex")

# Using elastic net
x <- sparse.model.matrix(~ -1 + as.factor(panel_year) + as.factor(household_income) +
                           as.factor(household_size) + as.factor(type_of_residence) +
                           as.factor(marital_status) + as.factor(hispanic_origin) +
                           as.factor(market) + as.factor(household_composition) +
                           as.factor(age) + as.factor(college) + as.factor(urban) +
                           as.factor(white) + as.factor(child), data = tamponPurch)
y <- tamponPurch$total
regLasso <- cv.glmnet(x, y, weights = tamponPurch$projection_factor, alpha = 1, parallel = TRUE)
regMixed <- cv.glmnet(x, y, weights = tamponPurch$projection_factor, alpha = 0.5, parallel = TRUE)
coef(regLasso, lambda = "lambda.1se")
coef(regMixed, lambda = "lambda.1se")
