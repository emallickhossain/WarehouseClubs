# Gets predictive covariates of package size purchase
library(data.table)
library(glmnet)
library(doMC)
registerDoMC(cores = 4)
source("./Nielsen/runReg.R")
path <- "/scratch/upenn/hossaine/"
tamponPurch <- na.omit(fread(paste0(path, "7270Purch.csv"),
                             select = c("panel_year", "month", "retailer_code",
                                        "brand_code_uc", "household_income",
                                        "household_size", "type_of_residence",
                                        "marital_status", "hispanic_origin", "market",
                                        "household_composition", "age", "college",
                                        "urban", "white", "child", "size1_amount",
                                        "projection_factor")))

x <- sparse.model.matrix(~ -1 + as.factor(panel_year) + as.factor(month) +
                           as.factor(retailer_code) + as.factor(brand_code_uc) +
                           as.factor(household_income) + as.factor(household_size) +
                           as.factor(type_of_residence) +
                           as.factor(marital_status) + as.factor(hispanic_origin) +
                           as.factor(market) + as.factor(household_composition) +
                           as.factor(age) + as.factor(college) + as.factor(urban) +
                           as.factor(white) + as.factor(child), data = tamponPurch)
y <- as.factor(tamponPurch$size1_amount)
weights <- tamponPurch$projection_factor

regLasso <- cv.glmnet(x, y, family = "multinomial", alpha = 1, weights = weights, parallel = TRUE)
regMixed <- cv.glmnet(x, y, family = "multinomial", alpha = 0.5, weights = weights, parallel = TRUE)
coef(regLasso, lambda = "lambda.1se")
coef(regMixed, lambda = "lambda.1se")
