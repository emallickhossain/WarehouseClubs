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
yrs <- 2004:2017
modCode <- 7260
path <- "/scratch/upenn/hossaine/"

# Loading product data
prod <- fread(paste0(path, "fullProd.csv"), nThread = threads)
prod <- prod[product_module_code %in% modCode]
prod[, "ply" := str_extract_all(upc_descr, "\\s\\dP\\s")]
prod[, "ply" := as.integer(gsub("P", "", ply))]
prod[, "sheet" := str_extract_all(upc_descr, "\\d{2,}S\\s")]
prod[, "sheet" := as.integer(gsub("S", "", sheet))]
prod[, "sheets" := ply * sheet * totalAmount]
prod[, c("upc_descr", "multi", "size1_amount", "ply", "sheet") := NULL]

# Loading trips data
trips <- fread(paste0(path, "fullTrips.csv"), nThread = threads,
               select = c("trip_code_uc", "household_code", "panel_year", "purchase_date"))

# Loading panel data
panel <- fread(paste0(path, "fullPanel.csv"), nThread = threads)

# Getting purchase data by household year
fullPurch <- NULL
for (i in yrs) {
  purch <- fread(paste0(path, "fullPurch", i, ".csv"), nThread = threads,
                 select = c("trip_code_uc", "upc", "upc_ver_uc", "quantity",
                            "product_module_code"))[product_module_code %in% modCode]
  fullPurch <- rbindlist(list(fullPurch, purch), use.names = TRUE)
}

# Combining all TP purchases together
fullPurch <- merge(fullPurch, prod, by = c("upc", "upc_ver_uc", "product_module_code"))
fullPurch <- fullPurch[, .(product_module_code, trip_code_uc, quantity,
                           sheets, totalAmount)]
fullPurch <- merge(fullPurch, trips, by = "trip_code_uc")[, "trip_code_uc" := NULL]
fullPurch[, "purchase_date" := as.Date(as.character(purchase_date))]
fullPurch[, "fullQ" := ifelse(product_module_code == modCode, sheets * quantity,
                              totalAmount * quantity)]
fullPurch[, c("sheets", "quantity", "totalAmount") := NULL]
setorder(fullPurch, household_code, panel_year, product_module_code, purchase_date)

# Getting first and last purchase dates and final purchase amount
finalSheet <- fullPurch[, .(start_date = head(purchase_date, 1),
                            end_date = tail(purchase_date, 1),
                            final = tail(fullQ, 1)),
                        by = .(household_code, panel_year, product_module_code)]
finalSheet[, "days" := as.integer(end_date - start_date)]
finalSheet[, c("start_date", "end_date") := NULL]

# Getting annual purchases and removing final purchase from tally
annualPurch <- fullPurch[, .(fullQ = sum(fullQ, na.rm = TRUE)),
                         by = .(household_code, panel_year, product_module_code)]
annualPurch <- merge(annualPurch, finalSheet,
                     by = c("household_code", "panel_year", "product_module_code"))
annualPurch[, "consRate" := (fullQ - final) / days]
annualPurch[, c("fullQ", "final", "days") := NULL]

# Merging with panel data to do predictive regression
annualPurch <- merge(annualPurch, panel, by = c("household_code", "panel_year"))
annualPurch[, "household_income" := as.factor(household_income)]
annualPurch <- annualPurch[consRate > 0 & consRate < Inf]
annualPurch[, "top1" := quantile(consRate, 0.99), by = product_module_code]
annualPurch[, "bottom1" := quantile(consRate, 0.01), by = product_module_code]
annualPurch <- annualPurch[consRate < top1 & consRate > bottom1]

# Plotting consumption rate distribution by income and household size
ggplot(data = annualPurch[product_module_code == modCode],
       aes(x = consRate, fill = household_income_coarse)) +
  geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.5, position = "identity") +
  geom_density(alpha = 0.8) +
  facet_wrap(vars(household_size)) +
  labs(title = "Average Daily Consumption Does Not Vary By Income",
       x = "Average Daily Consumption (Sheets / Day)",
       y = "Density",
       caption = paste0("Source: Author calculations from Nielsen Consumer Panel.")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0)) +
  scale_fill_grey()

########################## REGRESSION AND ELASTIC NET ##########################
regData <- annualPurch[product_module_code == modCode]
regData[, "age2" := age^2]
reg1 <- felm(consRate ~ household_income, data = regData)
reg2 <- felm(consRate ~ household_income + household_size +
               age + married + child, data = regData)
reg3 <- felm(consRate ~ household_income + household_size +
               age + married + child + white + hispanic_origin, data = regData)
reg4 <- felm(consRate ~ household_income + household_size +
               age + married + child + white + hispanic_origin | dma_cd, data = regData)
stargazer(reg1, reg2, reg3, reg4, type = "text", omit.stat = c("f", "ser"))

reg1Coef <- as.data.table(summary(reg1)$coefficients, keep.rownames = TRUE)
reg1Coef[, "reg" := "Income Only"]

reg2Coef <- as.data.table(summary(reg2)$coefficients, keep.rownames = TRUE)
reg2Coef[, "reg" := "Income and Demographics"]

finalCoefs <- rbindlist(list(reg1Coef, reg2Coef), use.names = TRUE)

# Organizing graph data
graphData <- finalCoefs[grepl("household_income", rn)]
graphData[, "rn" := gsub("household_income", "", rn)]
graphData[, "rn" := factor(rn, levels = c(8, 10, 11, 13, 15, 16, 17, 18, 19, 21, 23, 26, 27),
                           labels = c(11, 13.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 55, 65, 85, 100),
                           ordered = TRUE)]
graphData[, "rn" := as.numeric(as.character(rn))]
setnames(graphData, c("rn", "beta", "se", "t", "p", "reg"))

# Graphing Data
ggplot(data = graphData, aes(x = rn, y = beta, color = reg)) +
  geom_errorbar(aes(ymin = beta - 1.96 * se, ymax = beta + 1.96 * se), width = 0.2) +
  geom_point(aes(shape = reg), size = 3) +
  geom_hline(yintercept = 0) +
  labs(title = "TP Consumption Does Not Vary By Income",
       x = "Household Income", y = "Average Consumption Rate (Sheets / Day)",
       caption = paste0("Source: Author calulations from Nielsen Consumer Panel. \n",
                        "Note: Income coefficients from regression of average daily \n",
                        "consumption on income only and after controlling for household \n",
                        "size, age, marital status, and presence of children."),
       shape = "Covariates", color = "Covariates") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0)) +
  scale_color_grey()
ggsave(filename = "figures/annualPurchasesTP.png")

# Using elastic net
x <- sparse.model.matrix(~ -1 + as.factor(panel_year) + as.factor(household_income) +
                           household_size + as.factor(type_of_residence) +
                           married + as.factor(hispanic_origin) +
                           as.factor(dma_cd) + age + college + white + child,
                         data = regData)
y <- regData$consRate
regLasso <- cv.glmnet(x, y, alpha = 0.5, parallel = TRUE, nfolds = 100)
coef(regLasso, lambda = "lambda.1se")



# Sarah's regression looking directly at average unit cost paid by income.
# Running this on a purchase level gives positive income coefficients (mostly due to brand preferences)
# Adding store and brand fixed effects then gets slight negative coefficients,
# but I think it ends up being a little underpowered.
# If it's not underpowered, then there's no difference in unit prices paid between
# different households. In the context of my paper, this is okay because while the
# stylized fact is interesting, I focus on the first-best that households could achieve
# which is different than the difference between the richest and poorest.
tpPurch <- fread(paste0(path, "7260Purch.csv"), nThread = threads)[drop == 0]
cols <- c("panel_year", "household_income", "household_size", "type_of_residence",
          "marital_status", "hispanic_origin", "market", "college",
          "urban", "white", "child")
tpPurch[, (cols) := lapply(.SD, as.factor), .SDcols = cols]
tpPurch[, "unitCost" := total_price_paid / totVol]
tpPurch <- na.omit(tpPurch, cols = "unitCost")
tpPurch <- tpPurch[unitCost > 0]

tpPurch[, "unitCostUnadj" := total_price_paid / sizeUnadj]
topBrands <- c("ANGEL SOFT", "CHARMIN", "COTTONELLE", "QUILTED NORTHERN", "SCOTT", "CTL BR")
tpPurch[, "brandStore" := paste0(brand_descr, retailer_code)]
reg1 <- felm(data = tpPurch[brand_descr %in% topBrands], log(unitCost) ~ as.factor(household_income) |
               brand_code_uc + retailer_code | 0 | market,
             weights = tpPurch[brand_descr %in% topBrands]$projection_factor)
summary(reg1)

incomes <- c(2.5, 6.5, 9, 11, 13.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 55, 65, 85, 100)
betas <- c(0, reg1$coefficients[1:15])
se <- c(0, reg1$cse[1:15])
graphData <- data.table(income = incomes, beta = betas, se = se)
ggplot(graphData, aes(x = incomes, y = betas)) +
  geom_errorbar(aes(ymin = betas - 1.96 * se, ymax = betas + 1.96 * se), width = 1) +
  geom_line() +
  geom_hline(yintercept = 0) +
  labs(title = "Package Size Purchased Increases in Income", x = "Household Income",
       y = "Log TP Rolls Per Package",
       caption = paste0("Source: Author calulations using Nielsen Consumer Panel.\n",
                        "Note: TP rolls are standardized to 225-sheet, 2-ply rolls.\n",
                        "Midpoints of household income bins are plotted above.")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text())
ggsave(filename = "./figures/tpCoefficients.png")


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
