# Computes household propensity for different discounting behaviors within retail
# chain and within zip code
library(data.table)
library(ggplot2)
library(ggthemes)
library(ggridges)
library(lfe)
library(purrr)
library(stringr)
library(stargazer)
threads <- 8

# Getting trips and panel data for demographics and to deflate expenditures
trips <- fread("/scratch/upenn/hossaine/fullTrips.csv", nThread = threads,
               select = c("trip_code_uc", "household_code", "panel_year",
                          "purchase_date", "retailer_code", "store_code_uc"))
retailers <- fread("/scratch/upenn/hossaine/fullRetailers.csv")
trips <- merge(trips, retailers, by = "retailer_code")
trips[, c("yearMonth", "purchase_date") := .(substr(purchase_date, 1, 7), NULL)]

# Getting CPI to deflate values to Jan-2010 base
cpi <- fread("/scratch/upenn/hossaine/cpi.csv")
cpi[, "base" := cpi[date == "2010-01-01"]$value]
cpi[, "newIndex" := value / base * 100]
cpi[, c("base", "value") := NULL]
cpi[, "yearMonth" := .(substr(date, 1, 7))]
cpi[, "date" := NULL]

# Combining with CPI for price deflation
setkey(trips, yearMonth)
setkey(cpi, yearMonth)
fullData <- merge(trips, cpi, by = "yearMonth")[, "yearMonth" := NULL]

# Housekeeping
rm(trips, cpi)
setkey(fullData, trip_code_uc)

# Getting all purchases and coding them by discounting behavior
discBehaviorZIP <- NULL
discBehaviorStore <- NULL
discBehaviorRetailer <- NULL
for (yr in 2004:2017) {
  print(yr)
  purch <- fread(paste0("/scratch/upenn/hossaine/fullPurch", yr, ".csv"),
                 nThread = threads,
                 select = c("trip_code_uc", "product_module_code", "food",
                            "packagePrice", "quintile", "quantity"),
                 key = "trip_code_uc")[food == 0]
  purch[, ':=' (bulk = as.integer(quintile >= 4),
                totalExpenditure = packagePrice * quantity)]
  purch[, c("packagePrice", "quantity") := NULL]

  # Deflating expenditures
  setkey(purch, trip_code_uc)
  purch <- merge(purch, fullData, by = "trip_code_uc")[, "trip_code_uc" := NULL]
  purch[, "realExp" := totalExpenditure / newIndex * 100]
  purch[, c("totalExpenditure", "newIndex") := NULL]

  # Getting household propensities by product type
  householdAvgZIP <- purch[, .(bulk = weighted.mean(bulk, w = realExp)),
                           by = .(household_code, panel_year)]
  householdAvgStore <- purch[, .(bulk = weighted.mean(bulk, w = realExp)),
                             by = .(household_code, panel_year, store_code_uc)]
  householdAvgRetailer <- purch[, .(bulk = weighted.mean(bulk, w = realExp)),
                                by = .(household_code, panel_year, retailer_code, channel_type)]

  # Combining
  discBehaviorZIP <- rbindlist(list(discBehaviorZIP, householdAvgZIP),
                               use.names = TRUE)
  discBehaviorStore <- rbindlist(list(discBehaviorStore, householdAvgStore),
                                 use.names = TRUE)
  discBehaviorRetailer <- rbindlist(list(discBehaviorRetailer, householdAvgRetailer),
                                    use.names = TRUE)
}

# Saving
fwrite(discBehaviorZIP, "/scratch/upenn/hossaine/discBehaviorZIP.csv", nThread = threads)
fwrite(discBehaviorStore, "/scratch/upenn/hossaine/discBehaviorStore.csv", nThread = threads)
fwrite(discBehaviorRetailer, "/scratch/upenn/hossaine/discBehaviorRetailer.csv", nThread = threads)

# Adding demographics
discBehaviorZIP <- fread("/scratch/upenn/hossaine/discBehaviorZIP.csv", nThread = threads)
discBehaviorStore <- fread("/scratch/upenn/hossaine/discBehaviorStore.csv",
                           nThread = threads)[store_code_uc != 0]
discBehaviorRetailer <- fread("/scratch/upenn/hossaine/discBehaviorRetailer.csv", nThread = threads)

panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("panel_year", "household_code", "projection_factor",
                          "household_income", "men", "women", "age", "nChildren",
                          "dma_cd", "household_income_coarse", "married", "college",
                          "carShare", "law", "zip_code", "fips"),
               key = c("household_code", "panel_year"))
panel[, "household_income" := as.factor(household_income)]
panel[, "household_income_coarse" := as.factor(household_income_coarse)]
panel[, "lawInd" := (law >= 3)]

discBehaviorZIP <- merge(discBehaviorZIP, panel, by = c("household_code", "panel_year"))
discBehaviorStore <- merge(discBehaviorStore, panel, by = c("household_code", "panel_year"))
discBehaviorRetailer <- merge(discBehaviorRetailer, panel, by = c("household_code", "panel_year"))

# Making graphs of identifying variation for retailers
ex <- discBehaviorRetailer[men == 1 & women == 1 & nChildren == 2 & married == 1 &
                             college == 1 & panel_year == 2017]
ex[, "household_income" := factor(household_income,
                                  levels = c(4, 6, 8, 10, 11, 13, 15, 16, 17,
                                             18, 19, 21, 23, 26, 27),
                                  labels = c(6.5, 9, 11, 13.5, 17.5, 22.5, 27.5,
                                             32.5, 37.5, 42.5, 47.5, 55, 65, 85, 100),
                                  ordered = TRUE)]
ex[, "household_income" := as.numeric(as.character(household_income))]
ggplot(data = ex, aes(x = household_income, y = bulk, color = channel_type)) +
  geom_point(size = 1) +
  geom_jitter(width = 0.7) +
  geom_smooth(method = "lm", na.rm = TRUE) +
  facet_wrap(vars(channel_type)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  labs(x = "Annual Household Income ($000s)",
       y = "Annual Bulk Buying Share",
       color = "Channel Type") +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom") +
  scale_color_grey()
  # scale_color_colorblind()
ggsave(filename = "./figures/discountingBehaviorFERetailerVariation.pdf",
       width = 6, height = 4)
# ggsave(filename = "./figures/discountingBehaviorFERetailerVariationColor.pdf",
#        width = 6, height = 4)

# Regressions of bulk buying within locations
# This adds ZIP-year FEs
zipFip <- unique(panel[, .(household_code, panel_year, fips)])
discBehaviorZIP[, "zipYear" := paste(zip_code, panel_year, sep = "_")]
reg0ZIP <- felm(bulk ~ household_income + men + women + nChildren + age + married |
               panel_year + dma_cd,
             data = discBehaviorZIP,
             weights = discBehaviorZIP$projection_factor)
reg1ZIP <- felm(bulk ~ household_income + men + women + age + nChildren + married |
               zipYear + panel_year + dma_cd,
             data = discBehaviorZIP,
             weights = discBehaviorZIP$projection_factor)
reg2ZIP <- felm(bulk ~ household_income_coarse + men + women + age + nChildren + married |
               panel_year + dma_cd,
             data = discBehaviorZIP,
             weights = discBehaviorZIP$projection_factor)
reg3ZIP <- felm(bulk ~ household_income_coarse + men + women + age + nChildren + married |
               zipYear + panel_year + dma_cd,
             data = discBehaviorZIP,
             weights = discBehaviorZIP$projection_factor)
stargazer(reg0ZIP, reg1ZIP, reg2ZIP, reg3ZIP, type = "text")

coef0 <- as.data.table(summary(reg0ZIP)$coefficients, keep.rownames = TRUE)
confInt0 <- as.data.table(confint(reg0ZIP), keep.rownames = TRUE)
coef0 <- merge(coef0, confInt0, by = "rn")
coef0[, "reg" := "Without FE"]

coef1 <- as.data.table(summary(reg1ZIP)$coefficients, keep.rownames = TRUE)
confInt1 <- as.data.table(confint(reg1ZIP), keep.rownames = TRUE)
coef1 <- merge(coef1, confInt1, by = "rn")
coef1[, "reg" := "With FE"]

finalCoefs <- rbindlist(list(coef0, coef1), use.names = TRUE)
graphDataZIP <- finalCoefs[grepl("household_income", rn)]
graphDataZIP[, "rn" := gsub("household_income", "", rn)]
graphDataZIP[, "rn" := factor(rn, levels = c(4, 6, 8, 10, 11, 13, 15, 16, 17, 18, 19, 21, 23, 26, 27),
                           labels = c(6.5, 9, 11, 13.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 55, 65, 85, 100),
                           ordered = TRUE)]
graphDataZIP[, "rn" := as.numeric(as.character(rn))]
setnames(graphDataZIP, c("rn", "beta", "se", "t", "p", "LCL", "UCL", "Type"))

ggplot(data = graphDataZIP,
       aes(x = rn, y = beta * 100, color = Type)) +
  geom_errorbar(aes(ymin = 100 * LCL,
                    ymax = 100 * UCL), width = 0.2) +
  geom_point(aes(shape = Type), size = 3) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  labs(x = "Household Income ($000s)",
       y = "Difference in Bulk Purchasing (Percentage Points)") +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom") +
  scale_color_grey()
# scale_color_fivethirtyeight()
ggsave(filename = "./figures/discountingBehaviorFEZIP.pdf", height = 4, width = 6)
# ggsave(filename = "./figures/discountingBehaviorFEZIPColor.pdf", height = 4, width = 6)

################################################################################
########## ROBUSTNESS ##########################################################
########## CHECKING WITHIN COUNTY ##############################################
################################################################################
# Regressions of bulk buying within locations
# This adds ZIP-year FEs
discBehaviorZIP[, "fipYear" := paste(fips, panel_year, sep = "_")]
reg0fip <- felm(bulk ~ household_income + men + women + nChildren + age + married |
                  panel_year + dma_cd,
                data = discBehaviorZIP,
                weights = discBehaviorZIP$projection_factor)
reg1fip <- felm(bulk ~ household_income + men + women + age + nChildren + married |
                  fipYear + panel_year + dma_cd,
                data = discBehaviorZIP,
                weights = discBehaviorZIP$projection_factor)
reg2fip <- felm(bulk ~ household_income_coarse + men + women + age + nChildren + married |
                  panel_year + dma_cd,
                data = discBehaviorZIP,
                weights = discBehaviorZIP$projection_factor)
reg3fip <- felm(bulk ~ household_income_coarse + men + women + age + nChildren + married |
                  fipYear + panel_year + dma_cd,
                data = discBehaviorZIP,
                weights = discBehaviorZIP$projection_factor)
stargazer(reg0fip, reg1fip, reg2fip, reg3fip, type = "text")

# Plotting zip vs county FE charts
coef0fip <- as.data.table(summary(reg0fip)$coefficients, keep.rownames = TRUE)
confInt0fip <- as.data.table(confint(reg0fip), keep.rownames = TRUE)
coef0fip <- merge(coef0fip, confInt0fip, by = "rn")
coef0fip[, c("reg") := .("Without FE")]

coef1fip <- as.data.table(summary(reg1fip)$coefficients, keep.rownames = TRUE)
confInt1fip <- as.data.table(confint(reg1fip), keep.rownames = TRUE)
coef1fip <- merge(coef1fip, confInt1fip, by = "rn")
coef1fip[, c("reg") := .("With FE")]


finalCoefsfip <- rbindlist(list(coef0fip, coef1fip), use.names = TRUE)
graphDataFIP <- finalCoefsfip[grepl("household_income", rn)]
graphDataFIP[, "rn" := gsub("household_income", "", rn)]
graphDataFIP[, "rn" := factor(rn, levels = c(4, 6, 8, 10, 11, 13, 15, 16, 17, 18, 19, 21, 23, 26, 27),
                              labels = c(6.5, 9, 11, 13.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 55, 65, 85, 100),
                              ordered = TRUE)]
graphDataFIP[, "rn" := as.numeric(as.character(rn))]
setnames(graphDataFIP, c("rn", "beta", "se", "t", "p", "LCL", "UCL", "Type"))

ggplot(data = graphDataFIP,
       aes(x = rn, y = beta * 100, color = Type)) +
  geom_errorbar(aes(ymin = 100 * LCL,
                    ymax = 100 * UCL), width = 0.2) +
  geom_point(aes(shape = Type), size = 3) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  labs(x = "Household Income ($000s)",
       y = "Difference in Bulk Purchasing (Percentage Points)") +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom") +
  scale_color_grey()
# scale_color_fivethirtyeight()
ggsave(filename = "./figures/AppendixDiscountingBehaviorFEFIP.pdf", height = 4, width = 6)
# ggsave(filename = "./figures/AppendixDiscountingBehaviorFEFIPColor.pdf", height = 4, width = 6)

################################################################################
########## END ROBUSTNESS ######################################################
################################################################################

# Regressions of bulk buying within stores
# This adds store-year FEs
discBehaviorStore[, "storeYear" := paste(store_code_uc, panel_year, sep = "_")]
reg0Store <- felm(bulk ~ household_income + men + women + age + nChildren + married |
               panel_year + dma_cd,
             data = discBehaviorStore,
             weights = discBehaviorStore$projection_factor)
reg1Store <- felm(bulk ~ household_income + men + women + age + nChildren + married |
               storeYear + panel_year + dma_cd,
             data = discBehaviorStore,
             weights = discBehaviorStore$projection_factor)
reg2Store <- felm(bulk ~ household_income_coarse + men + women + age + nChildren + married |
               panel_year + dma_cd,
             data = discBehaviorStore,
             weights = discBehaviorStore$projection_factor)
reg3Store <- felm(bulk ~ household_income_coarse + men + women + age + nChildren + married |
               storeYear + panel_year + dma_cd,
             data = discBehaviorStore,
             weights = discBehaviorStore$projection_factor)

coef0 <- as.data.table(summary(reg0Store)$coefficients, keep.rownames = TRUE)
confInt0 <- as.data.table(confint(reg0Store), keep.rownames = TRUE)
coef0 <- merge(coef0, confInt0, by = "rn")
coef0[, "reg" := "Without FE"]

coef1 <- as.data.table(summary(reg1Store)$coefficients, keep.rownames = TRUE)
confInt1 <- as.data.table(confint(reg1Store), keep.rownames = TRUE)
coef1 <- merge(coef1, confInt1, by = "rn")
coef1[, "reg" := "With FE"]

finalCoefs <- rbindlist(list(coef0, coef1), use.names = TRUE)
graphData <- finalCoefs[grepl("household_income", rn)]
graphData[, "rn" := gsub("household_income", "", rn)]
graphData[, "rn" := factor(rn, levels = c(4, 6, 8, 10, 11, 13, 15, 16, 17, 18, 19, 21, 23, 26, 27),
                           labels = c(6.5, 9, 11, 13.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 55, 65, 85, 100),
                           ordered = TRUE)]
graphData[, "rn" := as.numeric(as.character(rn))]
setnames(graphData, c("rn", "beta", "se", "t", "p", "LCL", "UCL", "Type"))

ggplot(data = graphData,
       aes(x = rn, y = beta * 100, color = Type)) +
  geom_errorbar(aes(ymin = 100 * LCL,
                    ymax = 100 * UCL), width = 0.2) +
  geom_point(aes(shape = Type), size = 3) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  labs(x = "Household Income ($000s)",
       y = "Difference in Bulk Purchasing (Percentage Points)") +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom") +
  scale_color_grey()
# scale_color_fivethirtyeight()
ggsave(filename = "./figures/AppendixDiscountingBehaviorFEStore.pdf", height = 4, width = 6)
# ggsave(filename = "./figures/AppendixDiscountingBehaviorFEStoreColor.pdf", height = 4, width = 6)


# Regressions of bulk buying within retail chains
# This adds retailer-year FEs
discBehaviorRetailer[, "retailerYear" := paste(retailer_code, panel_year, sep = "_")]
discBehaviorRetailer[, "channelYear" := paste(channel_type, panel_year, sep = "_")]

reg0Retailer <- felm(bulk ~ household_income + men + women + age + nChildren + married |
                       panel_year + dma_cd,
                     data = discBehaviorRetailer,
                     weights = discBehaviorRetailer$projection_factor)
reg1Retailer <- felm(bulk ~ household_income + men + women + age + nChildren + married |
                       channelYear + panel_year + dma_cd,
                     data = discBehaviorRetailer,
                     weights = discBehaviorRetailer$projection_factor)
reg2Retailer <- felm(bulk ~ household_income + men + women + age + nChildren + married |
                       channelYear + retailerYear + panel_year + dma_cd,
                     data = discBehaviorRetailer,
                     weights = discBehaviorRetailer$projection_factor)
reg3Retailer <- felm(bulk ~ household_income_coarse + men + women + age + nChildren + married |
               panel_year + dma_cd,
             data = discBehaviorRetailer,
             weights = discBehaviorRetailer$projection_factor)
reg4Retailer <- felm(bulk ~ household_income_coarse + men + women + age + nChildren + married |
               channelYear + panel_year + dma_cd,
             data = discBehaviorRetailer,
             weights = discBehaviorRetailer$projection_factor)
reg5Retailer <- felm(bulk ~ household_income_coarse + men + women + age + nChildren + married |
                       channelYear + retailerYear + panel_year + dma_cd,
                     data = discBehaviorRetailer,
                     weights = discBehaviorRetailer$projection_factor)

coef0 <- as.data.table(summary(reg0Retailer)$coefficients, keep.rownames = TRUE)
confInt0 <- as.data.table(confint(reg0Retailer), keep.rownames = TRUE)
coef0 <- merge(coef0, confInt0, by = "rn")
coef0[, "reg" := "Without FE"]

coef1 <- as.data.table(summary(reg1Retailer)$coefficients, keep.rownames = TRUE)
confInt1 <- as.data.table(confint(reg1Retailer), keep.rownames = TRUE)
coef1 <- merge(coef1, confInt1, by = "rn")
coef1[, "reg" := "Channel FE"]

coef2 <- as.data.table(summary(reg2Retailer)$coefficients, keep.rownames = TRUE)
confInt2 <- as.data.table(confint(reg2Retailer), keep.rownames = TRUE)
coef2 <- merge(coef2, confInt2, by = "rn")
coef2[, "reg" := "Channel + Retailer FE"]

finalCoefs <- rbindlist(list(coef0, coef1, coef2), use.names = TRUE)
graphDataRetailer <- finalCoefs[grepl("household_income", rn)]
graphDataRetailer[, "rn" := gsub("household_income", "", rn)]
graphDataRetailer[, "rn" := factor(rn, levels = c(4, 6, 8, 10, 11, 13, 15, 16, 17, 18, 19, 21, 23, 26, 27),
                           labels = c(6.5, 9, 11, 13.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 55, 65, 85, 100),
                           ordered = TRUE)]
graphDataRetailer[, "rn" := as.numeric(as.character(rn))]
setnames(graphDataRetailer, c("rn", "beta", "se", "t", "p", "LCL", "UCL", "Type"))

# Retailer and Channel FE graph
group.colors <- c("Without FE" = "#FF2700", "Channel FE" = "#008FD5",
                  "Channel + Retailer FE" = "#77AB43")
group.shapes <- c("Without FE" = 17, "Channel FE" = 16,
                  "Channel + Retailer FE" = 15)
ggplot(data = graphDataRetailer,
       aes(x = rn, y = beta * 100, color = Type)) +
  geom_errorbar(aes(ymin = 100 * LCL,
                    ymax = 100 * UCL), width = 0.2) +
  geom_point(aes(shape = Type), size = 3) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  labs(x = "Household Income ($000s)",
       y = "Difference in Bulk Purchasing (Percentage Points)") +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom") +
  # scale_color_grey() +
  scale_color_manual(values = group.colors) +
  scale_shape_manual(values = group.shapes)
# ggsave(filename = "./figures/discountingBehaviorFE.pdf", height = 4, width = 6)
ggsave(filename = "./figures/discountingBehaviorFEColor.pdf", height = 4, width = 6)

# Getting stargazer table for income quartiles
stargazer(reg2ZIP, reg3ZIP, reg3Retailer, reg4Retailer, reg5Retailer,
          type = "text",
          add.lines = list(c("Demographics", "Y", "Y", "Y", "Y", "Y"),
                           c("ZIP-Year FE", "N", "Y", "N", "N", "N"),
                           c("Channel-Year FE", "N", "N", "N", "Y", "Y"),
                           c("Chain-Year FE", "N", "N", "N", "N", "Y")),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("ZIP Code", "Channel/Retailer Type"),
          column.separate = c(2, 3),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          keep = c("household_income*"),
          order = c(2, 3, 1),
          covariate.labels = c("25-50k", "50-100k", ">100k"),
          notes.align = "l",
          notes.append = TRUE,
          digits = 3,
          notes = c("Source: Using 2004--2017 Nielsen Consumer Panel data, this ",
                    "table displays the regression coefficients from estimating ",
                    "Equation \ref{eq:discountingBehaviorFE} which regresses a ",
                    "household's annual share of bulk purchases of non-food products ",
                    "on household characteristics (household composition, age, ",
                    "marital status, and education) and includes a DMA and year ",
                    "fixed effect (Columns 1, 3, 5). Columns 2, 4, and 6 also ",
                    "include a ZIP code-year, store type-year, or retail chain-year ",
                    "fixed effect."),
          label = "tab:discountingBehaviorFE",
          title = "Bulk Buying Within ZIP Codes, Store Types, or Retail Chains by Income",
          out = "tables/AppendixDiscountingBehaviorFE.tex")

# Full stargazer table for graphs
stargazer(reg0ZIP, reg1ZIP, reg0Retailer, reg1Retailer, reg2Retailer,
          type = "text",
          add.lines = list(c("Demographics", "Y", "Y", "Y", "Y", "Y"),
                           c("ZIP-Year FE", "N", "Y", "N", "N", "N"),
                           c("Channel-Year FE", "N", "N", "N", "Y", "Y"),
                           c("Chain-Year FE", "N", "N", "N", "N", "Y")),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("ZIP Code", "Channel/Retailer Type"),
          column.separate = c(2, 3),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          keep = c("household_income*"),
          covariate.labels = c("8-10k", "10-12k", "12-15k", "15-20k", "20-25k",
                               "25-30k", "30-35k", "35-40k", "40-45k", "45-50k",
                               "50-60k", "60-70k", "70-100k", ">100k"),
          notes.align = "l",
          notes.append = TRUE,
          digits = 3,
          notes = c("Source: Using 2004--2017 Nielsen Consumer Panel data, this ",
                    "table displays the regression coefficients from estimating ",
                    "Equation \ref{eq:discountingBehaviorFE} which regresses a ",
                    "household's annual share of bulk purchases of non-food products ",
                    "on household characteristics (household composition, age, ",
                    "marital status, and education) and includes a DMA and year ",
                    "fixed effect (Columns 1, 3, 5). Columns 2, 4, and 6 also ",
                    "include a ZIP code-year, store type-year, or retail chain-year ",
                    "fixed effect."),
          label = "tab:discountingBehaviorFEAll",
          title = "Bulk Buying Within ZIP Codes, Store Types, or Retail Chains by Income",
          out = "tables/AppendixDiscountingBehaviorFEAll.tex")
