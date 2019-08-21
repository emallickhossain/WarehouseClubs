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
                          "purchase_date", "retailer_code"))
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
discBehaviorRetailer <- NULL
discBehaviorChannel <- NULL
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
  householdAvgRetailer <- purch[, .(bulk = weighted.mean(bulk, w = realExp)),
                                by = .(household_code, panel_year, retailer_code)]
  householdAvgChannel <- purch[, .(bulk = weighted.mean(bulk, w = realExp)),
                               by = .(household_code, panel_year, channel_type)]

  # Combining
  discBehaviorZIP <- rbindlist(list(discBehaviorZIP, householdAvgZIP),
                               use.names = TRUE)
  discBehaviorRetailer <- rbindlist(list(discBehaviorRetailer, householdAvgRetailer),
                                    use.names = TRUE)
  discBehaviorChannel <- rbindlist(list(discBehaviorChannel, householdAvgChannel),
                                   use.names = TRUE)
}

# Saving
fwrite(discBehaviorZIP, "/scratch/upenn/hossaine/discBehaviorZIP.csv", nThread = threads)
fwrite(discBehaviorRetailer, "/scratch/upenn/hossaine/discBehaviorRetailer.csv", nThread = threads)
fwrite(discBehaviorChannel, "/scratch/upenn/hossaine/discBehaviorChannel.csv", nThread = threads)

# Adding demographics
discBehaviorZIP <- fread("/scratch/upenn/hossaine/discBehaviorZIP.csv", nThread = threads)
discBehaviorRetailer <- fread("/scratch/upenn/hossaine/discBehaviorRetailer.csv", nThread = threads)
discBehaviorChannel <- fread("/scratch/upenn/hossaine/discBehaviorChannel.csv", nThread = threads)

panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("panel_year", "household_code", "projection_factor",
                          "household_income", "household_size", "age", "child",
                          "dma_cd", "household_income_coarse", "married",
                          "carShare", "law", "zip_code"),
               key = c("household_code", "panel_year"))
panel[, "household_income" := as.factor(household_income)]
panel[, "household_income_coarse" := as.factor(household_income_coarse)]
panel[, "lawInd" := (law >= 3)]

discBehaviorZIP <- merge(discBehaviorZIP, panel, by = c("household_code", "panel_year"))
discBehaviorRetailer <- merge(discBehaviorRetailer, panel, by = c("household_code", "panel_year"))
discBehaviorChannel <- merge(discBehaviorChannel, panel, by = c("household_code", "panel_year"))

# Regressions of bulk buying within locations
# This adds ZIP-year FEs
discBehaviorZIP[, "zipYear" := paste(zip_code, panel_year, sep = "_")]
discBehaviorZIP[, "dmaYear" := paste(dma_cd, panel_year, sep = "_")]
reg0 <- felm(bulk ~ household_income + household_size + age + child + married |
               dmaYear,
             data = discBehaviorZIP,
             weights = discBehaviorZIP$projection_factor)
reg1 <- felm(bulk ~ household_income + household_size + age + child + married |
               zipYear,
             data = discBehaviorZIP,
             weights = discBehaviorZIP$projection_factor)
reg2 <- felm(bulk ~ household_income + household_size + age + child + married |
               zipYear + dmaYear,
             data = discBehaviorZIP,
             weights = discBehaviorZIP$projection_factor)

coef0 <- as.data.table(summary(reg0)$coefficients, keep.rownames = TRUE)
confInt0 <- as.data.table(confint(reg0), keep.rownames = TRUE)
coef0 <- merge(coef0, confInt0, by = "rn")
coef0[, "reg" := "Without Zip-Year FE"]

coef1 <- as.data.table(summary(reg1)$coefficients, keep.rownames = TRUE)
confInt1 <- as.data.table(confint(reg1), keep.rownames = TRUE)
coef1 <- merge(coef1, confInt1, by = "rn")
coef1[, "reg" := "With Zip-Year FE"]

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
ggsave(filename = "./figures/discountingBehaviorZipYearFE.png",
       height = 4, width = 6)

# Regressions of bulk buying within retail chains
# This adds retailer-year FEs
discBehaviorRetailer[, "retailerYear" := paste(retailer_code, panel_year, sep = "_")]
discBehaviorRetailer[, "dmaYear" := paste(dma_cd, panel_year, sep = "_")]
reg0 <- felm(bulk ~ household_income + household_size + age + child + married |
               dmaYear,
             data = discBehaviorRetailer,
             weights = discBehaviorRetailer$projection_factor)
reg1 <- felm(bulk ~ household_income + household_size + age + child + married |
               retailerYear,
             data = discBehaviorRetailer,
             weights = discBehaviorRetailer$projection_factor)
reg2 <- felm(bulk ~ household_income + household_size + age + child + married |
               retailerYear + dmaYear,
             data = discBehaviorRetailer,
             weights = discBehaviorRetailer$projection_factor)

coef0 <- as.data.table(summary(reg0)$coefficients, keep.rownames = TRUE)
confInt0 <- as.data.table(confint(reg0), keep.rownames = TRUE)
coef0 <- merge(coef0, confInt0, by = "rn")
coef0[, "reg" := "Without Retailer-Year FE"]

coef1 <- as.data.table(summary(reg1)$coefficients, keep.rownames = TRUE)
confInt1 <- as.data.table(confint(reg1), keep.rownames = TRUE)
coef1 <- merge(coef1, confInt1, by = "rn")
coef1[, "reg" := "With Retailer-Year FE"]

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
ggsave(filename = "./figures/discountingBehaviorRetailerYearFE.png",
       height = 4, width = 6)

# Regressions of bulk buying within channels
# This adds channel-year FEs
discBehaviorChannel[, "channelYear" := paste(channel_type, panel_year, sep = "_")]
discBehaviorChannel[, "dmaYear" := paste(dma_cd, panel_year, sep = "_")]
storeTypes <- c("Discount Store", "Grocery", "Dollar Store", "Warehouse Club", "Drug Store")
reg0 <- felm(bulk ~ household_income + household_size + age + child + married |
               dmaYear,
             data = discBehaviorChannel,
             weights = discBehaviorChannel$projection_factor)
reg1 <- felm(bulk ~ household_income + household_size + age + child + married |
               channelYear,
             data = discBehaviorChannel,
             weights = discBehaviorChannel$projection_factor)
reg2 <- felm(bulk ~ household_income + household_size + age + child + married |
               channelYear + dmaYear,
             data = discBehaviorChannel,
             weights = discBehaviorChannel$projection_factor)

coef0 <- as.data.table(summary(reg0)$coefficients, keep.rownames = TRUE)
confInt0 <- as.data.table(confint(reg0), keep.rownames = TRUE)
coef0 <- merge(coef0, confInt0, by = "rn")
coef0[, "reg" := "Without Store Type-Year FE"]

coef1 <- as.data.table(summary(reg1)$coefficients, keep.rownames = TRUE)
confInt1 <- as.data.table(confint(reg1), keep.rownames = TRUE)
coef1 <- merge(coef1, confInt1, by = "rn")
coef1[, "reg" := "With Store Type-Year FE"]

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
ggsave(filename = "./figures/discountingBehaviorChannelYearFE.png",
       height = 4, width = 6)
