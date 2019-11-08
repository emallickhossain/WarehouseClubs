# Gets spending shares by channel and storability
library(data.table)
library(ggthemes)
library(ggplot2)
library(lfe)
library(stringr)
library(stargazer)
library(binsreg)
yrs <- 2004:2017
threads <- 8

# Getting trips and adding in channel type
trips <- fread("/scratch/upenn/hossaine/fullTrips.csv", nThread = threads,
               select = c("retailer_code", "trip_code_uc", "household_code", "panel_year"))
retailers <- fread("/scratch/upenn/hossaine/fullRetailers.csv")
trips <- merge(trips, retailers, by = "retailer_code")
rm(retailers)

# Getting household characteristics
panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("panel_year", "household_code", "projection_factor",
                          "household_income", "men", "women", "nChildren", "age",
                          "college", "dma_cd", "married", "household_income_coarse"),
               key = c("household_code", "panel_year"))

# Getting purchases and merging with trips
fullPurch <- NULL
for (yr in 2004:2017) {
  print(yr)
  purch <- fread(paste0("/scratch/upenn/hossaine/fullPurch", yr, ".csv"),
                 nThread = threads,
                 select = c("trip_code_uc", "packagePrice", "quantity",
                            "product_module_code", "food"),
                 key = "trip_code_uc")[food == 0]
  purch[, "spend" := quantity * packagePrice]
  purch[, c("quantity", "packagePrice") := NULL]
  fullPurch <- rbindlist(list(fullPurch, purch), use.names = TRUE)
}

fullPurch <- merge(fullPurch, trips, by = "trip_code_uc")
annualSpend <- fullPurch[, .(spend = sum(spend)),
                         keyby = .(household_code, panel_year, channel_type)]
annualSpend[, "totalExp" := sum(spend), by = .(household_code, panel_year)]
fwrite(annualSpend, file = "/scratch/upenn/hossaine/expendituresByChannel.csv", nThread = threads)

############## COMPUTING CHANNEL SHARES OF TOTAL EXPENDITURES ##################
# Adding in zeros
annualSpend <- fread("/scratch/upenn/hossaine/expendituresByChannel.csv", nThread = threads)
zeros <- unique(annualSpend[, .(id = paste(household_code, panel_year, sep = "_"))])
zeros <- as.data.table(expand.grid(id = zeros$id,
                                   channel_type = unique(annualSpend$channel_type)))
zeros[, c("household_code", "panel_year") := tstrsplit(id, "_")]
zeros[, c("household_code", "panel_year", "id") :=
        .(as.integer(household_code), as.integer(panel_year), NULL)]
annualSpend <- merge(zeros, annualSpend, by = c("household_code", "panel_year",
                                                "channel_type"), all = TRUE)
annualSpend[is.na(spend), "spend" := 0]
annualSpend[, "totalExp" := mean(totalExp, na.rm = TRUE), by = .(household_code, panel_year)]

# Adding in household characteristics
annualSpend <- merge(annualSpend, panel, by = c("household_code", "panel_year"))
annualSpend[, "share" := spend / totalExp]

# Graph of identifying variation
ex <- annualSpend[men == 1 & women == 1 & nChildren == 2 & married == 1 &
                    college == 1 & panel_year == 2017]
ex[, "household_income" := factor(household_income,
                                  levels = c(4, 6, 8, 10, 11, 13, 15, 16, 17,
                                             18, 19, 21, 23, 26, 27),
                                  labels = c(6.5, 9, 11, 13.5, 17.5, 22.5, 27.5,
                                             32.5, 37.5, 42.5, 47.5, 55, 65, 85, 100),
                                  ordered = TRUE)]
ex[, "household_income" := as.numeric(as.character(household_income))]
ggplot(data = ex, aes(x = household_income, y = share, color = channel_type)) +
  geom_point(size = 1) +
  geom_jitter(width = 2) +
  geom_smooth(method = "lm") +
  facet_wrap(vars(channel_type)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  labs(x = "Annual Household Income ($000s)",
       y = "Annual Spending Share",
       color = "Channel Type") +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom") +
  scale_color_grey()
  # scale_color_colorblind()

ggsave("./figures/expendituresByChannelVariation.pdf", height = 4, width = 6)
# ggsave("./figures/expendituresByChannelVariationColor.pdf", height = 4, width = 6)


# Running regression
annualSpend[, "household_income" := as.factor(household_income)]
annualSpend[channel_type %in% c("Grocery", "Discount Store", "Drug Store"),
            "channel_type" := "Grocery/Discount/Drug Store"]
annualSpend <- annualSpend[, .(share = sum(share)),
                           by = .(household_code, panel_year, channel_type,
                                  projection_factor, household_income, men,
                                  women, nChildren, age, college, dma_cd, married,
                                  household_income_coarse)]
reg1 <- felm(share ~ household_income + men + women + nChildren + age + college + married |
               dma_cd + panel_year,
             data = annualSpend[channel_type == "Grocery/Discount/Drug Store"],
             weights = annualSpend[channel_type == "Grocery/Discount/Drug Store"]$projection_factor)
reg2 <- felm(share ~ household_income + men + women + nChildren + age + college + married |
               dma_cd + panel_year,
             data = annualSpend[channel_type == "Dollar Store"],
             weights = annualSpend[channel_type == "Dollar Store"]$projection_factor)
reg3 <- felm(share ~ household_income + men + women + nChildren + age + college + married |
               dma_cd + panel_year,
             data = annualSpend[channel_type == "Warehouse Club"],
             weights = annualSpend[channel_type == "Warehouse Club"]$projection_factor)

# Getting graph data
coef1 <- as.data.table(summary(reg1)$coefficients, keep.rownames = TRUE)
confInt1 <- as.data.table(confint(reg1), keep.rownames = TRUE)
coef1 <- merge(coef1, confInt1, by = "rn")
coef1[, "reg" := "Grocery/Discount/Drug Store"]

coef2 <- as.data.table(summary(reg2)$coefficients, keep.rownames = TRUE)
confInt2 <- as.data.table(confint(reg2), keep.rownames = TRUE)
coef2 <- merge(coef2, confInt2, by = "rn")
coef2[, "reg" := "Dollar Store"]

coef3 <- as.data.table(summary(reg3)$coefficients, keep.rownames = TRUE)
confInt3 <- as.data.table(confint(reg3), keep.rownames = TRUE)
coef3 <- merge(coef3, confInt3, by = "rn")
coef3[, "reg" := "Warehouse Club"]

finalCoefs <- rbindlist(list(coef1, coef2, coef3), use.names = TRUE)
graphData <- finalCoefs[grepl("household_income", rn)]
graphData[, "rn" := gsub("household_income", "", rn)]
graphData[, "rn" := factor(rn, levels = c(4, 6, 8, 10, 11, 13, 15, 16, 17, 18, 19, 21, 23, 26, 27),
                                  labels = c(6.5, 9, 11, 13.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 55, 65, 85, 100),
                                  ordered = TRUE)]
graphData[, "rn" := as.numeric(as.character(rn))]
setnames(graphData, c("rn", "beta", "se", "t", "p", "LCL", "UCL", "Type"))

fwrite(graphData, "./figures/expendituresByChannelColor.csv")
graphData <- fread("./figures/expendituresByChannelColor.csv")

ggplot(data = graphData,
       aes(x = rn, y = beta * 100, color = Type)) +
  geom_errorbar(aes(ymin = 100 * LCL,
                    ymax = 100 * UCL), width = 0.2) +
  geom_point(aes(shape = Type), size = 3) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  labs(x = "Annual Household Income ($000s)",
       y = "Diff. in Annual Budget Share\n(Percentage Points)") +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom",
        text = element_text(size = 14),
        axis.ticks.length = unit(0.25, "cm")) +
  scale_color_fivethirtyeight()
ggsave("./figures/expendituresByChannelColor.pdf", height = 4, width = 6)

# Stargazer table for income quartiles
reg1 <- felm(share ~ household_income_coarse + men + women + nChildren + age + college + married |
               dma_cd + panel_year,
             data = annualSpend[channel_type == "Grocery/Discount/Drug Store"],
             weights = annualSpend[channel_type == "Grocery/Discount/Drug Store"]$projection_factor)
reg2 <- felm(share ~ household_income_coarse + men + women + nChildren + age + college + married |
               dma_cd + panel_year,
             data = annualSpend[channel_type == "Dollar Store"],
             weights = annualSpend[channel_type == "Dollar Store"]$projection_factor)
reg3 <- felm(share ~ household_income_coarse + men + women + nChildren + age + college + married |
               dma_cd + panel_year,
             data = annualSpend[channel_type == "Warehouse Club"],
             weights = annualSpend[channel_type == "Warehouse Club"]$projection_factor)
stargazer(reg1, reg2, reg3, type = "text")
