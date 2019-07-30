# Computes household propensity for different discounting behaviors within store types
library(data.table)
library(ggplot2)
library(ggthemes)
library(lfe)
library(purrr)
threads <- 8

# Getting trips and panel data for demographics and to deflate expenditures
trips <- fread("/scratch/upenn/hossaine/fullTrips.csv", nThread = threads,
               select = c("trip_code_uc", "household_code", "panel_year", "retailer_code"))
retailers <- fread("/scratch/upenn/hossaine/fullRetailers.csv")
trips <- merge(trips, retailers, by = "retailer_code")
prod <- unique(fread("/scratch/upenn/hossaine/nielsen_extracts/HMS/Master_Files/prod.tsv",
                     select = c("product_module_code", "product_group_code", "product_group_descr")))

panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("panel_year", "household_code", "projection_factor",
                          "household_income", "household_size", "age", "child",
                          "married", "dma_cd", "household_income_coarse"),
               key = c("household_code", "panel_year"))
panel[, "household_income" := as.factor(household_income)]
panel[, "household_income_coarse" := as.factor(household_income_coarse)]

# Getting all purchases and coding them by discounting behavior
fullPurch <- NULL
for (yr in 2004:2017) {
  print(yr)
  purch <- fread(paste0("/scratch/upenn/hossaine/fullPurch", yr, ".csv"),
                 nThread = threads,
                 select = c("trip_code_uc", "quantity", "product_module_code",
                            "food", "totalAmount"),
                 key = "trip_code_uc")
  purch <- merge(purch, trips, by = "trip_code_uc")
  purch[, "totalQ" := totalAmount * quantity]
  purch <- purch[, .(totalQ = sum(totalQ),
                     totalPackages = sum(quantity)),
                 by = .(household_code, panel_year, product_module_code,
                        food, channel_type)]

  # Combining
  fullPurch <- rbindlist(list(fullPurch, purch), use.names = TRUE)
}

###################### PACKAGE SIZE BY INCOME OVERALL ##########################
allPurchases <- fullPurch[, .(totalQ = sum(totalQ),
                              totalPackages = sum(totalPackages)),
                          by = .(household_code, panel_year, product_module_code, food)]
allPurchases[, "logAvgSize" := log(totalQ / totalPackages)]
allPurchases <- merge(allPurchases, panel, by = c("household_code", "panel_year"))
allPurchases[, "marketYear" := paste0(dma_cd, "_", panel_year)]

# Getting modules purchased by more than 5000 households each year
topMods <- allPurchases[, uniqueN(household_code), by = .(product_module_code, panel_year)]
topMods <- unique(topMods[V1 > 5000]$product_module_code)
topMods <- data.table(product_module_code = topMods, topMod = 1)

getReg <- function(mod) {
  print(mod)
  regData <- allPurchases[product_module_code == mod]
  tryCatch({
    reg <- felm(data = regData,
                logAvgSize ~ household_income_coarse + household_size + age +
                  child + married | marketYear,
                weights = regData$projection_factor)
    coefs <- as.data.table(summary(reg)$coefficients, keep.rownames = TRUE)
    coefs[, "product_module_code" := mod]
  return(coefs)
  }, error = function(e){})
}

mods <- sort(unique(fullPurch$product_module_code))
fullCoefs <- rbindlist(map(mods, getReg), use.names = TRUE)
fullCoefs[, "rn" := gsub("household_income_coarse", "", rn)]
fullCoefs <- merge(fullCoefs, unique(allPurchases[, .(food, product_module_code)]),
                   by = "product_module_code")
setnames(fullCoefs, c("mod", "rn", "beta", "se", "t", "p", "food"))

# Making histogram
histData <- na.omit(fullCoefs[rn %in% c("25-50k", "50-100k", ">100k")])
histData[, "income" := factor(rn, levels = c("25-50k", "50-100k", ">100k"), ordered = TRUE)]
histData[p > 0.05, "beta" := 0]
histData <- merge(histData, topMods, by.y = "product_module_code", by.x = "mod", all.x = TRUE)
fwrite(histData, "/scratch/upenn/hossaine/discountingBehaviorChannel1.csv")

# Download
histData <- fread("/home/mallick/Downloads/discountingBehaviorChannel1.csv")

# Add product groups
groupData <- setDT(readxl::read_excel("/home/mallick/Desktop/Nielsen/Data/Panel/Product_Hierarchy_01.31.2019.xlsx"))
groupData <- groupData[, .(product_module_code, product_module_descr,
                           product_group_code, product_group_descr)]
histData <- merge(histData, groupData, by.y = "product_module_code", by.x = "mod")
histData[, "income" := factor(rn, levels = c("25-50k", "50-100k", ">100k"), ordered = TRUE)]

# Plotting coefficients
ggplot(data = histData[!mod %in% c(8602, 1483) & topMod == 1],
       aes(fill = income, y = beta, x = reorder(as.factor(mod), -beta), by = food)) +
  geom_bar(position = "identity", stat = "identity", alpha = 0.6) +
  #geom_errorbar(aes(ymin = beta - 1.96 * se, ymax = beta + 1.96 * se),
  #             width = 0.2) +
  #facet_wrap(vars(product_group_descr), scales = "free") +
  #facet_wrap(vars(food), scales = "free_x") +
  labs(title = "Rich Households Buy Larger Packages",
       x = "Product Category", y = "Log-Point Increase Over Poorest Households",
       fill = "Household Income",
       caption = paste0("Note: Figure plots difference in average package size purchased relative to \n",
                        "households making under $25k. Figure controls for household size, age, children, \n",
                        "and marital status. Only coefficients significant at the 5% level are plotted. \n",
                        "Standard errors are suppressed for clarity. Only product categories for which at \n",
                        "least 5000 households in a year made a purchase are plotted. Bars are not stacked \n",
                        "and represent an individual product category")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0),
        axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  scale_fill_grey()
ggsave("./code/5_figures/discountingBehaviorChannelAll.png", width = 8, height = 6)

# Generating table of how many categories do rich buy more, same, or less
histData[p < 0.05, "more" := ifelse(beta > 0, 1L, 0L)]
histData[is.na(more), "more" := 0L]
histData[, "same" := ifelse(p > 0.05, 1L, 0L)]
histData[p < 0.05, "less" := ifelse(beta < 0, 1L, 0L)]
histData[is.na(less), "less" := 0L]
foodN <- uniqueN(histData[food == 1]$mod)
nonFoodN <- uniqueN(histData[food == 0]$mod)
table1Food <- histData[food == 1, .(Bigger = round(sum(more) / foodN, 2),
                                    Same = round(sum(same) / foodN, 2),
                                    Smaller = round(sum(less) / foodN, 2)),
                       keyby = .(income)]
table1NonFood <- histData[food == 0, .(Bigger = round(sum(more) / nonFoodN, 2),
                                       Same = round(sum(same) / nonFoodN, 2),
                                       Smaller = round(sum(less) / nonFoodN, 2)),
                          keyby = .(income)]

# Redoing table, but now bigger or smaller has to be more than 10%
histData[p < 0.05, "more" := ifelse(beta > 0.1, 1L, 0L)]
histData[is.na(more), "more" := 0L]
histData[, "same" := ifelse(p > 0.05, 1L, 0L)]
histData[beta <= 0.1 & beta >= -0.1, "same" := 1L]
histData[p < 0.05, "less" := ifelse(beta < -0.1, 1L, 0L)]
histData[is.na(less), "less" := 0L]
table2Food <- histData[food == 1, .(Bigger = round(sum(more) / foodN, 2),
                                    Same = round(sum(same) / foodN, 2),
                                    Smaller = round(sum(less) / foodN, 2)),
                       keyby = .(income)]
table2NonFood <- histData[food == 0, .(Bigger = round(sum(more) / nonFoodN, 2),
                                       Same = round(sum(same) / nonFoodN, 2),
                                       Smaller = round(sum(less) / nonFoodN, 2)),
                          keyby = .(income)]

# Redoing table, but now bigger or smaller has to be more than 20%
histData[p < 0.05, "more" := ifelse(beta > 0.2, 1L, 0L)]
histData[is.na(more), "more" := 0L]
histData[, "same" := ifelse(p > 0.05, 1L, 0L)]
histData[beta <= 0.2 & beta >= -0.2, "same" := 1L]
histData[p < 0.05, "less" := ifelse(beta < -0.2, 1L, 0L)]
histData[is.na(less), "less" := 0L]
table3Food <- histData[food == 1, .(Bigger = round(sum(more) / foodN, 2),
                                    Same = round(sum(same) / foodN, 2),
                                    Smaller = round(sum(less) / foodN, 2)),
                       keyby = .(income)]
table3NonFood <- histData[food == 0, .(Bigger = round(sum(more) / nonFoodN, 2),
                                       Same = round(sum(same) / nonFoodN, 2),
                                       Smaller = round(sum(less) / nonFoodN, 2)),
                          keyby = .(income)]

# Plotting only "necessities"
graphNecessities <- graphData[product_group_code %in% c(4501, 4502, 4507, 6015)]
ggplot(data = graphNecessities,
       aes(fill = income, y = beta, x = reorder(as.factor(product_module_descr), -beta))) +
  geom_bar(position = "identity", stat = "identity", alpha = 0.6) +
  geom_errorbar(aes(ymin = beta - 1.96 * se, ymax = beta + 1.96 * se),
               width = 0.2) +
  facet_wrap(vars(product_group_descr), scales = "free") +
  labs(title = "Rich Households Buy Larger Packages",
       x = "Product Category", y = "Log-Point Increase Over Poorest Households",
       fill = "Household Income",
       caption = paste0("Note: Figure plots difference in average package size purchased relative to \n",
                        "households making under $25k. Figure controls for household size, age, children, \n",
                        "and marital status. Only coefficients significant at the 5% level are plotted. \n",
                        "Standard errors are suppressed for clarity. Only product categories for which at \n",
                        "least 5000 households in a year made a purchase are plotted. Each bar represents \n",
                        "an individual product category.")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0),
        axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  scale_fill_grey()
ggsave("./code/5_figures/discountingBehaviorChannelNecessities.png", width = 8, height = 6)


###################### PACKAGE SIZE BY CHANNEL AND INCOME ######################
channelPurchases <- fullPurch[, .(totalQ = sum(totalQ),
                                  totalPackages = sum(totalPackages)),
                              by = .(household_code, panel_year,
                                     product_module_code, food, channel_type)]
channelPurchases[, "logAvgSize" := log(totalQ / totalPackages)]
channelPurchases <- merge(channelPurchases, panel, by = c("household_code", "panel_year"))

getReg <- function(mod, chan) {
  print(c(mod, chan))
  regData <- channelPurchases[product_module_code == mod & channel_type == chan]
  tryCatch({
    reg <- felm(data = regData,
                logAvgSize ~ household_income_coarse + household_size + age |
                  dma_cd + panel_year,
                weights = regData$projection_factor)
    coefs <- as.data.table(summary(reg)$coefficients, keep.rownames = TRUE)
    coefs[, c("product_module_code", "channel") := .(mod, chan)]
    return(coefs)
  }, error = function(e){})
}

mods <- sort(unique(fullPurch$product_module_code))
channels <- c("Grocery", "Discount Store", "Warehouse Club", "Dollar Store")
toRun <- expand.grid(mods = mods, channels = channels)
fullCoefs <- rbindlist(map2(toRun$mods, toRun$channels, getReg), use.names = TRUE)
fullCoefs[, "rn" := gsub("household_income_coarse", "", rn)]
fullCoefs <- merge(fullCoefs, unique(channelPurchases[, .(storable, product_module_code)]),
                   by = "product_module_code")
setnames(fullCoefs, c("mod", "rn", "beta", "se", "t", "p", "channel", "storable"))

# Making histogram
histData <- fullCoefs[rn %in% c("25-50k", "50-100k", ">100k")]
histData[, "income" := factor(rn, levels = c("25-50k", "50-100k", ">100k"), ordered = TRUE)]
histData[p > 0.05, "beta" := 0]
histData <- merge(histData, topMods, by.y = "product_module_code", by.x = "mod", all.x = TRUE)
fwrite(histData, "/scratch/upenn/hossaine/discountingBehaviorChannel1.csv")

histData <- fullCoefs[rn %in% c("25-50k", "50-100k", ">100k")]
histData[p < 0.05, "beta" := 0]
ggplot(data = histData, aes(x = beta)) +
  geom_density() +
  facet_grid(rows = vars(rn), cols = vars(channel)) +
  theme_fivethirtyeight()
