# Computes bulk buying gap by package size for each non-food product
# Does similar analysis for the max size purchased by each household
# find /scratch/upenn/hossaine/nielsen_extracts/HMS/ -exec touch {} \;
library(data.table)
library(ggplot2)
library(ggthemes)
library(lfe)
library(purrr)
library(ggrepel)
library(stargazer)
threads <- 8

# Getting trips and panel data for demographics and to deflate expenditures
trips <- fread("/scratch/upenn/hossaine/fullTrips.csv", nThread = threads,
               select = c("trip_code_uc", "household_code", "panel_year", "retailer_code"))
retailers <- fread("/scratch/upenn/hossaine/fullRetailers.csv")
trips <- merge(trips, retailers, by = "retailer_code")
prod <- unique(fread("/scratch/upenn/hossaine/nielsen_extracts/HMS/Master_Files/Latest/products.tsv",
                     select = c("product_module_code", "product_group_code",
                                "product_group_descr"), quote = ""))

panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("panel_year", "household_code", "projection_factor",
                          "household_income", "men", "women", "age", "nChildren",
                          "married", "dma_cd", "household_income_coarse", "college"),
               key = c("household_code", "panel_year"))
panel[, "household_income" := as.factor(household_income)]
panel[, "household_income_coarse" := as.factor(household_income_coarse)]

# Getting bulk discount magnitude to control for it
getCoefs <- function(i) {
  data <- fread(paste0("/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas", i, ".csv"))
  return(data)
}
coefs <- rbindlist(map(c("1a", "1b", "1c1501",
                         #"1c1503", "1c1505",
                         "1c1506", "1c1507", "1c1508", "2", "3", "4", "5", "6", "7", "7b"),
                       getCoefs), use.names = TRUE)
coefs[abs(`t value`) <= 3, "Estimate" := 0]
coefs <- unique(coefs[reg == "Store-Week-Brand FE",
                      .(discount = Estimate, product_module_code = mod)])

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
  purch <- purch[, .(pkgSize = weighted.mean(totalAmount, w = totalQ),
                     maxSize = max(totalQ)),
                 by = .(household_code, panel_year, product_module_code, food)]

  # Combining
  fullPurch <- rbindlist(list(fullPurch, purch), use.names = TRUE)
}

###################### PACKAGE SIZE BY INCOME OVERALL ##########################
fullPurch[, "lPkgSize" := log(pkgSize)]
fullPurch <- merge(fullPurch, coefs, by = "product_module_code")
allPurchases <- merge(fullPurch, panel, by = c("household_code", "panel_year"))

getReg <- function(mod) {
  print(mod)
  regData <- allPurchases[product_module_code == mod]
  tryCatch({
    reg <- felm(data = regData,
                lPkgSize ~ discount + household_income_coarse + men + women + age +
                  nChildren + married + college | dma_cd + panel_year,
                weights = regData$projection_factor)
    coefs <- as.data.table(summary(reg)$coefficients, keep.rownames = TRUE)
    confInt1 <- as.data.table(confint(reg), keep.rownames = TRUE)
    coefs <- merge(coefs, confInt1, by = "rn")
    coefs[, "product_module_code" := mod]
  return(coefs)
  }, error = function(e){})
}

mods <- sort(unique(fullPurch$product_module_code))
fullCoefs <- rbindlist(map(mods, getReg), use.names = TRUE)
fullCoefs[, "rn" := gsub("household_income_coarse", "", rn)]
fullCoefs <- merge(fullCoefs, unique(allPurchases[, .(food, product_module_code)]),
                   by = "product_module_code")
setnames(fullCoefs, c("mod", "rn", "beta", "se", "t", "p", "LCL", "UCL", "food"))

# Making histogram
histData <- na.omit(fullCoefs[rn %in% c("25-50k", "50-100k", ">100k")])
histData[, "income" := factor(rn, levels = c("25-50k", "50-100k", ">100k"), ordered = TRUE)]
histData[p > 0.05, "beta" := 0]
# fwrite(histData, "/scratch/upenn/hossaine/discountingBehaviorChannel1.csv")
fwrite(histData, "/scratch/upenn/hossaine/discountingBehaviorChannel1AddDiscount.csv")

###################### BIGGEST PACKAGE PURCHASED BY INCOME OVERALL #############
allPurchases[, "lMaxSize" := log(maxSize)]

getReg <- function(mod) {
  print(mod)
  regData <- allPurchases[product_module_code == mod]
  tryCatch({
    reg <- felm(data = regData,
                lMaxSize ~ household_income_coarse + men + women + age +
                  nChildren + married + college | dma_cd + panel_year,
                weights = regData$projection_factor)
    coefs <- as.data.table(summary(reg)$coefficients, keep.rownames = TRUE)
    confInt1 <- as.data.table(confint(reg), keep.rownames = TRUE)
    coefs <- merge(coefs, confInt1, by = "rn")
    coefs[, "product_module_code" := mod]
    return(coefs)
  }, error = function(e){})
}

mods <- sort(unique(fullPurch$product_module_code))
fullCoefs <- rbindlist(map(mods, getReg), use.names = TRUE)
fullCoefs[, "rn" := gsub("household_income_coarse", "", rn)]
fullCoefs <- merge(fullCoefs, unique(allPurchases[, .(food, product_module_code)]),
                   by = "product_module_code")
setnames(fullCoefs, c("mod", "rn", "beta", "se", "t", "p", "LCL", "UCL", "food"))

# Making histogram
histData <- na.omit(fullCoefs[rn %in% c("25-50k", "50-100k", ">100k")])
histData[, "income" := factor(rn, levels = c("25-50k", "50-100k", ">100k"), ordered = TRUE)]
histData[p > 0.05, "beta" := 0]
fwrite(histData, "/scratch/upenn/hossaine/discountingBehaviorChannel2.csv")

# Download
# scp hossaine@wrds-cloud.wharton.upenn.edu:/scratch/upenn/hossaine/discountingBehaviorChannel1.csv /home/mallick/Downloads
histData <- fread("/home/mallick/Downloads/discountingBehaviorChannel1.csv")
# histData <- fread("/home/mallick/Downloads/discountingBehaviorChannel1AddDiscount.csv")

# Add product groups
groupData <- setDT(readxl::read_excel("/home/mallick/Desktop/Nielsen/Data/Panel/Product_Hierarchy_01.31.2019.xlsx"))
groupData <- groupData[, .(product_module_code, product_module_descr,
                           product_group_code, product_group_descr)]
histData <- merge(histData, groupData, by.y = "product_module_code", by.x = "mod")
histData[, "income" := factor(rn, levels = c("25-50k", "50-100k", ">100k"), ordered = TRUE)]

# Generating appendix table of coefficients
appxTable <- dcast(data = histData[food == 0],
                   product_module_descr ~ income, value.var = "beta")
setorder(appxTable, -`>100k`)
stargazer(appxTable, rownames = FALSE, summary = FALSE,
          out = "./code/6_paper/tables/appendixDiscountingBehaviorChannelNonFood.tex")

# Plotting coefficients
ggplot(data = histData,
       aes(fill = income, y = beta, x = reorder(as.factor(mod), -beta), by = food)) +
  geom_bar(position = "identity", stat = "identity", alpha = 0.6) +
  #geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.2) +
  #facet_wrap(vars(product_group_descr), scales = "free") +
  #facet_wrap(vars(food), scales = "free_x") +
  labs(x = "Product Category",
       y = "Log-Point Increase Over\nPoorest Households",
       fill = "Household Income") +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 14),
        axis.ticks.length = unit(0.25, "cm")) +
  scale_fill_economist()
ggsave("./code/5_figures/discountingBehaviorChannelAll.pdf", height = 4, width = 6)

# Plotting coefficients for only nonfood products (dropping bath oil dry [8602])
histData[mod == 7003 & income == ">100k", "labels" := "Detergent"]
# histData[mod == 7008 & income == ">100k", "labels" := "Light Detergent"]
# histData[mod == 7012 & income == ">100k", "labels" := "Heavy Detergent"]
histData[mod == 7260 & income == ">100k", "labels" := "Toilet Paper"]
# histData[mod == 7370 & income == ">100k", "labels" := "Charcoal"]
# histData[mod == 7373 & income == ">100k", "labels" := "Logs"]
histData[mod == 7734 & income == ">100k", "labels" := "Paper Towels"]
histData[mod == 8444 & income == ">100k", "labels" := "Diapers"]

ggplot(data = histData[food == 0 & mod != 8602],
       aes(fill = income, y = beta, x = reorder(as.factor(mod), -beta),
           by = food, label = labels)) +
  geom_bar(position = "identity", stat = "identity", alpha = 0.6) +
  #geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.2) +
  #facet_wrap(vars(product_group_descr), scales = "free") +
  #facet_wrap(vars(food), scales = "free_x") +
  geom_text_repel(nudge_x = 0.5, nudge_y = 0.2, box.padding = 0.25,
                  segment.alpha = 0.4) +
  labs(x = "Product Category",
       y = "Log-Point Increase Over\nPoorest Households",
       fill = "Household Income") +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 14),
        axis.ticks.length = unit(0.25, "cm")) +
  scale_fill_manual(values = c("#084594", "#4292c6", "#9ecae1"))

ggsave("./code/5_figures/discountingBehaviorChannelNonFoodColor.pdf", height = 4, width = 6)
# ggsave("./code/5_figures/discountingBehaviorChannelNonFoodColorAddDiscount.pdf", height = 4, width = 6)

# Download
# scp hossaine@wrds-cloud.wharton.upenn.edu:/scratch/upenn/hossaine/discountingBehaviorChannel2.csv /home/mallick/Downloads
histData <- fread("/home/mallick/Downloads/discountingBehaviorChannel2.csv")

# Add product groups
groupData <- setDT(readxl::read_excel("/home/mallick/Desktop/Nielsen/Data/Panel/Product_Hierarchy_01.31.2019.xlsx"))
groupData <- groupData[, .(product_module_code, product_module_descr,
                           product_group_code, product_group_descr)]
histData <- merge(histData, groupData, by.y = "product_module_code", by.x = "mod")
histData[, "income" := factor(rn, levels = c("25-50k", "50-100k", ">100k"), ordered = TRUE)]

# Plotting coefficients for only nonfood products (dropping bath oil dry [8602])
histData[mod == 7734 & income == ">100k", "labels" := "Paper Towel"]
histData[mod == 7260 & income == ">100k", "labels" := "Toilet Paper"]
histData[mod == 7012 & income == ">100k", "labels" := "Detergent"]
histData[mod == 8444 & income == ">100k", "labels" := "Diapers"]
histData[mod == 7285 & income == ">100k", "labels" := "Plastic Wrap"]

ggplot(data = histData[food == 0 & mod != 8602],
       aes(fill = income, y = beta, x = reorder(as.factor(mod), -beta),
           by = food, label = labels)) +
  geom_bar(position = "identity", stat = "identity", alpha = 0.6) +
  #geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.2) +
  #facet_wrap(vars(product_group_descr), scales = "free") +
  #facet_wrap(vars(food), scales = "free_x") +
  geom_text_repel(nudge_x = 0.5, nudge_y = 0.2, box.padding = 0.25,
                  segment.alpha = 0.4) +
  labs(x = "Product Category",
       y = "Log-Point Increase Over\nPoorest Households",
       fill = "Household Income") +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom") +
  scale_fill_grey()
# scale_fill_economist()
ggsave("./code/5_figures/discountingBehaviorChannelNonFoodMaxSize.pdf", height = 4, width = 6)
# ggsave("./code/5_figures/discountingBehaviorChannelNonFoodMaxSizeColor.pdf", height = 4, width = 6)




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
