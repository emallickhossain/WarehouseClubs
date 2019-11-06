# Constructs size and quantity distribution for toilet paper
library(data.table)
library(ggplot2)
library(ggthemes)
library(stringr)
library(Hmisc)
threads <- 8

################################################################################
################ ALL PRODUCTS ##################################################
################################################################################
# Looking at sheets on rolls scatter plot for all possible products
tp <- fread("/scratch/upenn/hossaine/fullProd.csv")[product_module_code == 7260]

# Getting rolls and sheets for each product
tp[, "rolls" := as.integer(multi * size1_amount)]
tp[, "ply" := str_extract_all(upc_descr, "\\s\\dP\\s")]
tp[, "ply" := as.integer(gsub("P", "", ply))]
tp[, "sheet" := str_extract_all(upc_descr, "\\d{2,}S\\s")]
tp[, "sheet" := as.integer(gsub("S", "", sheet)) * ply]
tp[, "totalSheet" := sheet * rolls]
tp[, c("multi", "size1_amount", "ply") := NULL]
tp[, "days" := totalSheet / (57 * 2)]

# Getting quantiles of package sizes purchased by household
tpPurch <- fread("/scratch/upenn/hossaine/fullTPPurchases.csv", nThread = threads,
                 select = c("household_code", "panel_year",
                            "upc_choice", "upc_ver_uc_choice"))
tpPurch <- merge(tpPurch, tp[, .(upc, upc_ver_uc, totalSheet)],
                 by.x = c("upc_choice", "upc_ver_uc_choice"),
                 by.y = c("upc", "upc_ver_uc"))
panel <- fread("/scratch/upenn/hossaine/fullPanel.csv",
               select = c("household_code", "panel_year", "household_size",
                          "age", "married", "child", "projection_factor"))
tpPurch <- merge(tpPurch, panel, by = c("household_code", "panel_year"))
tpPurch[, c("upc_choice", "upc_ver_uc_choice") := NULL]

annualPurch <- tpPurch[, .(totalSheet = mean(totalSheet)),
                       by = .(household_code, panel_year, household_size, age,
                              married, child, projection_factor)]

# Converting to a day's measure
annualPurch[, "days" := totalSheet / (57 * 2)]
sheetQtile <- annualPurch[, .(totalSheet25 = wtd.quantile(days, probs = 0.25,
                                                          weights = projection_factor),
                              totalSheet75 = wtd.quantile(days, probs = 0.75,
                                                          weights = projection_factor)),
                      by = .(panel_year)]
sheetQtile25 <- mean(sheetQtile$totalSheet25)
sheetQtile75 <- mean(sheetQtile$totalSheet75)

# Graphing
topBrands <- c("ANGEL SOFT", "CHARMIN", "KLEENEX COTTONELLE", "QUILTED NORTHERN",
               "SCOTT 1000", "CTL BR")
ggplot(data = tp[brand_descr %in% topBrands & rolls <= 24],
       aes(x = days, y = rolls, color = brand_descr)) +
  geom_point(size = 0.2) +
  geom_vline(xintercept = sheetQtile25, linetype = "dashed") +
  geom_vline(xintercept = sheetQtile75, linetype = "dashed") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_jitter(width = 0) +
  facet_wrap(vars(brand_descr)) +
  labs(x = "Days' Supply",
       y = "Rolls") +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        legend.position = "none",
        text = element_text(size = 14),
        axis.ticks.length = unit(0.25, "cm")) +
  scale_color_manual(values = rep("#253494", 6))
ggsave(filename = "./figures/tpSizeQuantityDistributionAllProdsColor.pdf", height = 4, width = 6)

################################################################################
################## MNL ESTIMATION DATA #########################################
################################################################################
fullChoiceFinal <- fread("/scratch/upenn/hossaine/fullChoiceFinal.csv", nThread = threads)
fullChoiceFinal <- unique(fullChoiceFinal[, .(upc, upc_ver_uc, brand_descr,
                                              rolls, totalSheet, stdTotalSheet)])
topBrands <- c("ANGEL SOFT", "CHARMIN", "KLEENEX COTTONELLE", "QUILTED NORTHERN",
               "SCOTT 1000", "CTL BR")
ggplot(data = fullChoiceFinal[brand_descr %in% topBrands & rolls <= 24],
       aes(x = rolls, y = totalSheet)) +
  geom_point(size = 1) +
  geom_jitter(height = 0) +
  facet_wrap(vars(brand_descr)) +
  labs(x = "Rolls",
       y = "Sheets") +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom") +
  scale_color_grey()
ggsave(filename = "./figures/tpSizeQuantityDistributionEstSample.pdf", height = 4, width = 6)
