# Computes bulk discounts in the 2016 Scanner data
library(data.table)
library(lfe)
library(purrr)
library(ggplot2)
library(ggthemes)
library(stringr)
library(foreach)
threads <- 8
path <- "/scratch/upenn/hossaine/nielsen_extracts/RMS/"

# Download 2016 Scanner from Globus for Sanitary products and departments 1:7
# Unzip

# Load data
prod <- fread("/scratch/upenn/hossaine/fullProd.csv", nThread = threads,
              select = c("upc", "upc_ver_uc", "brand_code_uc",
                         "totalAmount", "product_module_code"))
rms <- fread(paste0("/scratch/upenn/hossaine/nielsen_extracts/RMS/2016/",
                    "Annual_Files/rms_versions_2016.tsv"), drop = "panel_year")

fileNames <- list.files("/scratch/upenn/hossaine/nielsen_extracts/RMS/2016/Movement_Files/1503_2016",
                        recursive = TRUE, full.names = TRUE)

getBetas <- function(fileName) {
  tryCatch({
    print(fileName)
    assort <- fread(fileName, nThread = threads,
                    select = c("upc", "store_code_uc", "week_end", "price"))
    assort <- merge(assort, rms, by = "upc")
    assort <- merge(assort, prod, by = c("upc", "upc_ver_uc"))[, c("upc", "upc_ver_uc") := NULL]
    assort[, "storeWeekBrand" := paste(store_code_uc, week_end, brand_code_uc, sep = "_")]
    assort[, c("lunitPrice", "lq", "price") := .(log(price / totalAmount), log(totalAmount), NULL)]
    modCode <- unique(assort$product_module_code)
    assort[, c("totalAmount", "product_module_code") := NULL]

    if (!modCode %in% c(1484, 1553, 1362, 4000, 1327, 1323)) {
      # Running regressions
      reg1 <- felm(data = assort, lunitPrice ~ lq)
      reg1Coef <- as.data.table(summary(reg1)$coefficients, keep.rownames = TRUE)
      reg1Coef[, "reg" := "No FE"]

      reg2 <- felm(data = assort, lunitPrice ~ lq | store_code_uc)
      reg2Coef <- as.data.table(summary(reg2)$coefficients, keep.rownames = TRUE)
      reg2Coef[, "reg" := "Store FE"]

      reg3 <- felm(data = assort, lunitPrice ~ lq | store_code_uc + brand_code_uc)
      reg3Coef <- as.data.table(summary(reg3)$coefficients, keep.rownames = TRUE)
      reg3Coef[, "reg" := "Store and Brand FE"]

      reg4 <- felm(data = assort, lunitPrice ~ lq | store_code_uc + week_end)
      reg4Coef <- as.data.table(summary(reg4)$coefficients, keep.rownames = TRUE)
      reg4Coef[, "reg" := "Store and Week FE"]

      reg5 <- felm(data = assort, lunitPrice ~ lq | store_code_uc + week_end + brand_code_uc)
      reg5Coef <- as.data.table(summary(reg5)$coefficients, keep.rownames = TRUE)
      reg5Coef[, "reg" := "Store, Week, Brand FE"]

      reg6 <- felm(data = assort, lunitPrice ~ lq | storeWeekBrand)
      reg6Coef <- as.data.table(summary(reg6)$coefficients, keep.rownames = TRUE)
      reg6Coef[, "reg" := "Store-Week-Brand FE"]

      betaCoef <- rbindlist(list(reg1Coef, reg2Coef, reg3Coef, reg4Coef, reg5Coef,
                                 reg6Coef), use.names = TRUE)
    } else {
      reg6 <- felm(data = assort, lunitPrice ~ lq | storeWeekBrand)
      reg6Coef <- as.data.table(summary(reg6)$coefficients, keep.rownames = TRUE)
      reg6Coef[, "reg" := "Store-Week-Brand FE"]

      betaCoef <- rbindlist(list(reg6Coef), use.names = TRUE)
    }

    betaCoef[, "mod" := modCode]
    return(betaCoef)
  }, error = function(e){})
}

fullBetas <- getBetas(fileNames[1])
fwrite(fullBetas, "/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas1c15031484.csv") #group 1503
rm(fullBetas)

fullBetas <- getBetas(fileNames[2])
fwrite(fullBetas, "/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas1c15031553.csv") #group 1503
rm(fullBetas)

#fwrite(fullBetas, "/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas1a.csv") #through group 514
#fwrite(fullBetas, "/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas1b.csv") #through group 1021
#fwrite(fullBetas, "/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas1c1501.csv") #group 1501
#fwrite(fullBetas, "/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas1c1505.csv") #group 1505
#fwrite(fullBetas, "/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas1c1506.csv") #group 1506
#fwrite(fullBetas, "/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas1c1507.csv") #group 1507
#fwrite(fullBetas, "/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas1c1508.csv") #group 1508
#fwrite(fullBetas, "/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas2.csv")
#fwrite(fullBetas, "/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas3.csv")
#fwrite(fullBetas, "/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas4.csv")
#fwrite(fullBetas, "/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas5.csv")
#fwrite(fullBetas, "/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas6.csv")
#fwrite(fullBetas, "/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas7.csv") # through group 4510
#fwrite(fullBetas, "/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas7b.csv") # group 4511 and 5505

# Getting modules and food classification
prod <- unique(fread("/scratch/upenn/hossaine/fullProd.csv",
                     select = c("product_module_code", "food")))
fwrite(prod, "/home/upenn/hossaine/Nielsen/Data/prodFood.csv")

# Download and generate graph
# scp hossaine@wrds-cloud.wharton.upenn.edu:/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas*.csv /home/mallick/Downloads
# scp hossaine@wrds-cloud.wharton.upenn.edu:/home/upenn/hossaine/Nielsen/Data/prodFood.csv /home/mallick/Downloads
getCoefs <- function(i) {
  data <- fread(paste0("/home/mallick/Downloads/scannerBulkDiscountBetas", i, ".csv"))
  return(data)
}
coefs <- rbindlist(map(c("1a", "1b", "1c1501",
                         #"1c1503", "1c1505",
                         "1c1506", "1c1507", "1c1508", "2", "3", "4", "5", "6", "7", "7b"),
                       getCoefs), use.names = TRUE)
coefs[abs(`t value`) <= 3, "Estimate" := 0]

# Getting food classification
prod <- fread("/home/mallick/Downloads/prodFood.csv")
setnames(prod, "product_module_code", "mod")
coefs <- merge(coefs, prod, by = "mod")
coefs <- coefs[rn == "lq"]
coefs[, "foodChar" := ifelse(food == 1, "Food", "Non-Food")]

# Getting share of products with bulk discounts
nrow(coefs[reg == "Store-Week-Brand FE" & Estimate < 0]) / nrow(prod)

# Plotting distribution of betas
ggplot(data = coefs,
       aes(x = Estimate, y = stat(density), fill = foodChar)) +
  geom_histogram(bins = 50, alpha = 0.65, position = "identity") +
  geom_hline(yintercept = 0) +
  scale_x_continuous(limits = c(-2, 1)) +
  facet_wrap(vars(reg)) +
  labs(x = "Unit Price Change",
       y = "Density",
       fill = "Product Type") +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom") +
  scale_fill_grey()
  # scale_fill_fivethirtyeight()
ggsave(filename = "./code/5_figures/appendixBulkDiscountAllProdsScanner.pdf",
       height = 4, width = 6)
# ggsave(filename = "./code/5_figures/appendixBulkDiscountAllProdsScannerColor.pdf",
       # height = 4, width = 6)

# Plotting distribution of betas
annot <- coefs[, .(Mean = round(mean(Estimate, na.rm = TRUE), 2),
                     SD = round(sd(Estimate, na.rm = TRUE), 2)), by = foodChar]
ggplot(data = coefs[reg == "Store-Week-Brand FE"],
       aes(x = Estimate, y = stat(density), fill = foodChar)) +
  geom_histogram(bins = 30, alpha = 0.65, position = "identity") +
  geom_vline(xintercept = annot$Mean[1], linetype = 2,
             color = gray.colors(2)[1]) +
             # color = fivethirtyeight_pal()(2)[1]) +
  geom_vline(xintercept = annot$Mean[2], linetype = 2,
             color = gray.colors(2)[2]) +
             # color = fivethirtyeight_pal()(2)[2]) +
  annotate("text", label = paste0("Mean: ", annot$Mean[1], "\n", "SD: ", annot$SD[1]),
           x = annot$Mean[1] + 0.25, y = 0.7) +
  annotate("text", label = paste0("Mean: ", annot$Mean[2], "\n", "SD: ", annot$SD[2]),
           x = annot$Mean[2] - 0.25, y = 1.2) +
  scale_x_continuous(limits = c(-2, 1)) +
  labs(x = "Elasticity of Unit Price and Package Size",
       y = "Density",
       fill = "Product Type") +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom") +
  scale_fill_grey()
  # scale_fill_fivethirtyeight()

ggsave(filename = "./code/5_figures/bulkDiscountAllProdsScanner.pdf", height = 4, width = 6)
# ggsave(filename = "./code/5_figures/bulkDiscountAllProdsScannerColor.pdf", height = 4, width = 6)


# Getting typical range of sizes (log(big) - log(small))
prod <- fread("/scratch/upenn/hossaine/fullProd.csv")
avg <- prod[, .(avgSize = mean(totalAmount)),
            keyby = .(product_module_code, food, quintile)]
avgWide <- dcast(avg, product_module_code + food ~ quintile, value.var = "avgSize")
avgWide[, "twoToFour" := `4` / `2`]
fwrite(avgWide, "/scratch/upenn/hossaine/avgWide.csv")

# Plotting distribution of log sizes
# scp hossaine@wrds-cloud.wharton.upenn.edu:/scratch/upenn/hossaine/avgWide.csv /home/mallick/Downloads
avgWide <- fread("/home/mallick/Downloads/avgWide.csv")
avgWide[, "foodChar" := ifelse(food == 0, "Non-Food", "Food")]
annot <- avgWide[, .(Mean = round(mean(twoToFour, na.rm = TRUE), 2),
                     SD = round(sd(twoToFour, na.rm = TRUE), 2)), by = foodChar]
ggplot(data = avgWide,
       aes(x = twoToFour, y = stat(density), fill = foodChar)) +
  geom_histogram(bins = 30, alpha = 0.65, position = "identity") +
  geom_vline(xintercept = annot$Mean[1], linetype = 2, color = gray.colors(2)[1]) +
  geom_vline(xintercept = annot$Mean[2], linetype = 2, color = gray.colors(2)[2]) +
  # geom_vline(xintercept = annot$Mean[1], linetype = 2, color = fivethirtyeight_pal()(2)[1]) +
  # geom_vline(xintercept = annot$Mean[2], linetype = 2, color = fivethirtyeight_pal()(2)[2]) +
  annotate("text", label = paste0("Mean: ", annot$Mean[1], "\n", "SD: ", annot$SD[1]),
           x = annot$Mean[1] - 0.3, y = 0.9) +
  annotate("text", label = paste0("Mean: ", annot$Mean[2], "\n", "SD: ", annot$SD[2]),
           x = annot$Mean[2] + 0.3, y = 0.9) +
  scale_x_continuous(limits = c(1, 5)) +
  labs(x = "Ratio of 4th and 2nd Size Quintile",
       y = "Density",
       fill = "Product Type") +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom") +
  scale_fill_grey()
  # scale_fill_fivethirtyeight()

ggsave(filename = "./code/5_figures/prodSizeDiff.pdf", height = 4, width = 6)
# ggsave(filename = "./code/5_figures/prodSizeDiffColor.pdf", height = 4, width = 6)


# Getting visual of price/size variation for TP
tp <- fread("/scratch/upenn/hossaine/fullTPAssortment.csv", nThread = threads)
prod <- fread("/scratch/upenn/hossaine/prodTP.csv", nThread = threads)
tp <- merge(prod, tp, by = c("upc", "upc_ver_uc"))
obs <- tp[, .N, by = .(store_code_uc, brand_descr, week_end)]
setorder(obs, -N)
ex <- tp[store_code_uc == 2706061 & brand_descr == "CHARMIN"]
ex[, "unitPrice" := price / totalSheet * 1000]
ex[, "unitPriceRolls" := price / rolls]
ex[, "panel_year" := as.integer(substr(week_end, 1, 4))]

ggplot(data = ex[panel_year == 2016],
       aes(x = totalSheet / 1000, y = unitPrice)) +
  geom_point(size = 1, position = "jitter") +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  labs(x = "Total Sheets (thousands)",
       y = "Unit Price ($ / 1000 sheets)") +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom") +
  scale_color_fivethirtyeight()
ggsave(filename = "./figures/bulkDiscountVariation.pdf", height = 4, width = 6)
