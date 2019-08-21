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

fileNames <- list.files("/scratch/upenn/hossaine/nielsen_extracts/RMS/2016/Movement_Files",
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
    betaCoef[, "mod" := modCode]
    return(betaCoef)
  }, error = function(e){})
}

fullBetas <- rbindlist(map(fileNames, getBetas), use.names = TRUE)
#fwrite(fullBetas, "/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas1a.csv") #through group 514
#fwrite(fullBetas, "/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas1b.csv") #through group 1021
fwrite(fullBetas, "/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas1c.csv") #through group 1508
#fwrite(fullBetas, "/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas2.csv")



#fwrite(fullBetas, "/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas1x.csv")
# tampons and pads
#fwrite(fullBetas, "/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas1z.csv")
# 1z covers groups 508:1001
#fwrite(fullBetas, "/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas1y.csv")
# 1y covers groups 1002:1021
#fwrite(fullBetas, "/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas1a.csv")
# 1a only ran through until 1092 and 1125 (groups 501:507)
#fwrite(fullBetas, "/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas1b.csv")
# 1b only ran through 1362, 4000:4009, 4011, 4012, 1484, and 1553
#fwrite(fullBetas, "/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas1c.csv")
#fwrite(fullBetas, "/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas3-4.csv")
#fwrite(fullBetas, "/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas5-6.csv")
#fwrite(fullBetas, "/home/upenn/hossaine/Nielsen/Data/scannerBulkDiscountBetas7.csv")

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
coefs <- rbindlist(map(c("1a", "1b", "1c", "2", "3", "4", "5", "6", "7"), getCoefs),
                   use.names = TRUE)
coefs[abs(`t value`) <= 3, "Estimate" := 0]

# Getting food classification
prod <- fread("/home/mallick/Downloads/prodFood.csv")
setnames(prod, "product_module_code", "mod")
coefs <- merge(coefs, prod, by = "mod")
coefs[, "foodChar" := ifelse(food == 1, "Food", "Non-Food")]

# Getting share of products with bulk discounts
sum(coefs$Estimate < 0) / nrow(prod)

# Plotting distribution of betas
ggplot(data = coefs, aes(x = Estimate, fill = foodChar)) +
  geom_histogram(aes(y = ..density..), bins = 50, alpha = 0.65, position = "identity") +
  geom_density(alpha = 0.8) +
  scale_x_continuous(limits = c(-2, 1)) +
  labs(x = "Unit Price Change",
       y = "Density",
       fill = "Product Type") +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom") +
  scale_fill_grey()

ggsave(filename = "./code/5_figures/bulkDiscountAllProdsScanner.png",
       height = 4, width = 6)
