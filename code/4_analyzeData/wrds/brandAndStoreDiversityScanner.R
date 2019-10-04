# Computes number of brands and sizes offered by each store in Scanner data
library(data.table)
library(purrr)
library(knitr)
library(ggplot2)
library(ggthemes)
threads <- 8
path <- "/scratch/upenn/hossaine/nielsen_extracts/RMS/"

# Download 2016 Scanner from Globus for Sanitary products and departments 1:7
# Unzip

# Load data
prod <- fread("/scratch/upenn/hossaine/fullProd.csv", nThread = threads,
              select = c("upc", "upc_ver_uc", "brand_code_uc",
                         "totalAmount", "product_module_code", "food"))
rms <- fread(paste0("/scratch/upenn/hossaine/nielsen_extracts/RMS/2016/",
                    "Annual_Files/rms_versions_2016.tsv"), drop = "panel_year")

fileNames <- list.files("/scratch/upenn/hossaine/nielsen_extracts/RMS/2016/Movement_Files",
                        recursive = TRUE, full.names = TRUE)

fullAssort <- NULL
for (i in fileNames) {
    assort <- fread(i, select = c("upc", "store_code_uc"), nThread = threads)
    assort <- merge(assort, rms, by = "upc")
    assort <- merge(assort, prod, by = c("upc", "upc_ver_uc"))[, c("upc", "upc_ver_uc") := NULL]
    finalData <- assort[, .(Nbrands = uniqueN(brand_code_uc),
                            Nsizes = uniqueN(totalAmount),
                            NbrandSize = uniqueN(data.table(brand_code_uc, totalAmount)),
                            avgSize = mean(totalAmount)),
                        by = .(store_code_uc, product_module_code, food)]
    fullAssort <- rbindlist(list(fullAssort, finalData), use.names = TRUE)
    modCode <- unique(finalData$product_module_code)
    print(modCode)
}

fwrite(fullAssort, "/home/upenn/hossaine/Nielsen/Data/fullAssort1a.csv")
#fwrite(fullAssort, "/home/upenn/hossaine/Nielsen/Data/fullAssort1b.csv")
#fwrite(fullAssort, "/home/upenn/hossaine/Nielsen/Data/fullAssort2.csv")
#fwrite(fullAssort, "/home/upenn/hossaine/Nielsen/Data/fullAssort3-6.csv")
#fwrite(fullAssort, "/home/upenn/hossaine/Nielsen/Data/fullAssort7.csv")

# Getting all counts
getCount <- function(i) {
    data <- fread(paste0("/home/upenn/hossaine/Nielsen/Data/fullAssort", i, ".csv"))
    return(data)
}
fullCount <- rbindlist(map(c("1a", "1b", "2", "3-6", "7"), getCount), use.names = TRUE)
fullCount[, "foodChar" := ifelse(food == 1, "Food", "Non-Food")]

# Merging with store types
stores <- fread(paste0(path, "2016/Annual_Files/stores_2016.tsv"),
                select = c("store_code_uc", "channel_code"), nThread = threads)
fullCount <- merge(fullCount, stores, by = "store_code_uc")

# Getting characteristics for TP
tp <- fullCount[product_module_code == 7260]
tp[, quantile(NbrandSize, c(0.25, 0.5, 0.75)), by = channel_code]

# Averaging counts across different dimensions
ggplot(data = fullCount, aes(x = Nbrands)) +
    geom_histogram(aes(y = ..density..), bins = 50) +
    facet_grid(cols = vars(foodChar), rows = vars(channel_code)) +
    scale_x_continuous(limits = c(0, 100)) +
    labs(title = "Bulk Discounts Are Common and Substantial",
         subtitle = "Discounts are Larger for Non-Food Items",
         x = "Count",
         y = "Density",
         caption = paste0("Source: Author calculations from Nielsen Scanner Data for 2016.")) +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(), plot.caption = element_text(hjust = 0)) +
    scale_fill_grey()
