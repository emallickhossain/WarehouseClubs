# Calculates how often I see particular products in the scanner data at each store.
# find /scratch/upenn/hossaine/nielsen_extracts/RMS/2016 -exec touch {} \;
library(data.table)
library(purrr)
threads <- 8
path <- "/scratch/upenn/hossaine/nielsen_extracts/RMS/"

# Load data
prod <- fread("/scratch/upenn/hossaine/fullProd.csv", nThread = threads,
              select = c("upc", "upc_ver_uc", "brand_code_uc", "quintile",
                         "totalAmount", "product_module_code"))
rms <- fread(paste0("/scratch/upenn/hossaine/nielsen_extracts/RMS/2016/",
                    "Annual_Files/rms_versions_2016.tsv"), drop = "panel_year")

fileNames <- list.files("/scratch/upenn/hossaine/nielsen_extracts/RMS/2016/Movement_Files/",
                        recursive = TRUE, full.names = TRUE)

getWeeks <- function(fileName) {
  print(fileName)
  assort <- fread(fileName, nThread = threads,
                  select = c("upc", "store_code_uc", "week_end", "price", "units"))
  assort <- merge(assort, rms, by = "upc")
  assort <- merge(assort, prod, by = c("upc", "upc_ver_uc"))
  modCode <- unique(assort$product_module_code)
  finalDat <- assort[, .(weeks = uniqueN(week_end),
                         sales = sum(price * units),
                         units = sum(units)),
                     by = .(upc, upc_ver_uc, store_code_uc, brand_code_uc)]
  finalDat[, "mod" := modCode]
  return(finalDat)
}

weekCount <- rbindlist(map(fileNames, getWeeks), use.names = TRUE)
# fwrite(weekCount, "/scratch/upenn/hossaine/scannerQualityCheck1a.csv")
fwrite(weekCount, "/scratch/upenn/hossaine/scannerQualityCheck1b.csv")
# fwrite(weekCount, "/scratch/upenn/hossaine/scannerQualityCheck2.csv")
# fwrite(weekCount, "/scratch/upenn/hossaine/scannerQualityCheck3.csv")
# fwrite(weekCount, "/scratch/upenn/hossaine/scannerQualityCheck4.csv")
# fwrite(weekCount, "/scratch/upenn/hossaine/scannerQualityCheck5.csv")
# fwrite(weekCount, "/scratch/upenn/hossaine/scannerQualityCheck6.csv")
# fwrite(weekCount, "/scratch/upenn/hossaine/scannerQualityCheck7.csv")
