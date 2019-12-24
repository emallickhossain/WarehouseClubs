# Calculate distribution of weeks observed for each product
# find /scratch/upenn/hossaine/nielsen_extracts/ -exec touch {} \;
library(data.table)
library(stringr)
library(purrr)
threads <- 8

fileNames <- list.files("/scratch/upenn/hossaine/nielsen_extracts/RMS/2016/Movement_Files/",
                        recursive = TRUE, full.names = TRUE)

getWeeks <- function(fileName) {
  print(fileName)
  assort <- fread(fileName, nThread = threads,
                  select = c("upc", "store_code_uc", "week_end"),
                  key = c("upc", "store_code_uc", "week_end"))
  weekCount <- assort[, uniqueN(week_end), by = .(upc, store_code_uc)]
  weekDist <- weekCount[, quantile(V1, seq(0, 1, 0.1))]
  weekDistWide <- as.data.table(t(weekDist))
  weekDistWide[, "mod" := str_sub(fileName, -13, -10)]
  weekDistWide[, "N" := nrow(weekCount)]
  return(weekDistWide)
}

fullWeeks <- rbindlist(map(fileNames, getWeeks), use.names = TRUE)
fwrite(fullWeeks, "/home/upenn/hossaine/Nielsen/Data/weekDistribution1.csv")
# fwrite(fullWeeks, "/home/upenn/hossaine/Nielsen/Data/weekDistribution2-4.csv")
# fwrite(fullWeeks, "/home/upenn/hossaine/Nielsen/Data/weekDistribution5-6.csv")
# fwrite(fullWeeks, "/home/upenn/hossaine/Nielsen/Data/weekDistribution7.csv")

# Saving full set of data
week1 <- fread("/home/upenn/hossaine/Nielsen/Data/weekDistribution1.csv")
week2 <- fread("/home/upenn/hossaine/Nielsen/Data/weekDistribution2-4.csv")
week3 <- fread("/home/upenn/hossaine/Nielsen/Data/weekDistribution5-6.csv")
week4 <- fread("/home/upenn/hossaine/Nielsen/Data/weekDistribution7.csv")
fullWeeks <- rbindlist(list(week1, week2, week3, week4), use.names = TRUE)

prod <- unique(fread("/scratch/upenn/hossaine/fullProd.csv",
                     select = "product_module_code"))
fullWeeks <- fullWeeks[mod %in% prod$product_module_code]
fwrite(fullWeeks, "/home/upenn/hossaine/fullWeeks.csv")
