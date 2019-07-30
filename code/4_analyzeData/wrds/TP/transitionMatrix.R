# Looking at purchase transitions between brands and sizes
library(data.table)
library(ggplot2)
library(ggthemes)
library(stringr)
threads <- 8

trips <- fread("/scratch/upenn/hossaine/fullTrips.csv", nThread = threads,
               select = c("household_code", "trip_code_uc", "purchase_date"))

# Get purchase data
purch <- NULL
for (yr in 2004:2017){
  print(yr)
  dat <- fread(paste0("/scratch/upenn/hossaine/fullPurch", yr, ".csv"),
               nThread = threads,
               select = c("trip_code_uc", "packagePrice", "product_module_code",
                          "brand_code_uc", "totalAmount", "food",
                          "brand_descr"))[product_module_code == 7260]
  purch <- rbindlist(list(purch, dat), use.names = TRUE)
  rm(dat)
}

purch <- merge(purch, trips, by = "trip_code_uc")
purch[, "purchase_date" := as.Date(purchase_date)]
setkey(purch, household_code, purchase_date)
purch[, "tripNumber" := 1:.N, by = household_code]
purch[, "totalTrips" := max(tripNumber), by = household_code]

# Getting transition matrix
topBrands <- purch[, sum(totalAmount), by = brand_code_uc]
setorder(topBrands, -V1)
purch[, "totalAmount" := str_pad(totalAmount, 2, "left", "0")]

purch[brand_code_uc %in% topBrands$brand_code_uc[1:6],
      ':=' (newBrandSize = paste(brand_descr, totalAmount, sep = "_"),
            newBrand = brand_descr,
            newSize = totalAmount)]
purch[brand_code_uc %in% topBrands$brand_code_uc[1:6],
      ':=' (oldBrandSize = shift(newBrandSize, 1, type = "lag"),
            oldBrand = shift(newBrand, 1, type = "lag"),
            oldSize = shift(newSize, 1, type = "lag"))]
transMat <- prop.table(with(purch, table(oldBrandSize, newBrandSize)), 1)
meltTransMat <- setDT(melt(transMat))
setorder(meltTransMat, oldBrandSize, newBrandSize)
ggplot(data = meltTransMat,
       aes(x = as.factor(newBrandSize), y = as.factor(oldBrandSize), fill = value)) +
  geom_tile() +
  geom_text(aes(as.factor(newBrandSize), as.factor(oldBrandSize), label = round(value, 2)),
            color = "white", size = 4)
