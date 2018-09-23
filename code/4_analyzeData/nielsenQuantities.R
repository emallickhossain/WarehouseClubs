# Get total quantities purchased over time
# Large is defined as in Coibion et al (2017)
library(data.table)
years <- 2016
cutoff <- 0.9
samp <- 10000

products <- fread(paste0("/home/mallick/Desktop/Nielsen/Data/Consumer_Panel",
                         "_Data_2004-2016/nielsen_extracts/HMS/Master_Files/",
                         "Latest/products.tsv"),
                  select = c("upc", "upc_ver_uc", "multi", "size1_amount",
                             "size1_units", "product_module_code"))
products[, "size" := multi * size1_amount]
products[, c("multi", "size1_amount") := NULL]
products <- products[!is.na(size)]
products[, "cutoff" := quantile(size, probs = cutoff), by = product_module_code]
products[, "large" := ifelse(size > cutoff)]

getPur <- function(year) {
  purchases <- fread(paste0("/home/mallick/Desktop/Nielsen/Data/Consumer_Panel",
                            "_Data_2004-2016/nielsen_extracts/HMS/", year,
                            "/Annual_Files/purchases_", year, ".tsv"),
                     nrows = samp)
  purchases <- merge(purchases, products, by = c("upc", "upc_ver_uc"))
  unitPur <- purchases[, .(quantity = sum(quantity * qty),
                           price = sum(total_price_paid)),
                       by = c("trip_code_uc", "size1_units")]

  trip <- fread(paste0("/home/mallick/Desktop/Nielsen/Data/Consumer_Panel_Data",
                       "_2004-2016/nielsen_extracts/HMS/", year, "/Annual_Files/",
                       "trips_", year, ".tsv"),
                select = c("trip_code_uc", ""))
}
