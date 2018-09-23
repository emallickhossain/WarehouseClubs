# Get product data
# Drops all magnet data
library(data.table)
path <- "/home/mallick/Desktop/Nielsen/Data/Consumer_Panel_Data_2004-2016/nielsen_extracts/HMS/"

prod <- fread(paste0(path, "Master_Files/Latest/products.tsv"), quote = "")
prod <- prod[department_code != 99]
prod <- prod[!product_module_code %in% 445:468]
fwrite(prod, "/home/mallick/Desktop/Nielsen/Data/Clean/prod.csv")
