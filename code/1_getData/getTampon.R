# Gets and cleans tampon data.
source("./Nielsen/getItem.R")

# Inputs
yr <- 2006:2016
prodCode <- 7270

# Restrictions
sizeLim <- c(0, 96)
packageLim <- c(0, 1)
annualLim <- c(12, 365)
unitCostLim <- c(0.05, 1)

# Getting tampon purchases
prod <- fread(paste0(path, "prod.csv"),
              select = c("upc", "upc_ver_uc", "upc_descr", "product_module_code",
                         "brand_code_uc", "brand_descr", "multi", "size1_amount",
                         "size1_units"))[product_module_code %in% prodCode]
prod[, "size" := multi * size1_amount]
prod <- prod[size <= max(sizeLim) & size >= min(sizeLim)]
tamponPurch <- getItem(prod)

# Applying limits
tamponPurch <- tamponPurch[packages >= min(packageLim) & packages <= max(packageLim)]
tamponPurch <- tamponPurch[unitCost >= min(unitCostLim) & unitCost <= max(unitCostLim)]
tamponPurch <- tamponPurch[total >= min(annualLim) & total <= max(annualLim)]
fwrite(tamponPurch, paste0(path, prodCode, "Purch.csv"))
