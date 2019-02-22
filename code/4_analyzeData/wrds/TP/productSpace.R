# Puts toilet paper in product space
library(data.table)
library(stringr)
library(plot3D)
path <- "/scratch/upenn/hossaine/"
tp <- fread(paste0(path, "tp.csv"),
            select = c("upc_descr", "brand_code_uc", "brand_descr", "multi",
                       "sizeUnadj", "size"))
brands <- c(526996, 506045, 624459, 534596, 536746)
#CHARMIN, ANGEL SOFT, QUILTED NORTHERN,

# For Toilet paper, creating standardized roll
tp[, "ply" := str_extract_all(upc_descr, "\\s\\dP\\s")]
tp[, "ply" := as.integer(gsub("P", "", ply))]
tp[, "sheet" := str_extract_all(upc_descr, "\\d{2,}S\\s")]
tp[, "sheet" := as.integer(gsub("S", "", sheet))]

with(tp[brand_code_uc %in% brands], scatter3D(x = ply, y = sheet, z = size, type = "p",
                   xlab = "Ply", ylab = "Sheet", zlab = "Rolls",
                   ticktype = "detailed", colvar = brand_code_uc))
