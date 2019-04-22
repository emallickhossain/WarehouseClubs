# Puts toilet paper in product space
library(data.table)
library(stringr)
library(plot3D)
library(ggplot2)
library(ggthemes)
path <- "/scratch/upenn/hossaine/"
product <- 7260
tp <- fread(paste0(path, "prod.csv"),
            select = c("upc_descr", "brand_code_uc", "brand_descr", "multi",
                       "size1_amount", "product_module_code"))[product_module_code %in% product]
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

quantile(tp$sheet, na.rm = TRUE, seq(0, 1, 0.01))
quantile(tp$ply, na.rm = TRUE, seq(0, 1, 0.01))
quantile(tp$size1_amount, na.rm = TRUE, seq(0, 1, 0.01))
