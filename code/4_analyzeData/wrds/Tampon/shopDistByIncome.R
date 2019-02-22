# Gets travel distance by income
library(data.table)
library(ggplot2)
library(ggridges)
library(ggthemes)

path <- "/scratch/upenn/hossaine/"

tamponPurch <- fread(paste0(path, "tamponPurch.csv"))
tamponPurch[, c("upc", "upc_ver_uc", "trip_code_uc", "quantity", "product_module_code",
                "upc_descr", "product_module_descr", "product_group_code",
                "product_group_descr", "department_code", "department_descr",
                "size1_code_uc", "size1_units", "dataset_found_uc",
                "size1_change_flag_uc", "purchase_date") := NULL]

tamponPurch[, "household_income_coarse" := factor(household_income_coarse,
                                                  levels = c("<25k", "25-50k", "50-100k", ">100k"),
                                                  ordered = TRUE)]

ggplot(data = tamponPurch, aes(x = distKM, y = household_income_coarse)) +
  geom_density_ridges(rel_min_height = 0.01) +
  theme_fivethirtyeight() +
  xlim(0, 50) +
  labs(x = "Distance (km)",
       y = "Income",
       title = "Shopping Distance by Income Group",
       caption = "Source: Nielsen Consumer Panel")
