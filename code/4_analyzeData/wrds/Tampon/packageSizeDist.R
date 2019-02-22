# Generates package size distribution
library(data.table)
library(ggplot2)
library(ggridges)
library(ggthemes)
library(lfe)
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
ggplot(tamponPurch, aes(x = size, y = household_income_coarse)) +
  geom_density_ridges(rel_min_height = 0.01, scale = 1) +
  theme_fivethirtyeight()

# Coefficient of variation within households
coefVar <- tamponPurch[, .(mean = mean(size),
                           sd = sd(size)),
                       by = .(household_code, panel_year, projection_factor,
                              household_income, household_size, type_of_residence,
                              marital_status, race, hispanic_origin,
                              market, household_composition, household_income_coarse,
                              age, college, urban)]
coefVar[, "cv" := sd / mean]
reg <- felm(data = coefVar, cv ~ as.character(household_income_coarse) |
              panel_year + market + household_size + type_of_residence +
              marital_status + race + hispanic_origin + age + urban +
              college + household_composition | 0 | market,
            weights = coefVar$projection_factor)
summary(reg)
