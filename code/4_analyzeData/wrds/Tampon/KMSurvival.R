# Gets package size purchase differences between rich and poor households
library(data.table)
library(survival)
library(survminer)
library(ggplot2)
library(ggthemes)
library(ggfortify)

path <- "/scratch/upenn/hossaine/"

tamponPurch <- fread(paste0(path, "tamponPurch.csv"))
tamponPurch[, c("upc", "upc_ver_uc", "trip_code_uc", "quantity", "product_module_code",
                "upc_descr", "product_module_descr", "product_group_code",
                "product_group_descr", "department_code", "department_descr",
                "size1_code_uc", "size1_units", "dataset_found_uc",
                "size1_change_flag_uc") := NULL]
tamponPurch[, "purch" := ifelse(is.na(IPD) | IPD == 0, 0L, 1L)]
tamponPurch[, "household_income_coarse" := factor(household_income_coarse,
                                                  levels = c("<25k", "25-50k", "50-100k", ">100k"),
                                                  ordered = TRUE)]
# Cox Plot
reg <- coxph(data = tamponPurch, Surv(IPD, purch) ~ household_income_coarse +
               as.factor(panel_year) + as.factor(month) +
               market + household_size + as.factor(type_of_residence) +
               as.factor(marital_status) + as.factor(race) +
               as.factor(hispanic_origin) + age + as.factor(urban) +
               as.factor(college) + as.factor(household_composition))
ggadjustedcurves(reg, variable = "household_income_coarse", data = tamponPurch,
                 title = "Interpurchase Durations (Tampon)",
                 legend.title = "Income",
                 legend.labs = c("<25k", "25-50k", "50-100k", ">100k"),
                 xlab = "Interpurchase Time (Days)",
                 ylab = "Survival Probability")
ggsave("./figures/IPDsurvivalTampon.png")
