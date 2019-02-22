# Gets package size purchase differences between rich and poor households
library(data.table)
library(survival)
library(survminer)
library(ggplot2)
library(ggthemes)
library(ggfortify)

path <- "/scratch/upenn/hossaine/"

tpPurch <- fread(paste0(path, "tpPurch.csv"))
tpPurch[, c("upc", "upc_ver_uc", "trip_code_uc", "quantity", "product_module_code",
                "upc_descr", "product_module_descr", "product_group_code",
                "product_group_descr", "department_code", "department_descr",
                "size1_code_uc", "size1_units", "dataset_found_uc",
                "size1_change_flag_uc") := NULL]
tpPurch[, "purch" := ifelse(is.na(IPD) | IPD == 0, 0L, 1L)]
tpPurch[, "household_income_coarse" := factor(household_income_coarse,
                                                  levels = c("<25k", "25-50k", "50-100k", ">100k"),
                                                  ordered = TRUE)]
reg <- coxph(data = tpPurch, Surv(IPD, purch) ~ household_income_coarse +
               as.factor(panel_year) + as.factor(month) +
               market + household_size + as.factor(type_of_residence) +
               as.factor(marital_status) + as.factor(race) +
               as.factor(hispanic_origin) + age + as.factor(urban) +
               as.factor(college) + as.factor(household_composition))
ggadjustedcurves(reg, variable = "household_income_coarse", data = tpPurch,
                 title = "Interpurchase Durations (TP)",
                 legend.title = "Income",
                 legend.labs = c("<25k", "25-50k", "50-100k", ">100k"),
                 xlab = "Interpurchase Time (Days)",
                 ylab = "Survival Probability")
ggsave("./figures/IPDsurvivalTP.png")
