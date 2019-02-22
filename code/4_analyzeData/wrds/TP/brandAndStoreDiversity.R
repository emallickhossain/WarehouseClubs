# Generates unique brands and stores across households
library(data.table)
library(ggthemes)
library(ggplot2)
library(lfe)
path <- "/scratch/upenn/hossaine/"

tpPurch <- fread(paste0(path, "7260Purch.csv"))
tpPurch[, c("upc", "upc_ver_uc", "trip_code_uc", "quantity", "product_module_code",
            "upc_descr", "product_module_descr", "product_group_code",
            "product_group_descr", "department_code", "department_descr",
            "size1_code_uc", "size1_units", "dataset_found_uc",
            "size1_change_flag_uc", "purchase_date") := NULL]

uniqueBrands <- tpPurch[, .(brands = uniqueN(brand_code_uc),
                            stores = uniqueN(retailer_code),
                            channels = uniqueN(channel_type)),
                        by = .(household_code, panel_year, projection_factor,
                               household_income, household_size, marital_status,
                               race, hispanic_origin, market, age, college, urban)]

reg1 <- felm(data = uniqueBrands, brands ~ household_income |
               panel_year + market +
               household_size + marital_status + race + hispanic_origin + age +
               college + urban, weights = uniqueBrands$projection_factor)
reg2 <- felm(data = uniqueBrands, stores ~ household_income |
               panel_year + market +
               household_size + marital_status + race + hispanic_origin + age +
               college + urban, weights = uniqueBrands$projection_factor)
reg3 <- felm(data = uniqueBrands, channels ~ household_income |
               panel_year + market +
               household_size + marital_status + race + hispanic_origin + age +
               college + urban, weights = uniqueBrands$projection_factor)

# Top brands by income bracket
hhid <- unique(tpPurch[, .(household_code, panel_year)])
hhid[, "hhid" := paste0(household_code, "_", panel_year)]
hhs <- setDT(expand.grid(hhid = hhid$hhid,
                         brand_code_uc = unique(tpPurch$brand_code_uc)))
hhs[, c("household_code", "panel_year") := tstrsplit(hhid, "_")]
hhs[, "hhid" := NULL]
hhs[, c("household_code", "panel_year") := .(as.integer(household_code),
                                             as.integer(panel_year))]

topBrands <- unique(tpPurch[, .(total = sum(size)),
                            by = .(household_code, panel_year, projection_factor,
                                   brand_code_uc, brand_descr, annualTP,
                                   household_income_coarse)])
rm(tpPurch, hhid)

apple <- merge(hhs, unique(topBrands[, .(brand_code_uc, brand_descr)]), all.x = TRUE,
               by = c("brand_code_uc"))

rm(hhs)
apple <- merge(apple, topBrands[, .(household_code, panel_year, brand_code_uc, brand_descr, total)],
               by = c("household_code", "panel_year", "brand_code_uc", "brand_descr"), all.x = TRUE)
apple[is.na(total), "total" := 0]
apple <- merge(apple, topBrands, by = c("household_code", "panel_year"))

avgBrand <- apple[, .(avg = weighted.mean(total, w = projection_factor),
                      total = weighted.mean(annualTP, w = projection_factor)),
                  by = .(household_income_coarse, brand_code_uc, brand_descr)]
avgBrand[, "share" := avg / total * 100]
avgBrand[, "household_income_coarse" := factor(household_income_coarse,
                                               levels = c("<25k", "25-50k", "50-100k", ">100k"),
                                               ordered = TRUE)]

setorder(avgBrand, -share)
brands <- avgBrand[, .SD[1:10], by = .(household_income_coarse)]
tp <- avgBrand[brand_code_uc %in% brands$brand_code_uc]
tp[brand_code_uc == 5367465850, "brand_descr" := "DOLLAR LABEL"]
tp[brand_code_uc == 5367466920, "brand_descr" := "DISCOUNT LABEL"]
tp[brand_code_uc == 5367469101, "brand_descr" := "WAREHOUSE LABEL 1"]
tp[brand_code_uc == 5367469103, "brand_descr" := "WAREHOUSE LABEL 2"]

ggplot(data = tp, aes(x = brand_descr, y = share)) +
  geom_bar(stat = "identity", aes(fill = household_income_coarse), position = "dodge") +
  theme_fivethirtyeight() +
  coord_flip() +
  labs(x = "Brand",
       y = "Purchase Share",
       fill = "Income",
       title = "Brand Shares by Income",
       caption = "Source: Nielsen Consumer Panel")
ggsave("./figures/brandsTP.png")
