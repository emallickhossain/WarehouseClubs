# Generates unique brands and stores across households
library(data.table)
library(lfe)
library(stargazer)
library(ggplot2)
library(ggthemes)
path <- "/scratch/upenn/hossaine/"

tamponPurch <- fread(paste0(path, "tamponPurch.csv"))
tamponPurch[, c("upc", "upc_ver_uc", "trip_code_uc", "quantity", "product_module_code",
            "upc_descr", "product_module_descr", "product_group_code",
            "product_group_descr", "department_code", "department_descr",
            "size1_code_uc", "size1_units", "dataset_found_uc",
            "size1_change_flag_uc", "purchase_date") := NULL]

uniqueBrands <- tamponPurch[, .(brands = uniqueN(brand_code_uc),
                            stores = uniqueN(retailer_code),
                            channels = uniqueN(channel_type),
                            trips = .N),
                        by = .(household_code, panel_year, projection_factor,
                               household_income, household_size, marital_status,
                               race, hispanic_origin, market, age, college,
                               urban, type_of_residence, household_composition)]
uniqueBrands[, ':=' (brandConc = 1 - (brands - 1) / trips,
                     storeConc = 1 - (stores - 1) / trips,
                     channelConc = 1 - (channels - 1) / trips)]

reg1 <- felm(data = uniqueBrands, brandConc ~ as.factor(household_income) |
               panel_year + market +
               household_size + marital_status + race + type_of_residence +
               hispanic_origin + age + college + urban + household_composition,
             weights = uniqueBrands$projection_factor)
reg2 <- felm(data = uniqueBrands, storeConc ~ as.factor(household_income) |
               panel_year + market +
               household_size + marital_status + race + type_of_residence +
               hispanic_origin + age + college + urban + household_composition,
             weights = uniqueBrands$projection_factor)
reg3 <- felm(data = uniqueBrands, channelConc ~ as.factor(household_income) |
               panel_year + market +
               household_size + marital_status + race + type_of_residence +
               hispanic_origin + age + college + urban + household_composition,
             weights = uniqueBrands$projection_factor)
stargazer(reg1, reg2, reg3, type = "text")

# Top brands by income bracket
hhid <- unique(tamponPurch[, .(household_code, panel_year)])
hhid[, "hhid" := paste0(household_code, "_", panel_year)]
hhs <- setDT(expand.grid(hhid = hhid$hhid,
                         brand_code_uc = unique(tamponPurch$brand_code_uc)))
hhs[, c("household_code", "panel_year") := tstrsplit(hhid, "_")]
hhs[, "hhid" := NULL]
hhs[, c("household_code", "panel_year") := .(as.integer(household_code),
                                             as.integer(panel_year))]

topBrands <- tamponPurch[, .(total = sum(size)),
                         by = .(household_code, panel_year, projection_factor,
                                household_income, household_size, marital_status,
                                race, hispanic_origin, market, age, college,
                                urban, type_of_residence, household_composition,
                                brand_code_uc, brand_descr, annualTampon,
                                household_income_coarse)]
apple <- merge(hhs, unique(topBrands[, .(brand_code_uc, brand_descr)]), all.x = TRUE,
               by = c("brand_code_uc"))
apple <- merge(apple, topBrands[, .(household_code, panel_year, brand_code_uc, brand_descr, total)],
               by = c("household_code", "panel_year", "brand_code_uc", "brand_descr"), all.x = TRUE)
apple[is.na(total), "total" := 0]
apple <- merge(apple, unique(topBrands[, .(household_code, panel_year,
                                           projection_factor, household_income,
                                           household_size, marital_status,
                                           race, hispanic_origin, market, age, college,
                                           urban, type_of_residence, household_composition,
                                           household_income_coarse, annualTampon)]),
               by = c("household_code", "panel_year"))

avgBrand <- apple[, .(avg = weighted.mean(total, w = projection_factor),
                      total = weighted.mean(annualTampon, w = projection_factor)),
                  by = .(household_income_coarse, brand_code_uc, brand_descr)]
avgBrand[, "share" := avg / total * 100]
avgBrand[, "household_income_coarse" := factor(household_income_coarse,
                                               levels = c("<25k", "25-50k", "50-100k", ">100k"),
                                               ordered = TRUE)]

setorder(avgBrand, -share)
brands <- avgBrand[, .SD[1:10], by = .(household_income_coarse)]
tampon <- avgBrand[brand_code_uc %in% brands$brand_code_uc]
tampon[brand_code_uc == 5367466901, "brand_descr" := "DISCOUNT LABEL 1"]
tampon[brand_code_uc == 5367466905, "brand_descr" := "DISCOUNT LABEL 2"]
tampon[brand_code_uc == 5367466920, "brand_descr" := "DISCOUNT LABEL 3"]

ggplot(data = tampon, aes(x = brand_descr, y = share)) +
  geom_bar(stat = "identity", aes(fill = household_income_coarse), position = "dodge") +
  theme_fivethirtyeight() +
  coord_flip() +
  labs(x = "Brand",
       y = "Purchase Share",
       fill = "Income",
       title = "Brand Shares by Income",
       caption = "Source: Nielsen Consumer Panel")
ggsave("./figures/brandsTampon.png")
