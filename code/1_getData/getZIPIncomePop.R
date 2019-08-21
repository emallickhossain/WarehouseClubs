# Gets ZIP-level income and population data from Census for 2016
# Download from data.census.gov
# Median income: S1903 MEDIAN INCOME IN THE PAST 12 MONTHS (IN 2017 INFLATION-ADJUSTED DOLLARS)

inc <- fread(paste0("/home/mallick/Downloads/medInc/",
                    "ACSST5Y2016.S1903_data_with_overlays_2019-08-20T173511.csv"),
             select = c("NAME", "S1903_C02_001E"))[-1]
setnames(inc, c("zip_code", "medInc"))
inc[, "zip_code" := as.integer(gsub("ZCTA5 ", "", zip_code))]
inc[, "medInc" := as.integer(medInc)]

# Population: S0101 AGE and SEX
pop <- fread(paste0("/home/mallick/Downloads/pop/",
                    "ACSST5Y2016.S0101_data_with_overlays_2019-08-20T173915.csv"),
             select = c("NAME", "S0101_C01_001E"))[-1]
setnames(pop, c("zip_code", "pop"))
pop[, "zip_code" := as.integer(gsub("ZCTA5 ", "", zip_code))]
pop[, "pop" := as.integer(pop)]

fullData <- merge(pop, inc, by = "zip_code")
fwrite(fullData, "./code/0_data/zipIncPop.csv")
