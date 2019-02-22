# This gets the Nielsen panel data
# Drops all students and veterans
# Drops those making less than $10k
library(data.table)
library(purrr)
library(stringr)
library(readxl)
yr <- 2004:2016
path <- "/home/mallick/Desktop/Nielsen/Data/Consumer_Panel_Data_2004-2016/nielsen_extracts/HMS/"

getPanel <- function(yr) {
  panel <- fread(paste0(path, yr, "/Annual_Files/panelists_", yr, ".tsv"),
                 select = c("Household_Cd", "Panel_Year", "Projection_Factor",
                            "Household_Income", "Household_Size", "Type_Of_Residence",
                            "Male_Head_Birth", "Female_Head_Birth",
                            "Female_Head_Education", "Male_Head_Education",
                            "Male_Head_Occupation", "Female_Head_Occupation",
                            "Marital_Status", "Race", "Hispanic_Origin",
                            "Panelist_ZipCd", "Scantrack_Market_Identifier_Desc",
                            "Fips_State_Cd", "Fips_County_Cd",
                            "Household_Composition", "Age_And_Presence_Of_Children"))
  setnames(panel, tolower(names(panel)))
  panel[, "fips" := paste0(str_pad(fips_state_cd, 2, "left", "0"),
                           str_pad(fips_county_cd, 3, "left", "0"))]
  panel[, c("fips_state_cd", "fips_county_cd") := NULL]
  setnames(panel, c("household_cd", "scantrack_market_identifier_desc", "panelist_zipcd"),
           c("household_code", "market", "zip_code"))
  panel <- panel[!male_head_occupation %in% c(7, 10)]
  panel <- panel[!female_head_occupation %in% c(7, 10)]
  panel <- panel[household_income > 6]
  return(panel)
}
panel <- rbindlist(map(yr, getPanel))

# Adding income factors
panel[, "household_income_coarse" := cut(household_income, c(0, 13, 19, 26, 30),
                                         labels = c("<25k", "25-50k", "50-100k", ">100k"),
                                         ordered_result = TRUE)]

panel[, "household_income" := ifelse(household_income >= 27, 27, household_income)]

# Adding household size factors
panel[, "household_size" := cut(household_size, c(0, 1, 2, 3, 4, 9),
                                labels = c("1", "2", "3", "4", "5+"),
                                ordered_result = TRUE)]

# Adding age factors
panel[, "age" := panel_year - (female_head_birth + male_head_birth) / 2]
panel[is.na(age), "age" := as.numeric(panel_year - female_head_birth)]
panel[is.na(age), "age" := as.numeric(panel_year - male_head_birth)]
panel[, "age" := cut(age, c(0, 44, 64, 150),
                     labels = c("<45", "45-64", "65+"),
                     ordered_result = TRUE)]

# Adding college indicator if at least 1 HoH has graduated college
panel[, "college" := 0]
panel[female_head_education >= 5 | male_head_education >= 5, "college" := 1]

# Add urban-rural classification
download.file("https://www.ers.usda.gov/webdocs/DataFiles/53251/ruralurbancodes2013.xls?v=0",
              destfile = "./code/0_data/ruralUrban.xls")
ruralUrban <- setDT(read_xls("./code/0_data/ruralUrban.xls"))[, .(FIPS, RUCC_2013)]
ruralUrban[, c("urban", "RUCC_2013") := .(ifelse(RUCC_2013 <= 3, 1L, 0L), NULL)]
setnames(ruralUrban, "FIPS", "fips")
panel <- merge(panel, ruralUrban, by = "fips")
system("rm ./code/0_data/ruralUrban.xls")

# Getting income switchers
switchers <- unique(panel[, .(household_code, household_income)])
switchers <- switchers[duplicated(household_code), .(household_code)]
panel[, "switchers" := ifelse(household_code %in% switchers$household_code, 1L, 0L)]

# Getting income switchers (coarse)
switchers_coarse <- unique(panel[, .(household_code, household_income_coarse)])
switchers_coarse <- switchers_coarse[duplicated(household_code), .(household_code)]
panel[, "switchers_coarse" := ifelse(household_code %in% switchers_coarse$household_code, 1L, 0L)]

# Adding in lat and lon
zipLatLon <- fread("./code/0_data/zipLatLon.csv")
panel <- merge(panel, zipLatLon, by = "zip_code", all.x = TRUE)
panel[, "zip_code" := str_pad(zip_code, 5, "left", "0")]
panel[, "state" := substr(fips, 1, 2)]

# Add PUMA Code
puma <- fread("./code/0_data/PumaZIP.csv")[-1, .(state, zcta5, puma12, afact)]
puma <- puma[zcta5 != 99999]
puma <- puma[puma[, .I[which.max(afact)], by = .(state, zcta5)]$V1]
puma[, "afact" := NULL]
setnames(puma, "zcta5", "zip_code")
panel <- merge(panel, puma, by = c("state", "zip_code"))

# Adding car ownership
own <- fread("./code/0_data/carOwnership.csv")
own[, ':=' (state = str_pad(state, 2, "left", "0"),
            puma12 = str_pad(puma12, 5, "left", "0"))]
panel <- merge(panel, own, by = c("panel_year", "state", "puma12"), all.x = TRUE)

# Making race binary
panel[, "white" := ifelse(race == 1, 1L, 0L)]

# Adjusting type of home to be single-family home, mobile home, and other (likely apt)
panel <- panel[!is.na(type_of_residence)]
panel[, "type_of_residence" := cut(type_of_residence, c(0, 1, 6, 10),
                                   labels = c("Home", "Apt", "Mobile"))]

# Adding child indicator
panel[, "child" := ifelse(age_and_presence_of_children == 9, 0L, 1L)]

# Add Unit pricing indicators
nist <- fread("./code/0_data/nist130.csv")
fips <- unique(as.data.table(maps::state.fips)[, .(fips, abb)])
setnames(fips, c("fips", "state"))
nist <- merge(nist, fips, by = "state")
nist[, "fips" := str_pad(fips, 2, "left", "0")]
nist[, "state" := NULL]
setnames(nist, "fips", "state")
nistLong <- melt(nist, measure.vars = patterns("^law"), value.name = "law", variable.name = "panel_year")
nistLong[, "panel_year" := as.integer(gsub("law", "", panel_year))]
panel <- merge(panel, nistLong, by = c("state", "panel_year"))

# Housekeeping
panel[, c("race", "age_and_presence_of_children", "male_head_birth", "female_head_birth", "female_head_education", "male_head_education") := NULL]
fwrite(panel, "/home/mallick/Desktop/Nielsen/Data/Clean/fullPanel.csv")
