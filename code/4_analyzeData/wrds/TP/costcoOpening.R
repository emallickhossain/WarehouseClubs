# Looks at size differences when warehouse clubs open
library(data.table)
library(geosphere)
library(lfe)
library(stargazer)
path <- "/scratch/upenn/hossaine/"

tpPurch <- fread(paste0(path, "tpPurch.csv"))
tpPurch[, c("upc", "upc_ver_uc", "trip_code_uc", "quantity", "product_module_code",
            "upc_descr", "product_module_descr", "product_group_code",
            "product_group_descr", "department_code", "department_descr",
            "size1_code_uc", "size1_units", "dataset_found_uc",
            "size1_change_flag_uc", "purchase_date") := NULL]
zipLatLon <- fread("./Nielsen/zipLatLon.csv")
clubs <- na.omit(fread("./Nielsen/club_store_openings_by_zip.csv"))
setnames(clubs, "zip", "zip_code")
setkey(clubs, zip_code)
clubs[, "open_club" := as.Date(open_club)]
clubs[, "store" := 1:nrow(clubs)]
clubZips <- clubs[, .(zip_code)]
clubZips <- merge(clubZips, zipLatLon, by = "zip_code", all.x = TRUE)

hhZips <- unique(tpPurch[, .(zip_code)])
hhZips <- merge(hhZips, zipLatLon, by = "zip_code")
distMat <- distm(hhZips[, .(lon, lat)], clubZips[, .(lon, lat)])
closest <- data.table(zip_code = hhZips$zip_code,
                      store = apply(distMat, 1, which.min),
                      dist = apply(distMat, 1, min, na.rm = TRUE))
closest <- merge(closest, clubs, by = "store")[, "zip_code.y" := NULL]
closest[, c("open_year", "open_month") := .(year(open_club), month(open_club))]
setnames(closest, "zip_code.x", "zip_code")

tpPurch <- merge(tpPurch, closest, by = "zip_code")
tpPurch[, "monthsSinceOpen" := ((panel_year - 2004) * 12 + month) - ((open_year - 2004) * 12 + open_month)]

distance <- 30000
tpPurch[, "nearby" := (dist <= distance)]
tpPurch[, "monthsSinceOpen" := monthsSinceOpen * nearby]
tpPurch[, "open" := .(monthsSinceOpen > 0)]

# Getting HH that experienced an opening
openHH <- unique(tpPurch[monthsSinceOpen == 0]$household_code)
openings <- tpPurch[household_code %in% openHH]

reg1 <- felm(data = openings, log(size) ~ open |
               household_code + market | 0 | market,
             weights = openings$projection_factor)

reg2 <- felm(data = openings, log(size) ~ open + household_income |
               household_code + market | 0 | market,
             weights = openings$projection_factor)

reg3 <- felm(data = openings, log(size) ~ open * household_income |
               household_code + market | 0 | market,
             weights = openings$projection_factor)

stargazer(reg1, reg2, reg3, type = "text",
          add.lines = list(c("Household/MSA FE", "Y", "Y", "Y")),
          single.row = TRUE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("Openings"), column.separate = c(3),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          covariate.labels = c("Open", ">100k", "50-100k", "25-50k",
                               "Open:>100k", "Open:50-100k", "Open:25-50k"),
          notes.align = "l",
          notes = c("Standard errors are clustered at the market level.",
                    "Package size is of standardized 250, 2-ply rolls.",
                    "Club openings limited to a 30km radius."),
          order = c(1, 2, 4, 3, 5, 7, 6),
          digits = 2,
          label = "tab:clubOpening",
          title = "Warehouse Club Opening",
          out = "tables/clubOpeningTP.tex")
