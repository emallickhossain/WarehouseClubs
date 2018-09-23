# Gets warehouse club locations from Infogroup through WRDS
# Warehouse clubs: Costco, Sam's Club, BJ's
library(data.table)
bjs <- c("'B J S WHOLESALE CLUB'", "'B J WHOLESALE CLUB'", "'B J WHOLESALE CLUBS'",
         "'B J''S WHOELSALE CLUB'", "'B J''S WHOLESALE'", "'B J''S WHOLESALE CLUB'",
         "'B J''S WHOLESALE CLUB INC'", "'B J''S WHOLESALE DISTRIBUTION'",
         "'BJ''S WHOLESALE'", "'BJ''S WHOLESALE CLUB'", "'BJ''S WHOLESALE CLUB INC'")
bjsNoQuote <- c("B J S WHOLESALE CLUB", "B J WHOLESALE CLUB", "B J WHOLESALE CLUBS",
                "B J'S WHOELSALE CLUB", "B J'S WHOLESALE", "B J'S WHOLESALE CLUB",
                "B J'S WHOLESALE CLUB INC", "B J'S WHOLESALE DISTRIBUTION",
                "BJ'S WHOLESALE", "BJ'S WHOLESALE CLUB", "BJ'S WHOLESALE CLUB INC")
costco <- c("'COSTCO'", "'COSTCO CORP'", "'COSTCO GARDEN GROVE'",
            "'COSTCO WAREHOUSE'", "'COSTCO WHOLESALE'", "'COSTCO WHOLESALE CORP'",
            "'COSTCO WHOLESALE CORPORATION'")
costcoNoQuote <- c("COSTCO", "COSTCO CORP", "COSTCO GARDEN GROVE", "COSTCO WAREHOUSE",
                   "COSTCO WHOLESALE", "COSTCO WHOLESALE CORP", "COSTCO WHOLESALE CORPORATION")
sams <- c("'SAM ''S CLUB'", "'SAM''S'", "'SAM''S CLU'", "'SAM''S CLUB'",
          "'SAM''S CLUB FOR MEMBERS ONLY'", "'SAM''S CLUB MEMBERS ONLY'",
          "'SAM''S CLUB-MEMBERS ONLY'", "'SAM''S CUB'", "'SAM''S CUB-MEMNERS ONLY'",
          "'SAMS'", "'SAMS CLUB'", "'SAMS CLUB MEMBERS ONLY'", "'SAMS CUB'",
          "'SANMS CLUB-MEMBERS ONLY'", "'SARNM''S CLUB'")
samsNoQuote <- c("SAM 'S CLUB", "SAM'S", "SAM'S CLU", "SAM'S CLUB",
                 "SAM'S CLUB FOR MEMBERS ONLY", "SAM'S CLUB MEMBERS ONLY",
                 "SAM'S CLUB-MEMBERS ONLY", "SAM'S CUB", "SAM'S CUB-MEMNERS ONLY",
                 "SAMS", "SAMS CLUB", "SAMS CLUB MEMBERS ONLY", "SAMS CUB",
                 "SANMS CLUB-MEMBERS ONLY", "SARNM'S CLUB")

# Pulling and appending all demographic data, after adding a year column
res <- dbSendQuery(wrds, paste0("SELECT company, abi, archive_version_year, ",
                                "address_line_1, city, state, zipcode, ",
                                "year_established, latitude, longitude ",
                                "FROM ifgr.business ",
                                "WHERE primary_naics_code = '45291001' ",
                                "AND company IN (",
                                paste(bjs, collapse = ", "),
                                paste(costco, collapse = ", "),
                                paste(sams, collapse = ", "), ")"))
data <- setDT(dbFetch(res, n = -1))
dbClearResult(res)

data[company %in% bjsNoQuote, "company" := "bjs"]
data[company %in% samsNoQuote, "company" := "sams"]
data[company %in% costcoNoQuote, "company" := "costco"]
data[, "open" := 1]
data <- data[archive_version_year >= 2003]
dataWide <- dcast(data, ... ~ archive_version_year, value.var = "open", fill = 0)

# Combining duplicate locations
dataWide <- dataWide[, .(latitude = mean(latitude),
                         longitude = mean(longitude),
                         y2003 = sum(`2003`),
                         y2004 = sum(`2004`),
                         y2005 = sum(`2005`),
                         y2006 = sum(`2006`),
                         y2007 = sum(`2007`),
                         y2008 = sum(`2008`),
                         y2009 = sum(`2009`),
                         y2010 = sum(`2010`),
                         y2011 = sum(`2011`),
                         y2012 = sum(`2012`),
                         y2013 = sum(`2013`),
                         y2014 = sum(`2014`),
                         y2015 = sum(`2015`),
                         y2016 = sum(`2016`),
                         y2017 = sum(`2017`),
                         year_established = min(year_established, na.rm = TRUE)),
                     by = .(company, abi, address_line_1, city, state, zipcode)]
dataWide[is.infinite(year_established), "year_established" := NA]
dataWide[year_established %in% 19:20, "year_established" := NA]

fwrite(dataWide, "./code/0_data/clubLocationsRaw.csv")
