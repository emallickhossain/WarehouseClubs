# -------------- Mapbox API ----------------------------------------------------
Sys.setenv("MAPBOX_TOKEN" = "pk.eyJ1IjoiaWNlYnJlYWtlcjQ0NCIsImEiOiJjamZxenR1am0weDl4Mnpvd2IxcHN4b2pyIn0.eGT1aUtpQXDE2wzRy4iULA")

# -------------- Census API ----------------------------------------------------
censusAPI <- "c8b436007c1507dc5709fa1e7a54c116d113cc4a"

# -------------- BLS API -------------------------------------------------------
blsAPI <- "a2b18841aab447c69ca42778bc71004e"

# -------------- BEA API -------------------------------------------------------
beaAPI <- "3A7FBAD5-8651-47C8-95C8-B58C584DA9FD"

# -------------- FRED API -------------------------------------------------------
fredAPI <- "e2b57235e981bb9db50b789104012b3b"

# -------------------- WRDS setup ----------------------------------------------
library(RPostgres)
wrds <- dbConnect(Postgres(),
                  host="wrds-pgdata.wharton.upenn.edu",
                  port=9737,
                  user="hossaine",
                  password="Qazwsxedcf1992",
                  sslmode="require",
                  dbname="wrds")
