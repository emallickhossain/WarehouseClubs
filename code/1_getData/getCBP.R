# Gets CBP store data by ZIP code
library(data.table)
naicsCode <- c(445110, 446110, 452910, 452990)

# Downloading CBP data for 2016
fullCBP <- NULL
for (i in 2016) {
  print(i)
  temp <- tempfile()
  download.file(paste0("https://www2.census.gov/programs-surveys/cbp/datasets/",
                       i, "/zbp", substr(i, 3, 4), "detail.zip"), destfile = temp)
  unzip(temp)
  if (i == 2009) {
    cbp <- fread(paste0("./Zbp", substr(i, 3, 4), "detail.txt"))
  } else {
    cbp <- fread(paste0("./zbp", substr(i, 3, 4), "detail.txt"))
  }
  cbp <- cbp[naics %in% naicsCode][, "year" := i]
  fullCBP <- rbindlist(list(fullCBP, cbp), use.names = TRUE)
  system("rm *.txt")
}

setnames(fullCBP, "zip", "zip_code")
fwrite(fullCBP, "./code/0_data/cbp2016.csv")
