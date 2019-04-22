# Getting pulp and paper prices from BLS PPI (sourced from FRED)
# I reindex everything to December 2009.
library(data.table)
library(fredr)
library(purrr)
fredr_set_key(fredAPI)

# Getting pulp prices and reindexing to December 2009
pulp <- setDT(fredr("WPU091105"))[, "series_id" := NULL]
setnames(pulp, "value", "pulp")
baseVal <- pulp[date == "2009-12-01"]$pulp
pulp[, "pulp" := pulp / baseVal * 100]

# Getting tube prices, which are already indexed to Dec-2009
tubes <- setDT(fredr("WPU09150732"))[, "series_id" := NULL]
setnames(tubes, "value", "tubes")

# Merging both series together
paperProd <- merge(pulp, tubes, by = "date", all = TRUE)
fwrite(paperProd, "./code/0_data/paperPrices.csv")
