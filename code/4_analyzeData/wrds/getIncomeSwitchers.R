# Calculates the number of households that change their income in the sample.
# Good for testing if income changes are actually generating this pattern in bulk buying.
library(data.table)
path <- "/scratch/upenn/hossaine/"

panel <- fread(paste0(path, "fullPanel.csv"))
switchers <- unique(panel[, .(household_code, household_income)])
switchers <- switchers[duplicated(household_code), .(household_code)]
switchers <- panel[household_code %in% switchers$household_code]
uniqueN(switchers$household_code)
