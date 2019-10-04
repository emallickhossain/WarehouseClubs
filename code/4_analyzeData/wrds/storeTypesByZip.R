# Makes graph of number of different store types by zip code
library(data.table)
library(ggplot2)
library(ggthemes)

# Loading and combining data
cbp <- fread("./code/0_data/cbp2016.csv")
cbp[, "small" := n1_4 + n5_9 + n10_19 + n20_49]
cbp[, "large" := est - small]
cbp <- cbp[, .(zip_code, naics, est, large, small, year)]
incPop <- fread("./code/0_data/zipIncPop.csv")

# Getting all zips and NAICS to input zeros
allZips <- as.data.table(expand.grid(zip_code = unique(incPop$zip_code),
                                     naics = unique(cbp$naics)))
cbp <- merge(cbp, allZips, all.y = TRUE)
cbp[is.na(est), "est" := 0]
cbp[is.na(year), "year" := 2016]

# Merging data and classifying store types
fullData <- merge(cbp, incPop, by = "zip_code", all.y = TRUE)
fullData[naics == 445110, "store" := "Grocery"]
fullData[naics == 446110, "store" := "Drug"]
fullData[naics == 452910, "store" := "ClubDiscount"]
fullData[naics == 452990, "store" := "Dollar"]
fullData[is.na(large), "large" := 0]
fullData[is.na(small), "small" := 0]
fullDataWide <- dcast(fullData, zip_code + pop + medInc + year ~ store,
                      value.var = c("large", "small"))

# Collapsing into groups like Handbury 2019
fullDataWide[, ':=' (discClub = large_ClubDiscount + small_ClubDiscount,
                     largeGrocer = large_Grocery,
                     smallGrocer = small_Grocery,
                     dollar = large_Dollar + small_Dollar,
                     drug = large_Drug + small_Drug)]
fullDataWide <- fullDataWide[, .(zip_code, pop, medInc, year, discClub,
                                 largeGrocer, smallGrocer, dollar, drug)]

# Cutting income and getting counts per 1000 people
fullDataWide[, "incBin" := cut(medInc, breaks = c(0, 30000,
                                                  seq(40000, 90000, 10000), Inf),
                               labels = c(30, seq(40, 100, 10)))]
fullDataWide[, "incBin" := as.integer(as.character(incBin))]
fullDataWide[, "Discount and Club" := discClub / (pop / 1000)]
fullDataWide[, "Large Grocery" := largeGrocer / (pop / 1000)]
fullDataWide[, "Small Grocery" := smallGrocer / (pop / 1000)]
fullDataWide[, "Dollar" := dollar / (pop / 1000)]
fullDataWide[, "Drug" := drug / (pop / 1000)]

# Tabulating zeros
fullDataWide[, "discClubPresent" := (`Discount and Club` > 0)]
fullDataWide[, "dollarPresent" := (Dollar > 0)]

fullDataWide[, weighted.mean(discClubPresent, w = pop)]
fullDataWide[, weighted.mean(discClubPresent, w = pop), keyby = incBin]
fullDataWide[, weighted.mean(dollarPresent, w = pop)]
fullDataWide[, weighted.mean(dollarPresent, w = pop), keyby = incBin]

# Averaging by income bin and plotting
cols <- c("Discount and Club", "Large Grocery", "Small Grocery", "Dollar", "Drug")

graphData <- fullDataWide[, lapply(.SD, weighted.mean, w = pop),
                          .SDcols = cols, keyby = incBin]
graphDataLong <- melt(graphData, id.vars = "incBin", variable.name = "Store",
                      value.name = "pp")
ggplot(graphDataLong, aes(x = incBin, y = pp)) +
  geom_point(color = "darkblue") +
  geom_hline(yintercept = 0) +
  facet_wrap(vars(Store), scales = "free") +
  labs(x = "ZIP Median Income ($1000)",
       y = "Stores per 1,000 Residents") +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom")
ggsave("./code/5_figures/storeTypesByZipColor.pdf", height = 4, width = 6)
