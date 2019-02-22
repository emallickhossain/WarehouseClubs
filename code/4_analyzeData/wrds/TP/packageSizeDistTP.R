# Generates package size distribution
library(data.table)
library(ggplot2)
library(ggridges)
library(ggthemes)
library(lfe)
path <- "/scratch/upenn/hossaine/"

tpPurch <- fread(paste0(path, "7260Purch.csv"))
tpPurch[, c("upc", "upc_ver_uc", "trip_code_uc", "quantity", "product_module_code",
            "upc_descr", "size1_units", "purchase_date") := NULL]

tpPurch[, "household_income_coarse" := factor(household_income_coarse,
                                                  levels = c("<25k", "25-50k", "50-100k", ">100k"),
                                                  ordered = TRUE)]
ggplot(tpPurch[household_size == "2"], aes(x = size, y = household_income_coarse)) +
  geom_density_ridges(rel_min_height = 0.01, scale = 1) +
  theme_fivethirtyeight()

# Coefficient of variation within households
coefVar <- tpPurch[, .(meanSize = mean(size),
                       sdSize = sd(size),
                       meanUnit = mean(unitCost),
                       sdUnit = sd(unitCost)),
                       by = .(household_code, panel_year, projection_factor,
                              household_income, household_size, type_of_residence,
                              marital_status, white, hispanic_origin,
                              market, household_composition, household_income_coarse,
                              age, college, urban)]
coefVar[, ':=' (cvSize = sdSize / meanSize,
                cvUnit = sdUnit / meanUnit)]
ggplot(coefVar, aes(cvUnit, stat(density), color = household_income_coarse)) +
  geom_freqpoly(aes(linetype = household_income_coarse))
reg <- felm(data = coefVar, cv ~ factor(household_income_coarse, ordered = FALSE) |
              panel_year + market + household_size + type_of_residence +
              marital_status + white + hispanic_origin + age + urban +
              college + household_composition | 0 | market,
            weights = coefVar$projection_factor)
summary(reg)


# Plot purchase sizes for various households
tpPurch[, "trip" := 1:.N, by = household_code]
ggplot(data = tpPurch[household_code == 2000649],
       aes(x = trip, y = sizeUnadj, color = as.character(household_code))) +
  geom_line() +
  geom_line(aes(y = unitCost, color = "black")) +
  theme_fivethirtyeight() +
  theme(legend.position = "none")

# Tallying deviations from the mode purchase
tpPurch[, "mode" := as.numeric(names(table(sizeUnadj))[which.max(table(sizeUnadj))])]
tpPurch[, "dev" := ifelse(mode == sizeUnadj, 0L, 1L)]
totalDevs <- tpPurch[, .(totalDevs = sum(dev) / .N * 100,
                         mode = mode),
                     by = .(household_code, panel_year, projection_factor,
                            household_income, household_size, type_of_residence,
                            marital_status, white, hispanic_origin, market,
                            household_composition, household_income_coarse,
                            age, college, urban)]
reg <- felm(data = totalDevs, totalDevs ~ factor(household_income_coarse, ordered = FALSE) |
              panel_year + market + household_size + type_of_residence +
              marital_status + white + hispanic_origin + age + urban +
              college + household_composition | 0 | market,
            weights = totalDevs$projection_factor)

# Transition matrix between package sizes??
tpPurch[, "sizeUnadjBin" := cut(sizeUnadj, c(0, 4, 6, 8, 12, 24, 100), c(4, 6, 8, 12, 24, 100))]
tpPurch[, "sizeBin" := cut(size, c(0, 4, 6, 8, 12, 24, 100), c(4, 6, 8, 12, 24, 100))]
tpPurch[, "unitCostBin" := cut(unitCost, c(0, seq(0.2, 1, by = 0.1), 10), c(seq(0.2, 1, by = 0.1), 10))]

tpPurch[brand_descr %in% c("CHARMIN", "CHARIN BASIC", "CHARMIN ESSENTIALS",
                           "CHARMIN PLUS", "CHARMIN SCENTS", "CHARMIN SENSITIVE",
                           "CHARMIN ULTRA SCENTS"), "brandBin" := "CHARMIN"]
tpPurch[brand_descr %in% c("COTTONELLE", "KLEENEX COTTONELLE"), "brandBin" := "COTTONELLE"]
tpPurch[brand_descr %in% c("CTL BR"), "brandBin" := "CTL BR"]
tpPurch[brand_descr %in% c("MARCAL", "MARCAL PRIDE", "MARCAL SMALL STEPS",
                           "MARCAL SOFPAC", "SOFPAC BY MARCAL"), "brandBin" := "MARCAL"]
tpPurch[brand_descr %in% c("QUILTED NORTHERN"), "brandBin" := "QLTD NTN"]
tpPurch[brand_descr %in% c("SCOTT", "SCOTT 1000", "SCOTT EXTRA SOFT",
                           "SCOTT NATURALS", "SCOTT RAPID DISSOLVING"), "brandBin" := "SCOTT"]
tpPurch[is.na(brandBin), "brandBin" := "OTHER"]

tpPurch[, "brandSizeBin" := paste0(brandBin, sizeBin)]
tpPurch[, "sizeBrandBin" := paste0(sizeBin, brandBin)]

trans.matrix <- function(X, prob = T) {
  tt <- table( c(X[,-ncol(X)]), c(X[,-1]) )
  if(prob) tt <- tt / rowSums(tt)
  tt
}

getMatrix <- function(hhSize, income, category, prob = TRUE) {
  matrixSample <- tpPurch[trip <= 100 & household_income_coarse == income]
  tpPurchWide <- dcast(data = matrixSample, household_code ~ trip, value.var = category)
  tpPurchMatrix <- as.matrix(tpPurchWide[, -1])
  transMat <- round(trans.matrix(tpPurchMatrix, prob = prob), digits = 3)
  #transMat <- reorder_cormat(transMat)
  transDT <- setDT(melt(transMat))
  setorder(transDT, Var1)
  setnames(transDT, c("brand1", "brand2", "prob"))
  return(transDT)
}

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Plotting transition matrices
# All data together (ignoring size and income bins)
graphData <- getMatrix("4", "<25k", "sizeBrandBin", prob = TRUE)
ggplot(data = graphData, aes(x = brand1, y = brand2, fill = prob)) +
  geom_raster() +
  #geom_text(aes(brand2, brand1, label = prob), color = "black", size = 4) +
  theme_fivethirtyeight()
ggsave(filename = "./figures/tpSizeBrandTransitionNoText.png")

graphData <- getMatrix("4", ">100k", "brandSizeBin", prob = TRUE)
ggplot(data = graphData, aes(x = brand1, y = brand2, fill = prob)) +
  geom_raster() +
  #geom_text(aes(brand2, brand1, label = prob), color = "black", size = 4) +
  theme_fivethirtyeight()
ggsave(filename = "./figures/tpBrandSizeTransitionNoText.png")

# Adding income bins
graphData <- getMatrix("4", "<25k", "sizeBrandBin", prob = TRUE)
ggplot(data = graphData, aes(x = brand1, y = brand2, fill = prob)) +
  geom_raster() +
  #geom_text(aes(brand2, brand1, label = prob), color = "black", size = 4) +
  theme_fivethirtyeight()
ggsave(filename = "./figures/tpSizeBrandTransitionNoTextPoor.png")

graphData <- getMatrix("4", "<25k", "brandSizeBin", prob = TRUE)
ggplot(data = graphData, aes(x = brand1, y = brand2, fill = prob)) +
  geom_raster() +
  #geom_text(aes(brand2, brand1, label = prob), color = "black", size = 4) +
  theme_fivethirtyeight()
ggsave(filename = "./figures/tpBrandSizeTransitionNoTextPoor.png")

graphData <- getMatrix("4", ">100k", "sizeBrandBin", prob = TRUE)
ggplot(data = graphData, aes(x = brand1, y = brand2, fill = prob)) +
  geom_raster() +
  #geom_text(aes(brand2, brand1, label = prob), color = "black", size = 4) +
  theme_fivethirtyeight()
ggsave(filename = "./figures/tpSizeBrandTransitionNoTextRich.png")

graphData <- getMatrix("4", ">100k", "brandSizeBin", prob = TRUE)
ggplot(data = graphData, aes(x = brand1, y = brand2, fill = prob)) +
  geom_raster() +
  #geom_text(aes(brand2, brand1, label = prob), color = "black", size = 4) +
  theme_fivethirtyeight()
ggsave(filename = "./figures/tpBrandSizeTransitionNoTextRich.png")




graphData <- getMatrix("4", "<25k", "unitCostBin", prob = TRUE)
ggplot(data = graphData, aes(x = as.factor(brand1), y = as.factor(brand2), fill = prob)) +
  geom_raster() +
  geom_text(aes(as.factor(brand2), as.factor(brand1), label = prob), color = "black", size = 4) +
  theme_fivethirtyeight()

graphData <- getMatrix("4", ">100k", "unitCostBin", prob = TRUE)
ggplot(data = graphData, aes(x = as.factor(brand1), y = as.factor(brand2), fill = prob)) +
  geom_raster() +
  geom_text(aes(as.factor(brand2), as.factor(brand1), label = prob), color = "black", size = 4) +
  theme_fivethirtyeight()

apple <- density(tpPurch[type_of_residence == "Mobile" & household_income_coarse == "<25k"]$size)
appleDT <- data.table(x = apple$x, y = apple$y)
pear <- density(tpPurch[type_of_residence == "Mobile" & household_income_coarse == ">100k"]$size)
pearDT <- data.table(x = pear$x, y = pear$y)
ggplot(data = appleDT, aes(x = x, y = y)) +
  geom_line() +
  geom_line(data = pearDT, aes(x = x, y = y, color = "black"))
