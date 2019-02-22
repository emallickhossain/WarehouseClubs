# Generates package size distribution
library(data.table)
library(ggplot2)
library(ggridges)
library(ggthemes)
library(lfe)
path <- "/scratch/upenn/hossaine/"

tpPurch <- fread(paste0(path, "3625Purch.csv"))
tpPurch[, c("upc", "upc_ver_uc", "trip_code_uc", "quantity", "product_module_code",
            "upc_descr", "size1_units", "purchase_date") := NULL]

tpPurch[, "household_income_coarse" := factor(household_income_coarse,
                                              levels = c("<25k", "25-50k", "50-100k", ">100k"),
                                              ordered = TRUE)]
ggplot(tpPurch[household_size == "2"], aes(x = size, y = household_income_coarse)) +
  geom_density_ridges(rel_min_height = 0.01, scale = 1) +
  theme_fivethirtyeight()

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
tpPurch[size <= 32, "sizeBin" := "1quart"]
tpPurch[size > 32 & size <= 64, "sizeBin" := "2halfGal"]
tpPurch[size > 64 & size <= 128, "sizeBin" := "3gallon"]
tpPurch[size > 128, "sizeBin" := "4galPlus"]

trans.matrix <- function(X, prob = T) {
  tt <- table( c(X[,-ncol(X)]), c(X[,-1]) )
  if(prob) tt <- tt / rowSums(tt)
  tt
}

getMatrix <- function(hhSize, income, category) {
  matrixSample <- tpPurch[trip <= 50 & household_size == hhSize & household_income_coarse == income]
  tpPurchWide <- dcast(data = matrixSample, household_code ~ trip, value.var = category)
  tpPurchMatrix <- as.matrix(tpPurchWide[, -1])
  transMat <- round(trans.matrix(tpPurchMatrix), digits = 3)
  #transMat <- reorder_cormat(transMat)
  transDT <- setDT(melt(transMat))
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
graphData <- getMatrix("4", "<25k", "sizeBin")
ggplot(data = graphData, aes(x = as.factor(brand1), y = as.factor(brand2), fill = prob)) +
  geom_raster() +
  geom_text(aes(as.factor(brand2), as.factor(brand1), label = prob), color = "black", size = 4) +
  theme_fivethirtyeight()

graphData <- getMatrix("4", ">100k", "sizeBin")
ggplot(data = graphData, aes(x = as.factor(brand1), y = as.factor(brand2), fill = prob)) +
  geom_raster() +
  geom_text(aes(as.factor(brand2), as.factor(brand1), label = prob), color = "black", size = 4) +
  theme_fivethirtyeight()
