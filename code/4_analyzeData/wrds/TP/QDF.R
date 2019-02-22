# Compute Quantity Discount Function for toilet paper
library(data.table)
library(ggplot2)
library(purrr)
library(minpack.lm)
library(ggthemes)
path <- "/scratch/upenn/hossaine/"

tpPurch <- fread(paste0(path, "7260Purch.csv"))

################# NLS VERSION ##################################################
# Making equation partially linear with .lin1 = p_m and .lin2 = T
getParams <- function(brand) {
  print(brand)
  sol <- nls(unitCost ~ cbind(1, size ^ (-eta)), data = tpPurch[brand_code_uc == brand],
             start = list(eta = 0.3), algorithm = "plinear", model = TRUE,
             nls.control(maxiter = 150, tol = 1e-5, minFactor = 1e-8))
  coefs <- coef(sol)
  ans <- data.table(brand_code_uc = brand,
                    eta = coefs["eta"],
                    pm = coefs[".lin1"],
                    t = coefs[".lin2"])
  return(ans)
}

brands <- c(581898, 624459, 506045, 635074, 526996, 526997, 593633)
allCoefs <- rbindlist(map(brands, getParams))
brandKey <- unique(tpPurch[, .(brand_code_uc, brand_descr)])
allCoefs <- merge(allCoefs, brandKey, by = "brand_code_uc")

################# NLS.LM VERSION ##################################################
# Making equation partially linear with .lin1 = p_m and .lin2 = T
getParamsLM <- function(brand) {
  print(brand)
  sol <- nlsLM(unitCost ~ pm + t * size ^ (-eta), data = tpPurch[brand_code_uc == brand],
             start = list(eta = 0.3, pm = 1, t = 1), lower = c(0, 0, 0),
             control = nls.control(maxiter = 150, tol = 1e-5, minFactor = 1e-8))
  coefs <- coef(sol)
  ans <- data.table(brand_code_uc = brand,
                    eta = coefs["eta"],
                    pm = coefs["pm"],
                    t = coefs["t"])
  return(ans)
}

brands <- c(581898, 624459, 506045, 635074, 526996, 526997, 593633)
allCoefsLM <- rbindlist(map(brands, getParamsLM))
brandKey <- unique(tpPurch[, .(brand_code_uc, brand_descr)])
allCoefsLM <- merge(allCoefsLM, brandKey, by = "brand_code_uc")

x <- seq(1, 50, 0.1)
yCharminBasic <- allCoefs[, .(p = pm + t / x ^ eta), by = brand_descr]
yCharminBasic[, "q" := x]
ggplot(data = yCharminBasic, aes(x = q, y = p, color = brand_descr)) +
  geom_line(size = 1) +
  theme_fivethirtyeight() +
  scale_color_brewer(palette = "Set1") +
  theme(axis.title = element_text()) +
  ylim(0, 1.5) +
  labs(title = "Quantity Discounts Vary By Brand",
       x = "Package Size",
       y = "Unit Price ($ per Roll)")
ggsave(filename = "./figures/QDFTP.png", width = 6, height = 4, units = "in")

ggplot(data = tpPurch[brand_descr == "MARCAL"], aes(x = size, y = unitCost, color = "red")) +
  geom_point() +
  geom_line(data = yCharminBasic[brand_descr == "MARCAL"], aes(x = q, y = p, color = "black"), size = 1)
