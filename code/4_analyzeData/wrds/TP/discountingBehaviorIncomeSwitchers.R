# Gets discounting behavior for income switchers
# Uses output from discountingBehavior.R
library(data.table)
library(ggplot2)
library(ggthemes)
library(ggridges)
library(lfe)
library(purrr)
threads <- 8

# Adding demographics
discBehaviorAll <- fread("/scratch/upenn/hossaine/discBehaviorAll.csv", nThread = threads)
discBehaviorStorage <- fread("/scratch/upenn/hossaine/discBehaviorStorage.csv", nThread = threads)

panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("panel_year", "household_code", "projection_factor",
                          "household_income", "household_size", "age", "child",
                          "dma_cd", "household_income_coarse", "type_of_residence"),
               key = c("household_code", "panel_year"))
panel[, "household_income" := as.factor(household_income)]

discBehaviorAll <- merge(discBehaviorAll, panel, by = c("household_code", "panel_year"))
discBehaviorStorage <- merge(discBehaviorStorage, panel, by = c("household_code", "panel_year"))

# Summary stats of switchers
switchers <- panel[, uniqueN(household_income), by = household_code]
round(prop.table(table(switchers$V1)), digits = 2)

# Adding indicator for switchers
switchID <- switchers[V1 > 1]$household_code
discBehaviorAll[household_code %in% switchID, "switch" := 1L]
discBehaviorAll[is.na(switch), "switch" := 0L]
discBehaviorStorage[household_code %in% switchID, "switch" := 1L]
discBehaviorStorage[is.na(switch), "switch" := 0L]

# Running regressions
# If you add an interaction with panel year, you can test if there have been
# different trends over time, but nothing seemed significant or big enough
# to change the overall picture of increasing bulk with income.
runRegAll <- function(y) {
  regForm <- as.formula(paste0(y, "~", "household_income_coarse + age + ",
                               "household_size + child + type_of_residence | ",
                               "household_code + panel_year + dma_cd"))

  # All products
  regData <- discBehaviorAll[switch == 1]
  regAll <- felm(data = regData, formula = regForm, weights = regData$projection_factor)
  coefsAll <- as.data.table(summary(regAll)$coefficients, keep.rownames = TRUE)
  coefsAll[, c("discount", "reg") := .(y, "All")]

  # Storables
  regData <- discBehaviorStorage[storable == 1 & switch == 1]
  regStorable <- felm(data = regData, formula = regForm, weights = regData$projection_factor)
  coefsStorable <- as.data.table(summary(regStorable)$coefficients, keep.rownames = TRUE)
  coefsStorable[, c("discount", "reg") := .(y, "Non-Perishable")]

  # Non-Storables
  regData <- discBehaviorStorage[storable == 0 & switch == 1]
  regNonStorable <- felm(data = regData, formula = regForm, weights = regData$projection_factor)
  coefsNonStorable <- as.data.table(summary(regNonStorable)$coefficients, keep.rownames = TRUE)
  coefsNonStorable[, c("discount", "reg") := .(y, "Perishable")]

  # Combining
  coefs <- rbindlist(list(coefsAll, coefsStorable, coefsNonStorable), use.names = TRUE)
  return(coefs)
}

discounts <- c("coupon", "generic", "bulk")
finalCoefs <- rbindlist(map(discounts, runRegAll), use.names = TRUE)

# Organizing graph data
graphData <- finalCoefs[grepl("household_income", rn)]
graphData[, "rn" := gsub("household_income", "", rn)]
graphData[, "rn" := factor(rn, levels = c(8, 10, 11, 13, 15, 16, 17, 18, 19,
                                          21, 23, 26, 27),
                           labels = c(11, 13.5, 17.5, 22.5, 27.5, 32.5, 37.5,
                                      42.5, 47.5, 55, 65, 85, 100),
                           ordered = TRUE)]
graphData[, "rn" := as.numeric(as.character(rn))]
setnames(graphData, c("rn", "beta", "se", "t", "p", "disc", "Product Type"))

# Housekeeping
graphData[disc == "coupon", "disc" := "Coupon"]
graphData[disc == "bulk", "disc" := "Bulk"]
graphData[disc == "generic", "disc" := "Generic"]

# Graphing
ggplot(data = graphData[disc == "Bulk"], aes(x = rn, y = beta, color = `Product Type`)) +
  geom_errorbar(aes(ymin = beta - 1.96 * se, ymax = beta + 1.96 * se), width = 0.2) +
  geom_point() +
  geom_vline(xintercept = 0) +
  facet_wrap(vars(disc), scales = "fixed") +
  labs(title = "Rich Households Bulk Buy More",
       x = "Household Income", y = "Share of Purchases",
       caption = paste0("Source: Author calulations from Nielsen Consumer Panel. \n",
                        "Note: Demographic adjustments control for household size, \n",
                        "age, and presence of children.")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0)) +
  scale_color_grey()

############# Same analysis by module ##########################################
# Adding demographics
discBehaviorModule <- fread("/scratch/upenn/hossaine/discBehaviorModule.csv",
                            nThread = threads)
panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("panel_year", "household_code", "projection_factor",
                          "household_income", "household_size", "age", "child",
                          "dma_cd", "household_income_coarse"),
               key = c("household_code", "panel_year"))
panel[, "household_income" := as.factor(household_income)]

discBehaviorModule <- merge(discBehaviorModule, panel,
                            by = c("household_code", "panel_year"))

# Running regressions
# If you add an interaction with panel year, you can test if there have been
# different trends over time, but nothing seemed significant or big enough
# to change the overall picture of increasing bulk with income.
runRegAll <- function(y, module) {
  regForm <- as.formula(paste0(y, "~", "household_income + age + ",
                               "household_size + child | ",
                               "household_code + panel_year + dma_cd"))

  # All products
  regData <- discBehaviorModule[product_module_code == module]
  regAll <- felm(data = regData, formula = regForm, weights = regData$projection_factor)
  coefsAll <- as.data.table(summary(regAll)$coefficients, keep.rownames = TRUE)
  coefsAll[, c("discount", "module") := .(y, module)]
  return(coefsAll)
}

discounts <- c("coupon", "generic", "bulk")
modules <- c(7260, 7734, 8444, 7270, 3625)
toRun <- expand.grid(y = discounts, module = modules)
finalCoefs <- rbindlist(map2(toRun$y, toRun$module, runRegAll), use.names = TRUE)

# Adding in module names
finalCoefs[module == 7260, "Product" := "Toilet Paper"]
finalCoefs[module == 7734, "Product" := "Paper Towels"]
finalCoefs[module == 8444, "Product" := "Diapers"]
finalCoefs[module == 7270, "Product" := "Tampons"]
finalCoefs[module == 3625, "Product" := "Milk"]

# Organizing graph data
graphData <- finalCoefs[grepl("household_income", rn)]
graphData[, "rn" := gsub("household_income", "", rn)]
graphData[, "rn" := factor(rn, levels = c(8, 10, 11, 13, 15, 16, 17, 18, 19,
                                          21, 23, 26, 27),
                           labels = c(11, 13.5, 17.5, 22.5, 27.5, 32.5, 37.5,
                                      42.5, 47.5, 55, 65, 85, 100),
                           ordered = TRUE)]
graphData[, "rn" := as.numeric(as.character(rn))]
setnames(graphData, c("rn", "beta", "se", "t", "p", "disc", "module", "Product"))

# Housekeeping
graphData[disc == "coupon", "disc" := "Coupon"]
graphData[disc == "bulk", "disc" := "Bulk"]
graphData[disc == "generic", "disc" := "Generic"]

# Graphing
panels <- c("Coupon", "Generic", "Bulk")
ggplot(data = graphData[disc %in% "Generic"], aes(x = rn, y = beta, color = Product)) +
  geom_errorbar(aes(ymin = beta - 1.96 * se, ymax = beta + 1.96 * se), width = 0.2) +
  geom_point() +
  geom_vline(xintercept = 0) +
  facet_wrap(vars(Product), scales = "fixed") +
  labs(title = "Rich Households Bulk Buy More",
       subtitle = "Storable Necessities Have Larger Disparities",
       x = "Household Income", y = "Share of Purchases",
       caption = paste0("Source: Author calulations from Nielsen Consumer Panel. \n",
                        "Note: Demographic adjustments control for household size, \n",
                        "age, and presence of children.")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0)) +
  scale_color_grey()
