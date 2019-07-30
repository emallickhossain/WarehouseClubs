# Gets discounting behavior for income switchers
# Uses output from discountingBehavior.R
library(data.table)
library(ggplot2)
library(ggthemes)
library(ggridges)
library(lfe)
library(purrr)
library(stargazer)
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

# Running regressions
# If you add an interaction with panel year, you can test if there have been
# different trends over time, but nothing seemed significant or big enough
# to change the overall picture of increasing bulk with income.
runRegAll <- function(y) {
  regForm <- as.formula(paste0(y, "~", "household_income + age + ",
                               "household_size + child | ",
                               "household_code + panel_year + dma_cd"))

  # All products
  regData <- discBehaviorAll
  regAll <- felm(data = regData, formula = regForm, weights = regData$projection_factor)
  coefsAll <- as.data.table(summary(regAll)$coefficients, keep.rownames = TRUE)
  coefsAll[, c("discount", "reg") := .(y, "All")]

  # Storables
  regData <- discBehaviorStorage[storable == 1]
  regStorable <- felm(data = regData, formula = regForm, weights = regData$projection_factor)
  coefsStorable <- as.data.table(summary(regStorable)$coefficients, keep.rownames = TRUE)
  coefsStorable[, c("discount", "reg") := .(y, "Non-Perishable")]

  # Non-Storables
  regData <- discBehaviorStorage[storable == 0]
  regNonStorable <- felm(data = regData, formula = regForm, weights = regData$projection_factor)
  coefsNonStorable <- as.data.table(summary(regNonStorable)$coefficients, keep.rownames = TRUE)
  coefsNonStorable[, c("discount", "reg") := .(y, "Perishable")]

  # Combining
  coefs <- rbindlist(list(coefsAll, coefsStorable, coefsNonStorable), use.names = TRUE)
  return(coefs)
}

discounts <- c("coupon", "generic", "bulk")
finalCoefs <- rbindlist(map(discounts, runRegAll), use.names = TRUE)

# Creating regression tables
regForm <- as.formula(paste0("bulk ~ household_income_coarse + age + ",
                             "household_size + child | ",
                             "household_code + panel_year + dma_cd"))
reg1 <- felm(data = discBehaviorAll, formula = regForm,
             weights = discBehaviorAll$projection_factor)
reg2 <- felm(data = discBehaviorStorage[storable == 1],
             formula = regForm,
             weights = discBehaviorStorage[storable == 1]$projection_factor)
reg3 <- felm(data = discBehaviorStorage[storable == 0],
             formula = regForm,
             weights = discBehaviorStorage[storable == 0]$projection_factor)
stargazer(reg1, reg2, reg3, type = "text",
          add.lines = list(c("Household FE", "Y", "Y", "Y"),
                           c("Year FE", "Y", "Y", "Y"),
                           c("Market FE", "Y", "Y", "Y")),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("All", "Non-Perishable", "Perishable"),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          keep = c("household_income*"),
          order = c(2, 3, 1),
          covariate.labels = c("25-50k", "50-100k", ">100k"),
          notes.align = "l",
          digits = 3,
          label = "tab:discountingBehaviorIncomeSwitchers",
          title = "Bulk Discounting When Income Changes",
          out = "tables/discountingBehaviorIncomeSwitchers.tex")

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

# Creating regression tables
regForm <- as.formula(paste0("bulk ~ household_income_coarse + age + ",
                             "household_size + child | ",
                             "household_code + panel_year + dma_cd"))
reg1 <- felm(data = discBehaviorModule[product_module_code == 7260],
             formula = regForm,
             weights = discBehaviorModule[product_module_code == 7260]$projection_factor)
reg2 <- felm(data = discBehaviorModule[product_module_code == 7734],
             formula = regForm,
             weights = discBehaviorModule[product_module_code == 7734]$projection_factor)
reg3 <- felm(data = discBehaviorModule[product_module_code == 3625],
             formula = regForm,
             weights = discBehaviorModule[product_module_code == 3625]$projection_factor)
reg4 <- felm(data = discBehaviorModule[product_module_code == 4100],
             formula = regForm,
             weights = discBehaviorModule[product_module_code == 4100]$projection_factor)

# Looking at individual modules
stargazer(reg1, reg2, reg3, reg4, type = "text",
          add.lines = list(c("Household FE", "Y", "Y", "Y", "Y"),
                           c("Year FE", "Y", "Y", "Y", "Y"),
                           c("Market FE", "Y", "Y", "Y", "Y")),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          column.labels = c("Toilet Paper", "Paper Towels", "Milk", "Eggs"),
          dep.var.caption = "", dep.var.labels.include = FALSE,
          keep = c("household_income*"),
          order = c(2, 3, 1),
          covariate.labels = c("25-50k", "50-100k", ">100k"),
          notes.align = "l",
          digits = 3,
          label = "tab:discountingBehaviorIncomeSwitchers",
          title = "Bulk Discounting When Income Changes")


# Adding in module names
finalCoefs[module == 7260, "Product" := "Toilet Paper"]
finalCoefs[module == 7734, "Product" := "Paper Towels"]
finalCoefs[module == 8444, "Product" := "Diapers"]
finalCoefs[module == 7270, "Product" := "Tampons"]
finalCoefs[module == 3625, "Product" := "Milk"]
finalCoefs[module == 4100, "Product" := "Eggs"]
finalCoefs[, "storable" := ifelse(module %in% c(3625, 4100),
                                  "Perishable", "Non-Perishable")]

# Organizing graph data
graphData <- finalCoefs[grepl("household_income", rn)]
graphData[, "rn" := gsub("household_income", "", rn)]
graphData[, "rn" := factor(rn, levels = c(8, 10, 11, 13, 15, 16, 17, 18, 19,
                                          21, 23, 26, 27),
                           labels = c(11, 13.5, 17.5, 22.5, 27.5, 32.5, 37.5,
                                      42.5, 47.5, 55, 65, 85, 100),
                           ordered = TRUE)]
graphData[, "rn" := as.numeric(as.character(rn))]
setnames(graphData, c("rn", "beta", "se", "t", "p", "disc", "module", "Product", "storable"))

# Housekeeping
graphData[disc == "coupon", "disc" := "Coupon"]
graphData[disc == "bulk", "disc" := "Bulk"]
graphData[disc == "generic", "disc" := "Generic"]

# Graphing
panels <- c("Coupon", "Generic", "Bulk")
ggplot(data = graphData[disc %in% "Bulk"], aes(x = rn, y = beta, color = Product)) +
  geom_errorbar(aes(ymin = beta - 1.96 * se, ymax = beta + 1.96 * se), width = 0.2) +
  geom_point() +
  geom_vline(xintercept = 0) +
  facet_wrap(vars(storable), scales = "fixed") +
  labs(title = "Rich Households Bulk Buy More",
       subtitle = "Storable Necessities Have Larger Disparities",
       x = "Household Income", y = "Share of Purchases",
       caption = paste0("Source: Author calulations from Nielsen Consumer Panel. \n",
                        "Note: Demographic adjustments control for household size, \n",
                        "age, and presence of children.")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0)) +
  scale_color_grey()
