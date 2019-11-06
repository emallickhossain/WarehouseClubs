# Gets bulk buying changes with housing size
library(data.table)
library(lfe)
library(stargazer)

# Adding demographics
discBehaviorAll <- fread("/scratch/upenn/hossaine/discBehaviorAll.csv", nThread = threads)
discBehaviorFood <- fread("/scratch/upenn/hossaine/discBehaviorFood.csv", nThread = threads)

panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("panel_year", "household_code", "projection_factor",
                          "household_income", "household_size", "age", "child",
                          "dma_cd", "household_income_coarse", "married",
                          "carShare", "law", "zip_code", "college", "men", "women",
                          "nChildren", "type_of_residence"),
               key = c("household_code", "panel_year"))
panel[, "adults" := men + women]
panel[, "household_income" := as.factor(household_income)]
panel[, "household_income_coarse" := as.factor(household_income_coarse)]
panel[, "lawInd" := (law >= 3)]

# Adding lead of income
setorder(panel, household_code, panel_year)
panel[, "leadIncome" := shift(household_income, 1, type = "lead"), by = household_code]

discBehaviorAll <- merge(discBehaviorAll, panel, by = c("household_code", "panel_year"))
discBehaviorFood <- merge(discBehaviorFood, panel, by = c("household_code", "panel_year"))

# Running regression
reg1 <- felm(data = discBehaviorFood[food == 0],
             formula = bulk ~ type_of_residence |
               household_code + panel_year + dma_cd | 0 | household_code,
             weights = discBehaviorFood[food == 0]$projection_factor)
reg2 <- felm(data = discBehaviorFood[food == 0],
             formula = bulk ~ type_of_residence + married + age + adults +
               nChildren + household_income + carShare + college |
               household_code + panel_year + dma_cd | 0 | household_code,
             weights = discBehaviorFood[food == 0]$projection_factor)
reg3 <- felm(data = discBehaviorFood[food == 0],
             formula = bulk ~ type_of_residence + married + age + adults +
               nChildren + household_income + leadIncome + carShare + college |
               household_code + panel_year + dma_cd | 0 | household_code,
             weights = discBehaviorFood[food == 0]$projection_factor)
stargazer(reg1, reg2, reg3, type = "text",
          add.lines = list(c("Market FE's", "Y", "Y", "Y"),
                           c("Year FE's",   "Y", "Y", "Y"),
                           c("Demographics", "N", "Y", "Y"),
                           c("Future Income", "N", "N", "Y")),
          single.row = FALSE, no.space = TRUE, omit.stat = c("ser", "rsq"),
          out.header = FALSE,
          dep.var.caption = "", dep.var.labels.include = FALSE,
          keep = c("type_of_residence"),
          covariate.labels = c("Single-Family Home"),
          notes.align = "l",
          notes.append = TRUE,
          digits = 3,
          label = "tab:storageCosts",
          title = "Relationship Between Bulk Buying and Housing Changes",
          out = "tables/storageCosts.tex")
