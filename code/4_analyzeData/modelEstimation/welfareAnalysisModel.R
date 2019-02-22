# Numerical exercises for consumer problem
library(nleqslv)
library(purrr)
library(data.table)
library(ggplot2)
library(ggthemes)

# Price function
price <- function(q, eta, pm = 0.5, t = 1) {
  pm + t * q ^ -eta
}

# Expense function
expense <- function(q, params) {
  with(params, {D / q * S + h * q / 2 + D * (pm + t * q ^ -eta)})
}

# Unconstrained quantity choice
rich <- function(q, params) {
  with(params, {h * q ^ 2 / 2 - D * (S + eta * t * q ^ (1 - eta))})
}

# Poor quantity choice
poor <- function(B, params) {
  with(params, {(D * t / (12 * B - D * pm)) ^ (1 / eta)})
}

# Welfare Loss
welfLoss <- function(B, params) {
  richQ <- nleqslv(50, rich, params = params)$x
  richWelf <- expense(richQ, params)
  poorQ <- min(poor(B, params), richQ)
  poorWelf <- expense(poorQ, params)
  ans <- data.table(budget = B,
                    loss = (poorWelf - richWelf) / richWelf * 100,
                    poorSpend = poorWelf,
                    eta = as.character(params$eta),
                    S = as.character(params$S),
                    h = as.character(params$h),
                    D = as.character(params$D))
  return(ans)
}

# Unit pricing under bulk discounts ############################################
x <- seq(1, 30, 0.1)
eta0 <- data.table(x = x, price = price(x, eta = 0), eta = "0")
eta25 <- data.table(x = x, price = price(x, eta = 0.25), eta = "0.25")
eta05 <- data.table(x = x, price = price(x, eta = 0.5), eta = "0.5")
eta10 <- data.table(x = x, price = price(x, eta = 1), eta = "1")
priceData <- rbindlist(list(eta0, eta25, eta05, eta10))
ggplot(data = priceData, aes(x = x, y = price, color = eta)) +
  geom_line() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  ylim(0, 1.5) +
  labs(title = "Unit Price Under Bulk Discounting",
       x = "Quantity",
       y = "Unit Price") +
  scale_color_manual(values = c("0" = "#000000",
                                "0.25" = "#E69F00",
                                "0.5" = "#56B4E9",
                                "1" = "#009E73"))
ggsave(file = "./code/5_figures/bulkDiscountModel.png", width = 6, height = 4, units = "in")

# Comparing various bulk discounts
x <- seq(1, 30, 0.1)
yNoBulk <- rbindlist(map(x, welfLoss, params = list(pm = 0.5, t = 1, eta = 0, S = 1, h = 1, D = 100)))
yLowBulk <- rbindlist(map(x, welfLoss, params = list(pm = 0.5, t = 1, eta = 0.25, S = 1, h = 1, D = 100)))
yMedBulk <- rbindlist(map(x, welfLoss, params = list(pm = 0.5, t = 1, eta = 0.5, S = 1, h = 1, D = 100)))
yHighBulk <- rbindlist(map(x, welfLoss, params = list(pm = 0.5, t = 1, eta = 1, S = 1, h = 1, D = 100)))
y <- rbind(yNoBulk, yLowBulk, yMedBulk, yHighBulk)

ggplot(data = y, aes(x = budget, y = loss, color = eta)) +
  geom_line() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  ylim(0, 50) +
  labs(title = "Budget Constraints Are Costly When Binding",
       subtitle = "No Discount Is Better Than A Little",
       x = "Budget",
       y = "Excess Spending (%)") +
  scale_color_manual(values = c("0" = "#000000",
                                "0.25" = "#E69F00",
                                "0.5" = "#56B4E9",
                                "1" = "#009E73"))
ggsave(filename = "./code/5_figures/bulkDiscountInequality.png", width = 6, height = 4, units = "in")


ggplot(data = y, aes(x = budget, y = poorSpend, color = eta)) +
  geom_line() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  ylim(0, 500) +
  labs(title = "Constrained Households Spend Less Under Bulk Discounts",
       x = "Budget",
       y = "Total Spending") +
  scale_color_manual(values = c("0" = "#000000",
                                "0.25" = "#E69F00",
                                "0.5" = "#56B4E9",
                                "1" = "#009E73"))
ggsave(filename = "./code/5_figures/bulkDiscountPoorSpend.png", width = 6, height = 4, units = "in")

# Comparing shopping costs #####################################################
x <- seq(1, 30, 0.1)
yNoBulk <- rbindlist(map(x, welfLoss, params = list(pm = 0.5, t = 1, eta = 1, S = 0.1, h = 1, D = 100)))
yLowBulk <- rbindlist(map(x, welfLoss, params = list(pm = 0.5, t = 1, eta = 1, S = 0.5, h = 1, D = 100)))
yMedBulk <- rbindlist(map(x, welfLoss, params = list(pm = 0.5, t = 1, eta = 1, S = 1, h = 1, D = 100)))
yHighBulk <- rbindlist(map(x, welfLoss, params = list(pm = 0.5, t = 1, eta = 1, S = 2, h = 1, D = 100)))
yNoBulk0 <- rbindlist(map(x, welfLoss, params = list(pm = 0.5, t = 1, eta = 0, S = 0.1, h = 1, D = 100)))
yLowBulk0 <- rbindlist(map(x, welfLoss, params = list(pm = 0.5, t = 1, eta = 0, S = 0.5, h = 1, D = 100)))
yMedBulk0 <- rbindlist(map(x, welfLoss, params = list(pm = 0.5, t = 1, eta = 0, S = 1, h = 1, D = 100)))
yHighBulk0 <- rbindlist(map(x, welfLoss, params = list(pm = 0.5, t = 1, eta = 0, S = 2, h = 1, D = 100)))
y <- rbind(yNoBulk, yLowBulk, yMedBulk, yHighBulk, yNoBulk0, yLowBulk0, yMedBulk0, yHighBulk0)

ggplot(data = y, aes(x = budget, y = loss, color = S)) +
  geom_line() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  ylim(0, 50) +
  facet_grid(cols = vars(eta)) +
  labs(title = "Shopping Costs Raise Inequality",
       subtitle = "Bulk Discounts Exacerbate Inequality",
       x = "Budget",
       y = "Excess Spending (%)") +
  scale_color_manual(values = c("0.1" = "#000000",
                                "0.5" = "#E69F00",
                                "1" = "#56B4E9",
                                "2" = "#009E73"))
ggsave(filename = "./code/5_figures/bulkDiscountShopping.png", width = 6, height = 4, units = "in")

ggplot(data = y, aes(x = budget, y = poorSpend, color = S)) +
  geom_line() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  ylim(0, 500) +
  facet_grid(cols = vars(eta)) +
  labs(title = "Shopping Costs Raise Overall Spending",
       subtitle = "Bulk Discounts Can Reduce Total Spending",
       x = "Budget",
       y = "Total Spending") +
  scale_color_manual(values = c("0.1" = "#000000",
                                "0.5" = "#E69F00",
                                "1" = "#56B4E9",
                                "2" = "#009E73"))
ggsave(filename = "./code/5_figures/bulkDiscountShoppingPoorSpend.png", width = 6, height = 4, units = "in")

# Comparing inventory costs ####################################################
x <- seq(1, 30, 0.1)
yNoBulk <- rbindlist(map(x, welfLoss, params = list(pm = 0.5, t = 1, eta = 1, S = 1, h = 0.1, D = 100)))
yLowBulk <- rbindlist(map(x, welfLoss, params = list(pm = 0.5, t = 1, eta = 1, S = 1, h = 0.5, D = 100)))
yMedBulk <- rbindlist(map(x, welfLoss, params = list(pm = 0.5, t = 1, eta = 1, S = 1, h = 1, D = 100)))
yHighBulk <- rbindlist(map(x, welfLoss, params = list(pm = 0.5, t = 1, eta = 1, S = 1, h = 2, D = 100)))
yNoBulk0 <- rbindlist(map(x, welfLoss, params = list(pm = 0.5, t = 1, eta = 0, S = 1, h = 0.1, D = 100)))
yLowBulk0 <- rbindlist(map(x, welfLoss, params = list(pm = 0.5, t = 1, eta = 0, S = 1, h = 0.5, D = 100)))
yMedBulk0 <- rbindlist(map(x, welfLoss, params = list(pm = 0.5, t = 1, eta = 0, S = 1, h = 1, D = 100)))
yHighBulk0 <- rbindlist(map(x, welfLoss, params = list(pm = 0.5, t = 1, eta = 0, S = 1, h = 2, D = 100)))
y <- rbind(yNoBulk, yLowBulk, yMedBulk, yHighBulk, yNoBulk0, yLowBulk0, yMedBulk0, yHighBulk0)

ggplot(data = y, aes(x = budget, y = loss, color = h)) +
  geom_line() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  ylim(0, 50) +
  facet_grid(cols = vars(eta)) +
  labs(title = "Inventory Costs Reduce Inequality",
       subtitle = "Bulk Discounts Increase Inequality",
       x = "Budget",
       y = "Excess Spending (%)") +
  scale_color_manual(values = c("0.1" = "#000000",
                                "0.5" = "#E69F00",
                                "1" = "#56B4E9",
                                "2" = "#009E73"))
ggsave(filename = "./code/5_figures/bulkDiscountInventory.png", width = 6, height = 4, units = "in")

# Comparing annual demands #####################################################
x <- seq(1, 30, 0.1)
yNoBulk <- rbindlist(map(x, welfLoss, params = list(pm = 0.5, t = 1, eta = 1, S = 1, h = 1, D = 10)))
yLowBulk <- rbindlist(map(x, welfLoss, params = list(pm = 0.5, t = 1, eta = 1, S = 1, h = 1, D = 50)))
yMedBulk <- rbindlist(map(x, welfLoss, params = list(pm = 0.5, t = 1, eta = 1, S = 1, h = 1, D = 100)))
yHighBulk <- rbindlist(map(x, welfLoss, params = list(pm = 0.5, t = 1, eta = 1, S = 1, h = 1, D = 200)))
yNoBulk0 <- rbindlist(map(x, welfLoss, params = list(pm = 0.5, t = 1, eta = 0, S = 1, h = 1, D = 10)))
yLowBulk0 <- rbindlist(map(x, welfLoss, params = list(pm = 0.5, t = 1, eta = 0, S = 1, h = 1, D = 50)))
yMedBulk0 <- rbindlist(map(x, welfLoss, params = list(pm = 0.5, t = 1, eta = 0, S = 1, h = 1, D = 100)))
yHighBulk0 <- rbindlist(map(x, welfLoss, params = list(pm = 0.5, t = 1, eta = 0, S = 1, h = 1, D = 200)))
y <- rbind(yNoBulk, yLowBulk, yMedBulk, yHighBulk, yNoBulk0, yLowBulk0, yMedBulk0, yHighBulk0)

ggplot(data = y, aes(x = budget, y = loss, color = D)) +
  geom_line() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  ylim(0, 50) +
  facet_grid(cols = vars(eta)) +
  labs(title = "Demand Increases Inequality",
       subtitle = "Bulk Discounts Make Inequality Worse",
       x = "Budget",
       y = "Excess Spending (%)") +
  scale_color_manual(values = c("10" = "#000000",
                                "50" = "#E69F00",
                                "100" = "#56B4E9",
                                "200" = "#009E73"))
ggsave(filename = "./code/5_figures/bulkDiscountDemand.png", width = 6, height = 4, units = "in")
