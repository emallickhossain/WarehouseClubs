# Makes map of unit price laws
# Examines effects of unit price regulations on bulk buying
library(data.table)
library(ggplot2)
library(ggthemes)
library(maps)

# Making map of unit price laws in 2017
states_map <- setDT(map_data("state"))
graphData <- fread("./code/0_data/nist130.csv",
                   select = c("state", "type", "displayRequirements"))
graphData[type == "", "reg" := "No Reg"]
graphData[type == "voluntary", "reg" := "Vol. Disp"]
graphData[type == "mandatory" & displayRequirements == "No", "reg" := "Mand. Disp"]
graphData[type == "mandatory" & displayRequirements == "Yes", "reg" := "Mand. Disp, Strict"]
graphData[, "reg" := factor(reg, levels = c("Mand. Disp, Strict", "Mand. Disp",
                                            "Vol. Disp", "No Reg"),
                            ordered = TRUE)]
stateRef <- data.table(state = state.abb, region = tolower(state.name))
graphData <- merge(graphData, stateRef, by = "state")

states_map <- merge(states_map, graphData, by = "region")

ggplot(states_map, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = reg), color = "gray") +
  labs(fill = "Regulation Status") +
  theme_tufte() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom") +
  # scale_fill_grey(start = 0.5, end = 0.95)
  scale_fill_brewer(direction = -1)
ggsave(filename = "./code/5_figures/unitPriceLaws.pdf", height = 4, width = 6)
