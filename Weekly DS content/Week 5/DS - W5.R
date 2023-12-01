##%#########################################################################%##
#                                                                             #
#                          Data science W5 (19.10.2023)                       #
#                                                                             #
##%#########################################################################%##

#WD
setwd("~/") #erases previously set WDs
setwd("Personal repo - zmancekpali/Weekly DS content/Week 5") #sets a new one
getwd() #check that it's worked

#Libraries
library(tidyverse)
library(scales)

#Data
LPI_data <- read.csv("LPI_data.csv")

#Wrangling
(LPI_Bar <- LPI.models %>%
  group_by(biome) %>%
  summarise(count = length(unique(id))) %>%
  arrange(desc(count)))

#Plot
biome_colors <- rep("#615523", length(unique(LPI_Bar$biome)))

(LPI_Bargraph <- ggplot(data = LPI_Bar, aes(x = biome, y = count, fill = biome)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = biome_colors) +  # Use a discrete color scale
    theme(plot.background = element_rect(fill = "limegreen"),
          plot.title = element_text(size = 20, color = "orange", face = "bold.italic"),
          plot.subtitle = element_text(size = 10, color = "forestgreen", face = "bold"),
          plot.caption = element_text(size = 15, color = "red", angle = 35),
          panel.background = element_rect(fill = 'darkred', colour = 'lightblue', size = 4),
          panel.border = element_rect(fill = NA, color = "green", size = 2),
          panel.grid.major.x = element_line(color = "yellow", linetype = 2),
          panel.grid.minor.x = element_line(color = "orange", linetype = 3),
          panel.grid.minor.y = element_blank(),
          
          axis.title.y = element_text(face = "bold.italic", color = "orange"),
          axis.title.x = element_text(face = "bold.italic", color = "orange"),
          
          strip.background = element_rect(fill = "#FE1"),
          strip.text.y = element_text(color = "white"),
          strip.placement = "outside",
          
          legend.background = element_rect(fill = "orangered4"), # generally will want to match w plot background
          legend.key = element_rect(fill = "purple"),
          legend.direction = "horizontal",
          legend.position = "bottom",
          legend.justification = "left",
          legend.title = element_text(family = "serif", color = "red"),
          legend.text = element_text(family = "mono", face = "bold.italic", color = "blue")) +
    labs(title = "Try to find the biome",
         subtitle = "Good luck lol",
         caption = "don't cry"))

ggsave(LPI_Bargraph, file = "hanan_hannah_zoja_ugly_graph.jpg", path = "Plots",
       units = "cm", width = 50, height = 30)


