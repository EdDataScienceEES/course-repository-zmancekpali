# In class activity for Data Science in EES 2021
# Starter script written by Isla Myers-Smith and Gergana Daskalova
# 21st October 2020 and 20th October 2021

# Instructions ----

# In teams of 2, you are to make a beautiful or an ugly figure (you choose!). 

# Depending on your confidence, make a figure from Challenge Part 1, or Challenge Part 2 of this script
# Here, we're looking at the slopes of population change in the LPI (is the population increasing, decreasing or stable?)
# Part 1 just looks at which biomes we have data for
# Part 2 looks at how slopes vary between biomes

#To make your figure you need to use a pipe and dplyr functions and the ggplot2 package (all found in the TidyVerse). You can also use other functions. 
#Here is a list of the dplyr functions:
# https://dplyr.tidyverse.org/reference/

#And here is a link to a ggplot cheatsheet
# https://rstudio.github.io/cheatsheets/data-visualization.pdf 

#Have a google for fun themes! Or try installing package "ThemePark" - https://github.com/MatthewBJane/ThemePark 

#More on themes: https://riffomonas.org/code_club/2020-05-07-fun-with-themes

# Starter code ----

# Libraries
library(tidyverse)
library(scales)

#WD
setwd("Weekly DS content/Week 5")

# Load Living Planet Data
LPI_data <- read.csv("LPI_data.csv")

# Reshape data into long form
LPI_long <- gather(data = LPI_data, key = "year", value = "pop", 25:69) %>%
  filter(is.finite(pop)) %>%
  group_by(id) %>%
  filter(length(unique(year)) > 5) %>%
  mutate(scalepop = rescale(pop, to = c(-1, 1))) %>%
  drop_na(scalepop) %>%
  ungroup()

str(LPI_long)

# Calculate slopes of population change
LPI.models <- LPI_long %>%
  group_by(biome, Class, id) %>%
  do(mod = lm(scalepop ~ year, data = .)) %>%  # Create a linear model for each group
  mutate(.,
         slope = summary(mod)$coeff[2]) %>%
  ungroup() %>%
  mutate(id = id,
         biome = biome,
         Class = Class)

# You can ignore the warnings, it's just because some populations don't have enough data

# Group activity ----
# Rank all of the biomes from most to least well represented
# with number of populations monitored

### Challenge part 1: Adapt that code to make that first figure more beautiful and save!


(LPI_Bar <- LPI.models %>%
  group_by(biome) %>%
  summarise(count = length(unique(id))) %>%
  arrange(desc(count)))


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

ggsave(LPI_Bargraph, file = "hanan_hannah_zoja_ugly_graph.jpg", units = "cm", width = 50, height = 30)

