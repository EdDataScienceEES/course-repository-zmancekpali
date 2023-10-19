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

# Load Living Planet Data
LPI_data <- read.csv("weekly_materials/week_05/data/LPI_data.csv")

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

LPI_Bar <- LPI.models %>%
  group_by(biome) %>%
  summarise(count = length(unique(id))) %>%
  arrange(desc(count))

LPI_Bargraph <- ggplot(data=LPI_Bar, aes(x=biome, y=count)) +
    geom_bar(stat="identity")

# ggsave(LPI_Bargraph, filename = "weekly_materials/week_05/YouAndYourPartnersNames_UglyOrPretty.pdf", width = 8, height = 5)

### Challenge part 2: Answer the question with your own data wrangling and a beautiful graph and save!

# How are populations changing across the six best monitored biomes?

#Hint, we've got the number of biomes from the above question, just need to filter our model data to just these!

# HINT: You can use facet_wrap() or facet_grid() from the ggplot2 package
# to quickly create a graph with multiple panels

# EITHER Make your figure as beautiful as it can be or as ugly as it can be and save that file

# The group with the prettiest and ugliest figures win the respective "glory"
