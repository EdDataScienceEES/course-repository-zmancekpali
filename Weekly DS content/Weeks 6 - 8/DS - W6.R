##%#########################################################################%##
#                                                                             #
#                 Data science W6 - 8 (26.10.2023 - 9.11.2023)                #
#                                                                             #
##%#########################################################################%##

#Libraries
library(tidyverse)
library(lme4)
library(brms)
library(MCMCglmm)
library(broom)
library(broom.mixed)
library(ggeffects)

#WD
setwd("~/") #erases previously set WDs
setwd("DS - Personal repo - Zoja/Weekly DS content/Weeks 6 - 8") #sets a new one
getwd() #check that it's worked

#Data
# Load data and wrangle #------------------------------------------------------------------------
# Toolik data
toolik_plants <- read_csv("weekly_materials/week_06-08/data/toolik_plants.csv")

# Inspect data
head(toolik_plants)
str(toolik_plants) # What classes are the variables in the dataset?

toolik_plants$Plot <- as.factor(as.character(toolik_plants$Plot))

length(unique(toolik_plants$Site)) # How many sites are the data from?

unique(toolik_plants$Year) # How many years does this data cover?

# Remove non-species
toolik_plants <- toolik_plants %>%
  filter(!Species %in% c("Woody cover", "Tube",
                         "Hole", "Vole trail",
                         "removed", "vole turds",
                         "Mushrooms", "Water",
                         "Caribou poop", "Rocks",
                         "mushroom", "caribou poop",
                         "animal litter", "vole poop",
                         "Vole poop", "Unk?"))

# Calculate species richness
toolik_plants <- toolik_plants %>%
  group_by(Year, Site, Block, Plot) %>%
  summarise(Richness = length(unique(Species))) %>% 
  ungroup()

# Explore the distribution of the data
(hist2 <- ggplot(toolik_plants, aes(x = Richness)) +
    geom_histogram() +
    theme_classic())

#Make a new column with a simplified year variable for use in models (2008 becomes year 1)
toolik_plants$Year_simple <- toolik_plants$Year-2007



# WEEK 6. Linear models #-------------------------------------------------------------------------

## Gaussian family ####
# Richness over time


# Handy way of tidying model output!
(model.table1 <- as.data.frame(tidy(name_of_your_model)))


# QUESTIONS:

# Write a summary sentence of how plant richness has changed over time

# What is the rate of change?

# What is the statistical power of the model? sample size = ? number of plots = ?

# Does the model output change if you run the model again?

# What values should you report?

# What is the error around this model? Are you confident in it?

# How is species richness changing over time?


# PREDICTION:
# What is a model prediction?

# What is the predicted species richness in the year 2013?





## Trying a different family ####
# How did the Gaussian model fit? What is most appropriate distribution for these data? 

# Fit a new model:


# QUESTIONS:

# How has this changed the model diagnostic plots? Does it look better?

# How has this changed the model estimates? (The slope and intercept) 
# Compare these numbers to the numbers from the Gaussian model. Why might there be differences?

# Write a summary sentence of how plant richness has changed over time

# What is the rate of change?

# Which model do you trust more?!


# PREDICTION: 
# What is the predicted species richness in the year 2013?
