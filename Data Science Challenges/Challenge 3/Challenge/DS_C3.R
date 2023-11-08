##%#########################################################################%##
#                                                                             #
#                           Data science Challenge 3                          #
#                               Zoja Manček Páli                              #
#                                                                             #
##%#########################################################################%##

#If less than 5 levels, the effect must be fixed

#WD
setwd("~/") #erases previously set WDs
setwd("Personal repo - zmancekpali/Data Science Challenges/Challenge 3") #sets WD to this folder
getwd() #check that it's worked

#Libraries
library(tidyverse)

#Data
lpi <- read.csv("Challenge/LPD2022_public.csv")

#Inspection
head(lpi)
str(mammals)


#Wrangling
deer <- lpi %>% 
  filter(Class == "Mammalia",
         System == "Terrestrial",
         Common_name == "Reindeer / Caribou",
         Country == "Canada") %>% 
  pivot_longer(cols = 34:104, names_to = "Year", values_to = "Abundance") %>% 
  mutate(Year = as.numeric(sub("^X", "", Year)),
         Abundance = as.numeric(ifelse(Abundance == "NULL", NA, Abundance)))

ggplot(deer, aes(x = Year, y = Abundance, color = Subspecies)) +
  geom_line() +
  labs(x = "Year", y = "Abundance") +
  theme_minimal()

anova(lm(Abundance ~ Year, data = deer))
