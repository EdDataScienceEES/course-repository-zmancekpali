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

species_time_series_counts <- lpi %>%
  filter(Class == "Mammalia", System == "Marine") %>%
  pivot_longer(cols = 34:104, names_to = "Year", values_to = "Abundance") %>%
  group_by(Binomial) %>%
  summarize(TimeSeriesCount = sum(!is.na(Abundance))) %>%
  arrange(desc(TimeSeriesCount))
species_with_most_time_series <- species_time_series_counts$Binomial[1]


#Wrangling
mammals <- lpi %>% 
  filter(Class == "Mammalia",
         System == "Terrestrial",
         Common_name == "Reindeer / Caribou") %>% 
  pivot_longer(cols = 34:104, names_to = "Year", values_to = "Abundance") %>% 
  mutate(Year = as.numeric(sub("^X", "", Year)),
         Abundance = as.numeric(ifelse(Abundance == "NULL", NA, Abundance)))

ggplot(mammals, aes(x = Year, y = Abundance)) +
  geom_line() +
  labs(x = "Year", y = "Abundance") +
  theme_minimal() +
  theme(legend.position = "none")

anova(lm(Abundance ~ Year, data = mammals))

whales <- lpi %>% 
  filter(Class == "Mammalia",
         System == "Marine",
         Common_name == "Blue whale") %>% 
  pivot_longer(cols = 34:104, names_to = "Year", values_to = "Abundance") %>% 
  mutate(Year = as.numeric(sub("^X", "", Year)),
         Abundance = as.numeric(ifelse(Abundance == "NULL", NA, Abundance)))


ggplot(whales, aes(x = Year, y = Abundance, color = Country)) +
  geom_line() +
  labs(x = "Year", y = "Abundance") +
  facet_wrap(~Country, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none")
