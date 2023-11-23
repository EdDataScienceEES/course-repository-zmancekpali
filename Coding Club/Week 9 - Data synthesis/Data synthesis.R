##%#########################################################################%##
#                                                                             #
#                         Coding club W9 (15.11.2023)                         #
#                                                                             #
##%#########################################################################%##

#The beginning of this tutorial is the same as that in Week 5 so I won't repeat
#that here. 

#Libraries
devtools::install_github("wilkox/treemapify")
library(broom) 
library(ggalt)
library(ggrepel)  
library(ggthemes)
library(sf)
library(terra)
library(tidyverse)
library(treemapify)  
library(viridis)  
library(wesanderson)  

#WD
setwd("~/") #erases previously set WDs
setwd("Personal repo - zmancekpali/Coding Club/Week 9 - Data synthesis") #sets a new one
getwd() #check that it's worked

#Data
bird_pops <- read.csv("bird_pops.csv")
bird_traits <- read.csv("elton_birds.csv")

#Wrangling (mostly all in base R for some reason)
names(bird_pops)
(names(bird_pops) <- tolower(names(bird_pops)))

bird_pops_long <- gather(data = bird_pops, key = "year", value = "pop", 27:71)
head(bird_pops_long)

(bird_pops_long$year <- parse_number(bird_pops_long$year))
bird_pops_long$species.name <- paste(bird_pops_long$genus, bird_pops_long$species, sep = " ")

bird_pops_long <- bird_pops_long %>%
  distinct() %>%
  filter(is.finite(pop)) %>%
  group_by(id) %>%
  mutate(maxyear = max(year), minyear = min(year),
         duration = maxyear - minyear,
         scalepop = (pop - min(pop))/(max(pop) - min(pop))) %>%
  filter(is.finite(scalepop),
         length(unique(year)) > 5) %>%
  ungroup()
head(bird_pops_long)

country_sum <- bird_pops %>% group_by(country.list) %>%
  tally() %>%
  arrange(desc(n))

country_sum[1:15,] # the top 15


aus_pops <- bird_pops_long %>%
  filter(country.list == "Australia") #extract for Australia

aus_pops2 <- bird_pops_long %>%
  filter(str_detect(country.list, pattern = "Australia"))

#Models with dplyr and broom ----
aus_models <- aus_pops %>%
  group_by(decimal.latitude, decimal.longitude, class,
           species.name, id, duration, minyear, maxyear,
           system, common.name) %>%
  do(broom::tidy(lm(scalepop ~ year, .))) %>%
  filter(term == "year") %>%
  dplyr::select(-term) %>%
  ungroup() #how cool
head(aus_models)


#Synthesis from different sources ----
colnames(bird_traits)
(bird_traits <- bird_traits %>% rename(species.name = Scientific)) #doesnt work ??

# Select just the species and their diet
bird_diet <- bird_traits %>% dplyr::select(species.name, `Diet.5Cat`) %>%
  distinct() %>% rename(diet = `Diet.5Cat`)

# Combine the two datasets
# The second data frame will be added to the first one
# based on the species column
bird_models_traits <- left_join(aus_models, bird_diet, by = "species.name") %>%
  drop_na()
head(bird_models_traits)
