##%#########################################################################%##
#                                                                             #
#                         Coding club W9 (15.11.2023)                         #
#                                                                             #
##%#########################################################################%##

#The beginning of this tutorial is the same as that in Week 5 so I won't repeat
#that here. 

#WD
setwd("~/") #erases previously set WDs
setwd("Personal repo - zmancekpali/Coding Club/Week 9 - Data synthesis") #sets a new one
getwd() #check that it's worked


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

#Data
bird_pops <- read.csv("Data/bird_pops.csv")
bird_traits <- read.csv("Data/elton_birds.csv")

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

#Select just the species and their diet
bird_diet <- bird_traits %>% dplyr::select(species.name, `Diet.5Cat`) %>%
  distinct() %>% rename(diet = `Diet.5Cat`)

#Combine the two datasets
#The second data frame will be added to the first one
#based on the species column
bird_models_traits <- left_join(aus_models, bird_diet, by = "species.name") %>%
  drop_na()
head(bird_models_traits)

#Boxplot + jitter plot
(trends_diet <- ggplot(bird_models_traits, aes(x = diet, y = estimate,
                                               colour = diet)) +
    geom_boxplot())

(trends_diet <- ggplot(data = bird_models_traits, aes(x = diet, y = estimate,
                                                      colour = diet)) +
    geom_jitter(size = 3, alpha = 0.3, width = 0.2))

#Sorting the whole data frame by the mean trends
bird_models_traits <- bird_models_traits %>%
  group_by(diet) %>%
  mutate(mean_trend = mean(estimate)) %>%
  ungroup() %>%
  mutate(diet = fct_reorder(diet, -mean_trend))

#Calculating mean trends per diet categories
diet_means <- bird_models_traits %>% group_by(diet) %>%
  summarise(mean_trend = mean(estimate)) %>%
  arrange(mean_trend)

#Plots
(trends_diet <- ggplot() +
    geom_jitter(data = bird_models_traits, aes(x = diet, y = estimate,
                                               colour = diet),
                size = 3, alpha = 0.3, width = 0.2) +
    geom_segment(data = diet_means,aes(x = diet, xend = diet,
                                       y = mean(bird_models_traits$estimate),
                                       yend = mean_trend),
                 size = 0.8) +
    geom_point(data = diet_means, aes(x = diet, y = mean_trend,
                                      fill = diet), size = 5,
               colour = "grey30", shape = 21) +
    geom_hline(yintercept = mean(bird_models_traits$estimate),
               size = 0.8, colour = "grey30") +
    geom_hline(yintercept = 0, linetype = "dotted", colour = "grey30") +
    coord_flip() +
    theme_clean() +
    scale_colour_manual(values = wes_palette("Cavalcanti1")) +
    scale_fill_manual(values = wes_palette("Cavalcanti1")) +
    scale_y_continuous(limits = c(-0.23, 0.23),
                       breaks = c(-0.2, -0.1, 0, 0.1, 0.2),
                       labels = c("-0.2", "-0.1", "0", "0.1", "0.2")) +
    scale_x_discrete(labels = c("Carnivore", "Fruigivore", "Omnivore", "Insectivore", "Herbivore")) +
    labs(x = NULL, y = "\nPopulation trend") +
    guides(colour = FALSE, fill = FALSE))

ggsave("trends_diet.png", trends_diet, path = "Plots", height = 5, width = 8)


diet_sum <- bird_models_traits %>% group_by(diet) %>% tally()
(diet_bar <- ggplot(diet_sum, aes(x = diet, y = n,
                                  colour = diet,
                                  fill = diet)) +
    geom_bar(stat = "identity") +
    scale_colour_manual(values = wes_palette("Cavalcanti1")) +
    scale_fill_manual(values = wes_palette("Cavalcanti1")) +
    guides(colour = FALSE))

(diet_area <- ggplot(diet_sum, aes(area = n, fill = diet, label = n,
                                   subgroup = diet)) +
    geom_treemap() +
    geom_treemap_subgroup_border(colour = "white", size = 1) +
    geom_treemap_text(colour = "white", place = "center", reflow = T) +
    scale_colour_manual(values = wes_palette("Cavalcanti1")) +
    scale_fill_manual(values = wes_palette("Cavalcanti1")) +
    guides(fill = FALSE))  #this removes the colour legend

ggsave("diet_area.png", diet_area, path = "Plots", height = 5, width = 8)


#Timelines
bird_models_traits$id <- as.factor(as.character(bird_models_traits$id))

(timeline_aus <- ggplot() +
    geom_linerange(data = bird_models_traits, aes(ymin = minyear, ymax = maxyear,
                                                  colour = diet,
                                                  x = id),
                   size = 1) +
    scale_colour_manual(values = wes_palette("Cavalcanti1")) +
    labs(x = NULL, y = NULL) +
    theme_bw() +
    coord_flip())

ggsave("timeline_aus.jpg", timeline_aus, path = "Plots", height = 5, width = 8)

#Create a sorting variable
bird_models_traits$sort <- bird_models_traits$diet
bird_models_traits$sort <- factor(bird_models_traits$sort, levels = c("VertFishScav",
                                                                      "FruiNect",
                                                                      "Omnivore",
                                                                      "Invertebrate",
                                                                      "PlantSeed"),
                                  labels = c(1, 2, 3, 4, 5))

bird_models_traits$sort <- paste0(bird_models_traits$sort, bird_models_traits$minyear)
bird_models_traits$sort <- as.numeric(as.character(bird_models_traits$sort))

(timeline_aus <- ggplot() +
    geom_linerange(data = bird_models_traits, aes(ymin = minyear, ymax = maxyear,
                                                  colour = diet,
                                                  x = fct_reorder(id, desc(sort))),
                   size = 1) +
    scale_colour_manual(values = wes_palette("Cavalcanti1")) +
    labs(x = NULL, y = NULL) +
    theme_bw() +
    coord_flip() +
    guides(colour = F) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(),
          axis.ticks = element_blank(),
          legend.position = "bottom",
          panel.border = element_blank(),
          legend.title = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.title = element_text(size = 20, vjust = 1, hjust = 0),
          axis.text = element_text(size = 16),
          axis.title = element_text(size = 20)))

ggsave("timeline_aus.jpg", timeline_aus, path = "Plots", height = 5, width = 8)


#Combining the datasets
mass <- bird_traits %>% dplyr::select(species.name, BodyMass.Value) %>%
  rename(mass = BodyMass.Value)
bird_models_mass <- left_join(aus_models, mass, by = "species.name") %>%
  drop_na(mass)
head(bird_models_mass)

(trends_mass <- ggplot(bird_models_mass, aes(x = log(mass), y = abs(estimate))) +
    geom_point(colour = "turquoise4", size = 3, alpha = 0.3) +
    geom_smooth(method = "lm", colour = "deepskyblue4", fill = "turquoise4") +
    geom_label_repel(data = subset(bird_models_mass, log(mass) > 9),
                     aes(x = log(mass), y = abs(estimate),
                         label = common.name),
                     box.padding = 1, size = 5, nudge_x = 1,
                     # We are specifying the size of the labels and nudging the points so that they
                     # don't hide data points, along the x axis we are nudging by one
                     min.segment.length = 0, inherit.aes = FALSE) +
    geom_label_repel(data = subset(bird_models_mass, log(mass) < 1.8),
                     aes(x = log(mass), y = abs(estimate),
                         label = common.name),
                     box.padding = 1, size = 5, nudge_x = 1,
                     min.segment.length = 0, inherit.aes = FALSE) +
    theme_clean() +
    labs(x = "\nlog(mass)", y = "Absolute population change\n"))

ggsave("trends_mass.png", trends_mass, path = "Plots", height = 5, width = 6)
