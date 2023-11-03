##%#########################################################################%##
#                                                                             #
#                           Data science Challenge 1                          #
#                               Zoja Manček Páli                              #
#                                                                             #
##%#########################################################################%##

#WD
setwd("~/") #erases previously set WDs
setwd("Personal repo - zmancekpali/Data Science Challenges/Challenge 1") #sets WD to this folder
getwd() #check that it's worked

#Libraries
library(ggthemes)
library(gridExtra)
library(tidyverse)

#Data
LPI_data <- read.csv("Challenge/LPI_birds.csv")
site_coords <- read.csv("Challenge/site_coords.csv") 

#Exploring the data
head(LPI_data)
head(site_coords)
summary(LPI_data)
summary(LPI_data$Class)
str(LPI_data)


#Wrangling code for LPI dataset:
LPI <- LPI_data %>%
  pivot_longer(cols = 25:69, names_to = "year", values_to = "pop") %>% #reshapes the data into long df
  mutate(year = as.numeric(parse_number(year)),
         genus_species = paste(Genus, Species, sep = "_"),
         genus_species_id = paste(genus_species, id, sep = "_")) %>% #creates new columns for genus_species and genus_species_id
  filter(!is.na(pop)) %>% #filters out NA values from the population column
  group_by(genus_species_id) %>% #groups the dataset by genus_species_id
  mutate(maxyear = max(year),
         minyear = min(year), 
         lengthyear = maxyear - minyear) %>% #creates new columns for max, min, mean, and length of years
  mutate(scalepop = (pop - min(pop)) / (max(pop) - min(pop))) %>% #calculates scaled population values
  filter(!is.na(scalepop), lengthyear > 5) %>% #filters out NA scaled population values and time periods longer than 5 years
  ungroup() %>% #ungroups the previous groupings
  select(-Data.source.citation, -Authority) #removes these two columns


#Wrangling code for the goose population
goose <- LPI %>%
  filter(Class == "Aves",
         Common.Name == "Canada goose",
         Order == "Anseriformes",
         Family ==  "Anatidae",
         Genus == "Branta",
         Species == "canadensis") %>% #filters out these specific conditions from the LPI_long dataset
  group_by(Country.list) %>% 
  ungroup() %>% 
  select(Country.list, year, scalepop, id, lengthyear) %>% #selects these columns from the goose data frame
  filter(Country.list == "United Kingdom") %>% #filters out data for the UK only
  group_by(id) %>% #groups them by species id
  filter(lengthyear > 15) #filters only time periods longer than 15 years


#Plot
(goose_plot <- ggplot(goose, aes(x = year, y = scalepop, color = Country.list)) +
                  geom_line() +
                  geom_point() +
                  labs(title="Goose population trends") + 
                  xlab("Year") +
                  ylab("Scaled population") +
                  theme(plot.title=element_text(size=15, hjust=0.5)) +
                  theme_classic() +
                  theme(legend.title=element_blank()) +
                  theme(legend.position = "bottom"))
ggsave("goose_plot.png", goose_plot, path = "Plots", units = "cm", width = 20, height = 15) #saves the plot at set dimensions to the Plots folder

#Map
goose_sites <- left_join(goose, site_coords, by = "id") #joins coordinates and goose by id

(goose_map <- ggplot(goose_sites, aes(x = Decimal.Longitude, y = Decimal.Latitude, colour = Country.list)) +
                    borders("world", colour = "gray40", fill = "gray75", size = 0.3) +
                    coord_cartesian(xlim = c(-10, 35), ylim = c(30, 70)) +
                    theme_map() +geom_point(size=4) +
                    theme(legend.position="none") +
                    theme(plot.title = element_text(size=15, hjust=0.5)) +
                    labs(title = "Population map"))
ggsave("goose_map.png", goose_map, path = "Plots", units = "cm", width = 20, height = 15)


#Panel of both plots
(goose_grid <- grid.arrange(goose_plot, goose_map, ncol = 2))
ggsave("goose_grid.png", goose_grid, path = "Plots", units = "cm", width = 30, height = 15) #saves the plot at set dimensions to your WD

