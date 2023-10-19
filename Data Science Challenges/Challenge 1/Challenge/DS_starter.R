# Starter code for data wrangling challenge
# Please rewrite this code to make it as efficient as possible using pipes (dplyr)

# Librarys
library(dplyr)
library(tidyverse)
library(beepr)
library(ggthemes)
library(gridExtra)

# Load Living Planet Data
LPI_data <- read.csv("LPI_birds.csv")

# Explore data
head(LPI_data)
summary(LPI_data)
summary(LPI_data$Class)

# Reshape data into long form
LPI_long <- pivot_longer(data = LPI_data, cols = 25:69, names_to = "year",values_to = "pop")

# EXtract numeric values from year column
LPI_long$year <- parse_number(LPI_long$year)

# Make concatination of genus and species
LPI_long$genus_species <- paste(LPI_long$Genus, LPI_long$Species, sep="_")

# Make concatination of genus and species and population id
LPI_long$genus_species_id <- paste(LPI_long$Genus, LPI_long$Species, LPI_long$id, sep="_")

# Only keep rows with numeric values
LPI_long_fl <- filter(LPI_long, is.finite(pop))

# Create columns for the first and most recent years that data were collected
LPI_long_fl_1 <- LPI_long_fl %>% group_by(genus_species_id) %>% mutate(maxyear=max(year)) %>% mutate(minyear=min(year))

# Make a new data frame
LPI_long_fl_2 <- LPI_long_fl_1

# Create a column for the length of time data available
# This is the duration of monitoring for each population
LPI_long_fl_2$lengthyear <- LPI_long_fl_2$maxyear-LPI_long_fl_2$minyear

# Scale population trend data
LPI_long_fl_3 <- LPI_long_fl_2 %>% mutate(scalepop=(pop-min(pop))/(max(pop)-min(pop)))

# Only keep rows with numeric values
LPI_long_fl_4 <- LPI_long_fl_3 %>% filter(is.finite(scalepop))

# Only keep rows with more than 5 years of data
LPI_long_fl_5 <- LPI_long_fl_4 %>% filter(lengthyear > 5) 

# Remove any groupings
LPI_long_fl_6 <- LPI_long_fl_5 %>% ungroup()  

# Remove unnecessary columns
LPI_long_fl_6 <- LPI_long_fl_6 %>% select(-Data.source.citation)
LPI_long_fl_6 <- LPI_long_fl_6 %>% select(-Authority)

LPI_long <- LPI_long_fl_6

# Filter out Goose populations
Aves <- LPI_long %>% filter(Class=="Aves")
Birds <- Aves %>% filter(Common.Name == "Canada goose")
Birds <- Aves %>% filter(Order=="Anseriformes")
Birds <- Aves %>% filter(Family == "Anatidae")
Goose <- Birds %>% filter(Genus=="Branta")
Goose2 <- Goose %>% filter(Species=="canadensis")
Goose2 <- Goose %>% group_by(Country.list) %>% ungroup()

unique(Goose2$Country.list)

# Pick the countries of interest
GooseUnitedKingdom <- Goose2 %>% filter(Country.list=="United Kingdom")

GoosePops <- rbind(GooseUnitedKingdom)
                      
library(ggplot2)

# Data for plotting
plotGooseData <- GoosePops %>% select(Country.list,year,scalepop,id,lengthyear)

# Get only the populations with more than 15 years of data from those locations
plotGooseData <-plotGooseData %>% group_by(id) %>% filter(lengthyear>15)

# Plot Goose populations over time
(f1 <- ggplot(plotGooseData, aes(x=year, y=scalepop, group = id, colour=Country.list))+geom_line()+geom_point()+theme(legend.position = "bottom")+labs(title="Goose trends")+theme(plot.title=element_text(size=15, hjust=0.5)))

# Load Site Coordinate Data
site_coords <- read.csv("site_coords.csv")
head(site_coords)

# Merge Goose data with site coordinates
Goose_sites <- left_join(plotGooseData, site_coords, by = "id")
                        
# Make map of where the Goose populations are located
(f2 <- ggplot(Goose_sites, aes(x=Decimal.Longitude, y=Decimal.Latitude, colour=Country.list))+borders("world", colour = "gray40", fill = "gray75", size = 0.3) +coord_cartesian(xlim = c(-10, 35), ylim = c(30, 70)) +theme_map() +geom_point(size=4) +theme(legend.position="none") +theme(plot.title=element_text(size=15, hjust=0.5)) +labs(title="Population map"))

# Make a panel of the two graphs
grid.arrange(f1, f2, ncol = 2)

# Remember to save your graphs with code and insert the code in your script.
