##%#########################################################################%##
#                                                                             #
#                          Data science Challenge 2                           #
#                              Zoja Manček Páli                               #
#                                                                             #
##%#########################################################################%##

#WD
setwd("~/")
setwd("Personal repo - zmancekpali/Data Science Challenges/Challenge 2")
getwd()

#Libraries
library(ggmap)
library(gridExtra)
library(sf)
library(tidyverse)
library(tigris)

#Data
cars <- read.csv("Challenge/Electric_Vehicle_Population_Data.csv")  # Data from https://catalog.data.gov/dataset/electric-vehicle-population-data
ggmap::register_google(key = "AIzaSyDnersipSvcXuK4tCDbr8NOpa-qsrYf9pc", 
                       write = TRUE)  # Register your own Google API Key here

#Inspection
head(cars)
str(cars)

#Initial wrangling for the first map (map1) ----
wa_county_coordinates <- cars %>%
  filter(State == "WA") %>%  # Filter out WA state
  mutate(Vehicle.Location = gsub("POINT \\((-?\\d+\\.\\d+) (-?\\d+\\.\\d+)\\)", 
                                 "\\1,\\2", Vehicle.Location),  # Removes the POINT and () around the longitude and latitude
         Vehicle.Location = ifelse(Vehicle.Location == "", NA, 
                                   Vehicle.Location)) %>%  # Replaces  missing location values with NAs
  separate(Vehicle.Location, into = c("Longitude", "Latitude"), sep = ",") %>%  # Splits the location values into latitude and longitude columns
  select(County, Longitude, Latitude) %>%  # Select only the relevant columns
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude)) # Changes longitude and latitude values to numeric

washington_cars <- cars %>%
  filter(State == "WA") %>%  # Filter out WA state
  group_by(County, Make) %>%  # Group by the county, make, and model of the car
  summarise(Count = n()) %>%  # Count the number of occurrences of each make and model in each county
  arrange(County, desc(Count)) %>%  # Select the car with the highest number of occurrences in each county
  slice(1) %>%  # This makes sure you only have one value per county
  ungroup()  # Ungroup

joined <- left_join(washington_cars, wa_county_coordinates, by = "County") %>%  # Join the two datasets by the County column
  group_by(County) %>%  # Group by county
  arrange(County, desc(Count)) %>%  # Arrange by county in descending order
  slice(1) %>% # This makes sure you only have one value per county
  ungroup()  # Ungroup

#Additional wrangling for the bubble plot
tesla_counties <- cars %>%   
  filter(State == "WA", Make == "TESLA") %>%  # Filters data for Teslas in WA
  group_by(County, Model) %>%  # Group by county   
  summarise(Count = n()) %>%  # Count the number of the occurrences of Tesla in each county  
  ungroup()  # Ungroup

filtered_wa_counties <- tesla_counties %>%   
  filter(County %in% c('King', 'Snohomish', 'Pierce'))  # Filter out the 3 counties with the most Tesla occurences

#Additional wrangling for the second map

wa_counties <- counties(year = 2022, state = "WA")

tesla_count_county <- cars %>% # Wrangling for Tesla count by county graph
  filter(State == "WA", Make == "TESLA") %>% # Filters WA Tesla cars
  group_by(County) %>% # Groups Tesla cars by county 
  summarise(Count = n()) %>% # Gets count of Tesla cars in each county 
  ungroup()

joined2 <- wa_counties %>% 
  left_join(tesla_count_county, by = c("NAME" = "County")) # Merged mapping data with electric vehicle data by county

#Maps ----
washington_state <- c(lat = 47.751076, lon = -120.740135)  # Set centre coordinates for WA plot 
washington_map <- get_map(location = washington_state, zoom = 6, 
                          source = "google", maptype = "terrain")  # Get the map from Google
(washington_ggmap <- ggmap(washington_map))  # Plot the map of WA
joined$Make <- factor(joined$Make)

#WA Map
(map1 <- ggmap(washington_map) +
    geom_point(data = joined, aes(x = Longitude, y = Latitude, 
                                  color = Make, shape = Make), size = 3) +  # Plots the county coorinates and groups them by car make
    ggrepel::geom_label_repel(data = joined, aes(x = Longitude, y = Latitude, 
                                                 label = paste(County)),
                              max.overlaps = 100, box.padding = 0.5, 
                              point.padding = 0.1, segment.color = "black", 
                              size = 3) +  # Adds labels with county names
    scale_color_manual(values = c("TESLA" = "darkred", "FORD" = "black", 
                                  "CHEVROLET" = "lightpink3"), 
                       guide = guide_legend(title = "Make")) +  # Changes the colour of the points by make
    scale_shape_manual(values = c("TESLA" = 15, "FORD" = 16, 
                                  "CHEVROLET" = 17), 
                       guide = guide_legend(title = "Make")) +  # Changes the shape of the points by make
    xlab("Longitude") +
    ylab("Latitude") +
    theme(legend.position = c(0.85, 0.87)) +  # Specify legend position
    labs(title = "Most Common Electric Car Model in WA Counties",       
         x = "Longitude",       
         y = "Latitude"))

ggsave("map_1.png", map1, path = "Plots", units = "cm", 
       width = 20, height = 17)  # Save plot to Final Plots folder

#Bubble plot
(bb_plot <- ggplot(filtered_counties, aes(x = Model, y = County, 
                                          size = Count, color = Count)) +
    geom_point(alpha = 0.6) +    
    scale_color_gradient(low = "lightpink3", high = "darkred") + # Adjust colors as needed  
    labs(title = "Tesla Model Counts in Washington's 3 Largest Counties",       
         x = "Model",       
         y = "County") +  
    scale_size_continuous(range = c(5, 20)) +    
    theme_minimal() +  
    guides(size = guide_legend(title = "Count"),
           color = guide_legend(title = "Count")) +  # Add legend 
    theme(axis.line.x = element_line(color="black", linewidth = 0.5),
          axis.line.y = element_line(color="black", linewidth = 0.5)))  # Add axis lines 

ggsave("bubble_plot.png", bb_plot, path = "Challenge/Final Plots", units = "cm", 
       width = 20, height = 17)   # Save plot to Final Plots folder

#WA Tesla Count by County Map
(map2 <- ggplot(data = joined2) +
    geom_sf(aes(fill = Count), color = "white") +
    scale_fill_distiller(palette = "Reds", direction = 1, 
                         breaks = c(0, 1000, 5000, 10000, 20000, 35000)) +  # Adjusts the count bar
    labs(title = "Tesla Count by Washington State County",
         fill = "Count") +
    theme_minimal() +
    theme(panel.grid.major = element_blank()) +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.background = element_rect(fill = "white"),
          plot.title = element_text(family = "Georgia",size = 12, face = "bold", 
                                    vjust = -1),
          legend.title = element_text(family = "Georgia", size = 8, face = "bold"),
          legend.position = "right") +
    guides(fill = guide_legend(keywidth = 6, keyheight = 6) +
             geom_sf_label(data = merged_data, aes(label = NAME), size = 7)) +
    ggrepel::geom_label_repel(data = joined, aes(x = Longitude, y = Latitude, 
                                                 label = paste(County)),
                              max.overlaps = 100, box.padding = 0.5, 
                              point.padding = 0.1, segment.color = "black", 
                              size = 3))  # Adds labels with county names

ggsave("map_2.png", map2, path = "Challenge/Final Plots", units = "cm", 
       width = 20, height = 17)   # Save plot to Final Plots folder

#Arrange the plots
grid <- grid.arrange(map1, map2, bb_plot, ncol = 3)
ggsave("grid.png", grid, path = "Challenge/Final Plots", units = "cm",
       width = 50, height = 17)  # Save plot to Final Plots folder
