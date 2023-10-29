##%#########################################################################%##
#                                                                             #
#                           Data science Challenge 2                          #
#                               Zoja Manček Páli                              #
#                                                                             #
##%#########################################################################%##

#Group assignment (assign roles - issues about the meeting, explain the roles in the README, etc.)- 
#1. Find a story with publicly available data (ecological ideally)
#2. Develop key message
#3. Build and present a workflow
#4. One figure that's appropriate to be presented (easy to read, good colours, include a caption)

#WD
setwd("~/") #erases previously set WDs
setwd("DS - Personal repo - Zoja/Data Science Challenges/Challenge 2") #sets a new one
getwd() #check that it's worked


#Libraries
library(tibble)
library(sf)
library(tidyverse)

#Data
cars <- read.csv("Electric_Vehicle_Population_Data.csv") #data from https://catalog.data.gov/dataset/electric-vehicle-population-data
capitals <- read.csv("us-state-capitals.csv") #data from https://github.com/jasperdebie/VisInfo/blob/master/us-state-capitals.csv

#Inspection 
head(cars)
str(cars)

#Wrangling
#Tidying the data ----
new_cars <- cars %>% 
  mutate(Vehicle.Location = gsub("POINT \\((-?\\d+\\.\\d+) (-?\\d+\\.\\d+)\\)", "\\1,\\2", Vehicle.Location), # removes the POINT and () around the longitude and latitude
         Vehicle.Location = ifelse(Vehicle.Location == "", NA, Vehicle.Location)) %>% # adds NA for missing location values
  separate(Vehicle.Location, into = c("Longitude", "Latitude"), sep = ",") %>% # splits the values into latitude and longitude
  filter(State != "BC" & State != "AP") %>% # removes the Non-US state rows 
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude)) %>%  # changes longitude and latitude values to numeric
  group_by(State, Make) %>% # group by the state and the make of the car
  summarise(Count = n()) %>% # count the number of the occurrences of each make in each state
  arrange(State, desc(Count)) %>% # select the car with the highest number of occurrences in each state
  slice(1) %>%
  ungroup() # ungroup

length(unique(new_cars$State)) #41 states total (excluding the non-US ones)
length(unique(new_cars$Make)) #12 different makes of cars

#State capital coordinates ----
state_capitals <- data.frame(
  State = c("WA", "BC", "IN", "NH", "VA", "NC", "GA", "CA", "NE", "CO", "NY", 
            "MD", "DC", "AL", "LA", "IL", "TX", "FL", "AZ", "OH", "HI", "CT", 
            "MA", "OR", "NJ", "IA", "SC", "AR", "ID", "NV", "OK", "MN", "UT", 
            "AP", "AK", "KY", "MO", "PA", "MT", "KS", "WY", "MS", "DE"),
  cap_lat = c(47.6097, 39.7684, 43.2081, 37.54, 35.7796, 33.749, 
              38.8951, 40.8136, 39.7392, 42.6526, 38.9787, 38.8951, 
              32.8067, 30.6954, 30.4583, 39.7817, 30.2672, 30.4383, 
              33.4484, 39.9612, 21.3045, 41.7658, 42.3601, 44.06, 
              40.2206, 41.5911, 33.751, 34.7465, 38.5816, 36.1699, 
              35.4634, 44.8994, 46.5891, 38.8951, 58.3019, 61.2181, 
              37.7749, 38.5731, 46.5891, 38.3362, 39.9526, 46.5891, 
              39.7392),
  cap_long = c(-122.3331, -86.1581, -71.5376, -77.46, -78.6382, -84.387, 
               -77.0364, -96.7056, -104.9903, -73.7562, -76.7019, -77.0364, 
               -117.1611, -87.7406, -91.1403, -89.6811, -97.7431, -84.2708, 
               -112.0740, -82.9988, -157.8580, -72.6851, -71.0589, -122.8742, 
               -74.7597, -93.6208, -84.3880, -92.2896, -93.6237, -119.7539, 
               -97.3328, -94.6395, -93.6237, -134.4197, -149.4068, -93.6237, 
               -84.0907, -112.0740, -95.7129, -77.0369, -111.8910, -75.3778, 
               -75.4694)) #gives the state capitals for each state

#Final data for plotting ----
cars_per_state <- new_cars %>%
  left_join(state_capitals, by = c("State" = "State"))

#Plotting data
usa <- map_data("world", region = c("USA", "Canada", 'Mexico'))

ggplot() +
  geom_map(data = usa, map = usa, aes(x = long, y = lat, map_id = region),
           color = "gray80", fill = "gray80") +
  geom_path(data = usa, aes(x = long, y = lat, group = group), color = "black", size = 0.3) +
  geom_point(data = cars_per_state, aes(x = cap_long, y = cap_lat), 
             alpha = 0.8, color = "red", size = 3) +
  ggrepel::geom_label_repel(data = cars_per_state, aes(x = cap_long, y = cap_lat, label = Make),
                            max.overlaps = 100, box.padding = 0.5, point.padding = 0.1, 
                            segment.color = "blue", size = 2) +
  coord_sf(xlim = c(-170, -60), ylim = c(10, 75))  +
  theme_void()

