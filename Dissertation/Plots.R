##%#########################################################################%##
#                                                                             #
#                    Dissertation script - Zoja Manček Páli                   #
#                              Started: 12.3.2024                             #
#                                                                             #
##%#########################################################################%##

#WD
setwd("~/") #erases previously set WDs
setwd("Personal repo - zmancekpali/Dissertation") #sets a new one
getwd() #check that it's worked

#Libraries
library(ggmap)
library(ggspatial)
library(gridExtra)
library(tidyverse)

#Set up the google maps connection
ggmap::register_google(key = "AIzaSyDnersipSvcXuK4tCDbr8NOpa-qsrYf9pc", 
                       write = TRUE) #register your own Google API Key here

#Data
leaves <- read.csv("traits_analysis.csv")
leaves2 <- read.csv("traits_analysis2.csv")


leaves <- leaves %>% 
  select("type", "code", "latin_name", "long", "lat") %>%  #select the relevant columns
  mutate(type = recode(type, "Alien" = "Alien species",
                       "Invasive" = "Invasive species", 
                       "Naturalised" = "Naturalised species", 
                       "Native" = "Native species")) %>%  #recode the invasion type names
  distinct(long, lat, .keep_all = TRUE) #remove multiple rows (avoids overplotting)

leaves2 <- leaves2 %>% 
  select("type", "code", "latin_name", "long", "lat") %>%  #select the relevant columns
  mutate(type = recode(type, "Alien" = "Alien species",
                       "Invasive" = "Invasive species", 
                       "Naturalised" = "Naturalised species", 
                       "Native" = "Native species")) %>%  #recode the invasion type names
  distinct(code, long, lat, .keep_all = TRUE) #remove multiple rows (avoids overplotting)


#Maps
(edinburgh <- get_googlemap("edinburgh", zoom = 14))
rbge <- c(left = -3.21240, bottom = 55.948, right = -3.2025, top = 58.9682) #set the map view window accordingly; I want to view the Botanics
edi_map_sattelite <- get_map(rbge, maptype = 'satellite', source = "google", zoom = 14) #specify what kind of map you want
ggmap(edi_map_satellite)



rbge_map <- get_googlemap("royal botanic gardens edinburgh", zoom = 16, maptype = "satellite")
ggmap(rbge_map)

(rbge_simple_map <- ggmap(rbge_map) +
    geom_point(data = leaves2, aes(x = long, y = lat, color = type), 
               size = 3, alpha = 0.5))

(rbge_map <- ggmap(edi_map_satellite) +
    geom_point(data = leaves, aes(x = lat, y = long, color = type, shape = type), 
               size = 4) +
    scale_color_manual(values = c("#5EA8D9", "#CC168F", "green", "#EEC900"),
                       name = "Invasion type") +
    scale_shape_manual(values = c(17, 15, 18, 16), name = "Invasion type") +
    xlab("Latititude") +
    ylab("Longitude") +
    theme(legend.position = c(0.85, 0.87),
          legend.key = element_rect(fill = "floralwhite"),
          legend.background = element_rect(fill = "floralwhite")))



(rbge_map_with_names <- ggmap(edi_map_satellite) +
    geom_point(data = leaves, aes(x = lat, y = long, color = type, shape = type), 
               size = 3) +
    scale_color_manual(values = c("#5EA8D9", "#CD6090", "#2CB82E", "#EEC900"),
                       name = "Invasion type") +
    scale_shape_manual(values = c(16, 17, 18, 15), name = "Invasion type") +
    ylab("Longitude") +
    xlab("Latitude") +
    theme(legend.position = c(0.85, 0.87),
          legend.key = element_rect(fill = "floralwhite"),
          legend.background = element_rect(fill = "floralwhite")) +
    ggrepel::geom_label_repel(data = leaves, aes(x = lat, y = long, label = latin_name),
                              max.overlaps = 200, box.padding = 0.5, point.padding = 0.1, 
                              segment.color = "floralwhite", size = 3, fontface = "italic") +
    annotation_north_arrow(location = "tl", which_north = "true", 
                           style = north_arrow_fancy_orienteering (text_col = 'floralwhite',
                                                                   line_col = 'floralwhite',
                                                                   fill = 'floralwhite')))
ggsave("rbge_map_with_names.jpg", rbge_map_with_names, path = "Plots", units = "cm", 
       width = 30, height = 20)

(map_with_codes <- ggmap(edi_map_satellite) +
    geom_point(data = leaves2, aes(x = long, y = lat, color = type, shape = type), 
               size = 3) +
    scale_color_manual(values = c("#5EA8D9", "#CD6090", "#698B69", "#EEC900"),
                       name = "Invasion type") +
    scale_shape_manual(values = c(16, 17, 18, 15), name = "Invasion type") +
    xlab("Longitude") +
    ylab("Latitude") +
    theme(legend.position = c(0.85, 0.87),
          legend.key = element_rect(fill = "floralwhite"),
          legend.background = element_rect(fill = "floralwhite")) +
    ggrepel::geom_label_repel(data = leaves2, aes(x = long, y = lat, label = code),
                              max.overlaps = 300, box.padding = 0.5, 
                              point.padding = 0.1, segment.color = "floralwhite", 
                              size = 3, fontface = "plain") +
    annotation_north_arrow(location = "tl", which_north = "true", 
                           style = north_arrow_fancy_orienteering (text_col = 'floralwhite',
                                                                   line_col = 'floralwhite',
                                                                   fill = 'floralwhite')))

ggsave("map_with_codes.jpg", map_with_codes, path = "Plots", units = "cm", 
       width = 30, height = 20)

(rbge_grid <- grid.arrange(rbge_map_with_names, map_with_codes, ncol = 2))
ggsave("rbge_grid.jpg", rbge_grid, path = "Plots", units = "cm", width = 50,
       height = 20)

#trying somethig else here:
library("rnaturalearth")
library("rnaturalearthdata")
library(maps)

worldmap = map_data('world')

rbge <- c(left = -3.21240, bottom = 55.948, right = -3.2025, top = 58.9682) #set the map view window accordingly; I want to view the Botanics
ggplot() + 
  geom_polygon(data = worldmap, 
               aes(x = long, y = lat, group = group),
               fill = 'gray90', 
               color = 'black') +
  theme_classic() +
  ylab("Latitude") +
  xlab("Longitude") +
  coord_fixed(ratio = 1.3,
              xlim = c(-3.215, -3.2025), 
              ylim = c(55.948, 55.9682)) +
  geom_point(data = leaves2, 
             aes(x = long, 
                 y = lat, size = 3), alpha = .7) + 
  scale_size_area(max_size = 8) + 
  scale_color_viridis_c() + 
  theme(legend.position = 'none') + 
  theme(title = element_text(size = 12))

