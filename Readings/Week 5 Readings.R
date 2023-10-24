##%#########################################################################%##
#                                                                             #
#                          Data science W5 (19.10.2023)                       #
#                                  Readings                                   #
#                                                                             #
##%#########################################################################%##

#R for Data Science Chapter 3 Data Visualisation (https://r4ds.had.co.nz/data-visualisation.html)
#Libraries
library(nycflights13)
library(tidyverse)

#Data
mpg

#ggplot() ----
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class)) #colours the dots according to the class variable

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue") #colours the dots blue

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class)) #changes size of the dots

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class)) #changes opacity of the dots

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class)) #changes shape of the dots

#Facets
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)

#Geometric objects
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy)) #basic

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv, color = drv)) +
  geom_point(mapping = aes(x = displ, y = hwy, color = drv)) #improved

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth()

#Barplots
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))

ggplot(data = diamonds) + 
  stat_count(mapping = aes(x = cut)) #interchangeable with geom_bar()

#Adjustments
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, colour = cut)) #colours (outline) by clarity

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut)) #fills by cut

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity)) #fills by clarity

ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(alpha = 1/5, position = "identity") #alpha changes the opacity

ggplot(data = diamonds, mapping = aes(x = cut, colour = clarity)) + 
  geom_bar(fill = NA, position = "identity") #no fill, just outline

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill") #stacked (good for comparing proportions)

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge") #not stacked

#Coordinate systems
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() #vertical boxplot

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip() #horizontal boxplot

nz <- map_data("nz")
ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") #map of new zealand

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap() #corrects the aspect ratio for maps

slo <- map_data("slovenia")


