##%#########################################################################%##
#                                                                             #
#                          Data science W5 (19.10.2023)                       #
#                                  Readings                                   #
#                                                                             #
##%#########################################################################%##

#R for Data Science Chapter 3 Data Visualisation (https://r4ds.had.co.nz/data-visualisation.html) ----
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


#R for Data Science Chapter 28 Graphics for Communication (https://r4ds.had.co.nz/graphics-for-communication.html) ----
#Libraries
library(nycflights13)
library(tidyverse)

#Data
mpg

#ggplot() ----
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Fuel efficiency generally decreases with engine size", #title text
    subtitle = "Two seaters (sports cars) are an exception because of their light weight", #subtitle text
    caption = "Data from fueleconomy.gov", #caption text
    x = "Engine displacement (L)", #x-axis
    y = "Highway fuel economy (mpg)", #y-axis
    colour = "Car type") #colour by car type

#Annotations
best_in_class <- mpg %>%
  group_by(class) %>%
  filter(row_number(desc(hwy)) == 1)

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_text(aes(label = model), data = best_in_class) #annotations; basic

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_label(aes(label = model), data = best_in_class, nudge_y = 2, alpha = 0.5) #much easier to read annotations

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_point(size = 3, shape = 1, data = best_in_class) +
  ggrepel::geom_label_repel(aes(label = model), data = best_in_class) #this package automatically makes it so the labels don't overlap

#Single label
label <- mpg %>%
  summarise(
    displ = max(displ),
    hwy = max(hwy),
    label = "Increasing engine size is \nrelated to decreasing fuel economy.")

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_text(aes(label = label), data = label, vjust = "top", hjust = "right")

#Legend position
base <- ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class))

base + theme(legend.position = "left")
base + theme(legend.position = "top")
base + theme(legend.position = "bottom")
base + theme(legend.position = "right") # the default

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_smooth(se = FALSE) +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(nrow = 1, override.aes = list(size = 4))) #legend all in one row

#Colours and shapes + brewer
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv, shape = drv)) +
  scale_colour_brewer(palette = "Set1") #see the brewer_palettes.png for all palettes

presidential %>%
  mutate(id = 33 + row_number()) %>%
  ggplot(aes(start, id, colour = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_colour_manual(values = c(Republican = "red", Democratic = "blue"))

#Themes
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme_classic() #my favourite; see visualisation-themes.png for all ggplot themes





