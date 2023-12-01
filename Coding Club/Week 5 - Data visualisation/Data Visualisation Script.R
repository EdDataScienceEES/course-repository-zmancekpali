##%#########################################################################%##
#                                                                             #
#                         Coding club W5 (18.10.2023)                         #
#                                                                             #
##%#########################################################################%##

#WD
setwd("~/") #erases previously set WDs
setwd("Personal repo - zmancekpali/Coding Club/Week 5 - Data Visualisation") #sets a new one
getwd() #check that it's worked

#Libraries
library(colourpicker)
library(dplyr)
library(ggalt)
library(ggplot2)
library(ggrepel)
library(gridExtra)
library(readr)
library(tidyr)
library(viridis)

#Data
LPI <- read.csv("Data/LPIdata_CC.csv")
LPI2 <- gather(LPI, "year", "abundance", 9:53)
magic_veg <- read.csv("Data/magic_veg.csv")
lter <- read.csv("Data/lter.csv")
niwot_plant_exp <- read.csv("Data/niwot_plant_exp.csv")

#Exploration
str(LPI2)
unique(LPI2$Common.Name) 
str(magic_veg)

#Wrangling
LPI2$year <- parse_number(LPI2$year)
LPI2$abundance <- as.numeric(LPI2$abundance)

#Plotting vulture populations ----
vulture <- filter(LPI2, Common.Name == "Griffon vulture / Eurasian griffon")
vulture <- na.omit(vulture)

#Histogram
base_hist <- hist(vulture$abundance)
(vulture_hist <- ggplot(vulture, aes(x = abundance)) +                
    geom_histogram(binwidth = 250, colour = "#8B5A00", fill = "#CD8500") + #changing the bin width and colours of the bins
    geom_vline(aes(xintercept = mean(abundance)), #adding a line for mean abundance
               colour = "red", linetype = "dashed", linewidth = 1) + 
    theme_classic() + 
    ylab("Count\n") + 
    xlab("\nGriffon vulture abundance")  + #\n adds a blank line between axis and text
    theme(axis.text = element_text(size = 12), 
          axis.title.x = element_text(size = 14, face = "plain"), 
          panel.grid = element_blank(), #removing the grey grid lines
          plot.margin = unit(c(1,1,1,1), units = , "cm")))   


#Colourpicker
#Select howevermany colours you want, and they will appear like below:
c("#5F62BF", "#448F4E", "#CC6CC4")

#Scatter plot
vultureITCR <- filter(vulture, Country.list %in% c("Croatia", "Italy"))
(vulture_scatter <- ggplot(vultureITCR, aes (x = year, y = abundance, colour = Country.list)) +
    geom_point(size = 2) +                                             
    geom_smooth(method = "lm", aes(fill = Country.list)) +  #adding linear model line, colour-coded by country
    theme_bw() +
    scale_fill_manual(values = c("#EE7600", "#00868B")) + #adding custom colours for solid geoms (ribbon)
    scale_colour_manual(values = c("#EE7600", "#00868B"), #adding custom colours for lines and points
                        labels = c("Croatia", "Italy")) + #adding labels for the legend
    ylab("Griffon vulture abundance\n") +                             
    xlab("\nYear")  +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1), #making the years at a bit of an angle
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                        
          panel.grid = element_blank(), #removing gridlines                               
          plot.margin = unit(c(1,1,1,1), units = , "cm"), #adding a 1cm margin around the plot
          legend.text = element_text(size = 12, face = "italic"), #setting the font for the legend text
          legend.title = element_blank(), #removing the legend title
          legend.position = c(0.9, 0.9))) 

#Boxplot
(vulture_boxplot <- ggplot(vultureITCR, aes(Country.list, abundance)) + 
    geom_boxplot(aes(fill = Country.list)) +
    theme_bw() +
    scale_fill_manual(values = c("#EE7600", "#00868B")) +              
    scale_colour_manual(values = c("#EE7600", "#00868B")) +
    ylab("Griffon vulture abundance\n") +                             
    xlab("\nCountry")  +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                     
          panel.grid = element_blank(),                                      
          plot.margin = unit(c(1,1,1,1), units = , "cm"),   
          legend.position = "none")) 

#Bar plot
richness <- LPI2 %>% filter (Country.list %in% c("United Kingdom", "Germany", "France", "Netherlands", "Italy")) %>%
  group_by(Country.list) %>%
  mutate(richness = (length(unique(Common.Name)))) #create new column based on how many unique common names (or species) there are in each country 

(richness_barplot <- ggplot(richness, aes(x = Country.list, y = richness)) +
    geom_bar(position = position_dodge(), stat = "identity", colour = "black", fill = "#00868B") +
    theme_bw() +
    ylab("Species richness\n") +                             
    xlab("Country")  +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                      
          panel.grid = element_blank(),                                          
          plot.margin = unit(c(1,1,1,1), units = , "cm")))

#Facets and panels
(vulture_scatter_all <- ggplot(vulture, aes (x = year, y = abundance, colour = Country.list)) +
    geom_point(size = 2) +                                              
    geom_smooth(method = "lm", aes(fill = Country.list)) +           
    theme_bw() +
    ylab("Griffon vulture abundance\n") +                             
    xlab("\nYear")  +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),     
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                        
          panel.grid = element_blank(),                                 
          plot.margin = unit(c(1,1,1,1), units = , "cm"),              
          legend.text = element_text(size = 12, face = "italic"),    
          legend.title = element_blank(),                             
          legend.position = "right")) #this plots them all on one plot (hard to read because they overlap quite a bit)

(vulture_scatter_facets <- ggplot(vulture, aes (x = year, y = abundance, colour = Country.list)) +
    geom_point(size = 2) +                                             
    geom_smooth(method = "lm", aes(fill = Country.list)) +          
    facet_wrap(~ Country.list, scales = "free_y") + #this line creates the faceting
    theme_bw() +
    ylab("Griffon vulture abundance\n") +                             
    xlab("\nYear")  +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1), 
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                        
          panel.grid = element_blank(),                          
          plot.margin = unit(c(1,1,1,1), units = , "cm"),         
          legend.text = element_text(size = 12, face = "italic"), 
          legend.title = element_blank(),      
          legend.position = "right")) #much easier to read

grid.arrange(vulture_hist, vulture_scatter, vulture_boxplot, ncol = 1)

(panel <- grid.arrange(
  vulture_hist + ggtitle("(a)") + ylab("Count") + xlab("Abundance") + 
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")),
  
  vulture_boxplot + ggtitle("(b)") + ylab("Abundance") + xlab("Country") +
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")),
  
  vulture_scatter + ggtitle("(c)") + ylab("Abundance") + xlab("Year") +
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")) +
    theme(legend.text = element_text(size = 12, face = "italic"),     
          legend.title = element_blank(),                                   
          legend.position = c(0.85, 0.85)), # changing the legend position so that it fits within the panel
  ncol = 1)) # ncol determines how many columns you have
ggsave("vulture_panel2.png", panel, path = "Plots", width = 5, height = 12) 


#Challenge ----
#Plotting razorbill and ruff populations over time
razorbill <- filter(LPI2, Common.Name == "Razorbill") %>% na.omit(razorbill)
ruff <- filter(LPI2, Common.Name == "Ruff") %>% na.omit(ruff)
(razorbill_scatter <- ggplot(razorbill, aes (x = year, y = abundance)) +
    geom_point(size = 2, col = "orange") +                                             
    geom_smooth(method = "lm", col = "orchid") + 
    theme_classic() +
    ylab("Razorbill abundance\n") +                             
    xlab("\nYear"))

(ruff_scatter <- ggplot(ruff, aes (x = year, y = abundance)) +
    geom_point(size = 2, col = "red") +                                             
    geom_smooth(method = "lm", col = "lightblue3") + 
    theme_classic() +
    ylab("Ruff abundance\n") +                             
    xlab("\nYear"))
challenge_scatter_grid <- grid.arrange(razorbill_scatter, ruff_scatter, ncol = 2)
ggsave(challenge_scatter_grid, file = "Challenge_species_scatter_grid.jpg", path = "Plots",
       width = 12, height = 5)

#Boxplots (meant to be 5 countries but these two species only intersect in two so I will do two)
common_countries <- intersect(unique(ruff$Country.list), unique(razorbill$Country.list))

razorbill_countries <- filter(razorbill, Country.list %in% c("Norway", "Spain"))
ruff_countries <- filter(ruff, Country.list %in% c("Norway", "Spain"))

(razorbill_boxplot <- ggplot(razorbill_countries, aes(Country.list, abundance)) + 
    geom_boxplot(aes(fill = Country.list)) +
    theme_classic() +
    scale_fill_manual(values = c("#EE7600", "#00868B")) +              
    scale_colour_manual(values = c("#EE7600", "#00868B")) +
    ylab("Razorbill vulture abundance\n") +                             
    xlab("\nCountry")  +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                     
          panel.grid = element_blank(),                                      
          plot.margin = unit(c(1,1,1,1), units = , "cm"),   
          legend.position = "none")) 

(ruff_boxplot <- ggplot(ruff_countries, aes(Country.list, abundance)) + 
    geom_boxplot(aes(fill = Country.list)) +
    theme_classic() +
    scale_fill_manual(values = c("#EE7600", "#00868B")) +              
    scale_colour_manual(values = c("#EE7600", "#00868B")) +
    ylab("Ruff vulture abundance\n") +                             
    xlab("\nCountry")  +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                     
          panel.grid = element_blank(),                                      
          plot.margin = unit(c(1,1,1,1), units = , "cm"),   
          legend.position = "none"))  

challenge_boxplot_grid <- grid.arrange(razorbill_boxplot, ruff_boxplot, ncol = 2)
ggsave(challenge_boxplot_grid, file = "Challenge_species_boxplot_grid.jpg", width = 12, height = 5)


#Customising figures ----
#Ggplot histograms
species_counts <- magic_veg %>%
  group_by(land, plot) %>%
  summarise(Species_number = length(unique(species)))

(hist <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity")) #stacked
(narnia_hist <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") + #next to one another
    scale_x_continuous(breaks = c(1,2,3,4,5,6)) + #continuous x and y axes
    scale_y_continuous(limits = c(0, 50)) +
    scale_fill_manual(values = c("rosybrown1", "#deebf7"), #specifying the colours
                      labels = c("HOGSMEADE", "NARNIA"), #changing the site labels
                      name = "Land of Magic") + #legend title
    labs(title = "Species richness by plot", #title
         subtitle = "In the magical lands", #subtitle
         caption = "Data from the Ministry of Magic", #adds a caption
         x = "\n Plot number", y = "Number of species \n") + #axes labels
    theme_classic() + 
    theme(panel.grid = element_blank(), #removes gridlines
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.title = element_text(face = "bold"),
          legend.position = "bottom", 
          legend.box.background = element_rect(color = "grey", linewidth = 0.3))) 
ggsave("narnia_hist.png", narnia_hist, path = "Plots", width = 7, height = 5, dpi = 300) #either way of saving the plots works

#Create own colour palette
land <- factor(c("Narnia", "Hogsmeade", "Westeros", "The Shire", "Mordor", "Forbidden Forest", "Oz"))
counts <- as.numeric(c(55, 48, 37, 62, 11, 39, 51))
more_magic <- data.frame(land, counts)
length(levels(more_magic$land)) #7 levels 
magic.palette <- c("#698B69", "#5D478B", "#5C5C5C", "#CD6090", "#EEC900", "#5F9EA0", "#6CA6CD")    # defining 7 colours
names(magic.palette) <- levels(more_magic$land)   

(hist <- ggplot(more_magic, aes(x = land, y = counts, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") + 
    scale_y_continuous(limits = c(0, 65)) +
    scale_fill_manual(values = magic.palette,                        # using our palette here
                      name = "Land of Magic") +                
    labs(title = "Species richness in magical lands", 
         x = "", y = "Number of species \n") + 
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.text.x = element_text(angle = 45, hjust = 1), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.title = element_text(face = "bold"),
          legend.position = "bottom", 
          legend.box.background = element_rect(color = "grey", size = 0.3)))

#Ggplot box plots
yearly_counts <- magic_veg %>%
  group_by(land, plot, year) %>%                  
  summarise(Species_number = length(unique(species))) %>%
  ungroup() %>%
  mutate(plot = as.factor(plot))

(boxplot <- ggplot(yearly_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_boxplot() +
    scale_x_discrete(breaks = 1:6) +
    scale_fill_manual(values = c("rosybrown1", "#deebf7"),
                      breaks = c("Hogsmeade","Narnia"),
                      name="Land of magic",
                      labels=c("Hogsmeade", "Narnia")) +
    labs(title = "Species richness by plot", 
         x = "\n Plot number", y = "Number of species \n") + 
    theme_bw() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "bottom", 
          legend.box.background = element_rect(color = "grey", size = 0.3)))

yearly_counts$land <- factor(yearly_counts$land, 
                             levels = c("Narnia", "Hogsmeade"),
                             labels = c("Narnia", "Hogsmeade")) #rordering the factors
(boxplot <- ggplot(yearly_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_boxplot() +
    scale_x_discrete(breaks = 1:6) +
    scale_fill_manual(values = c("#deebf7", "rosybrown1"),
                      breaks = c("Narnia","Hogsmeade"),
                      name = "Land of magic",
                      labels = c("Narnia", "Hogsmeade")) +
    labs(title = "Species richness by plot", 
         x = "\n Plot number", y = "Number of species \n") + 
    theme_bw() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "bottom", 
          legend.box.background = element_rect(color = "grey", size = 0.3)))

yearly_counts$plot <- factor(yearly_counts$plot, 
                             levels = c("6", "1", "2", "3", "4", "5"),
                             labels = c("6", "1", "2", "3", "4", "5")) #reordering again

(boxplot2 <- ggplot(yearly_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_boxplot() +
    scale_x_discrete(breaks = 1:6) +
    scale_fill_manual(values = c("#deebf7", "rosybrown1"),
                      breaks = c("Narnia","Hogsmeade"),
                      name = "Land of magic",
                      labels = c("Narnia", "Hogsmeade")) +
    labs(title = "Species richness by plot", 
         x = "\n Plot number", y = "Number of species \n") + 
    theme_bw() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "bottom", 
          legend.box.background = element_rect(color = "grey", size = 0.3)))

#Regression lines
heights <- magic_veg %>%
  filter(!is.na(height)) %>%                 
  group_by(year, land, plot, id) %>%
  summarise(Max_Height = max(height)) %>%      
  ungroup() %>%                              
  group_by(year, land, plot) %>%
  summarise(Height = mean(Max_Height))        

(improved_mm_scat <- ggplot(heights, aes(year, Height, colour = land)) +
    geom_point() +
    theme_classic() +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2)))

#Ggplot theme
ggplot_theme <- function(){          
  theme_classic()+                        
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),       # customising lots of things
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 13),
          panel.grid = element_blank(),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          legend.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.position = c(0.9, 0.9))
}

(boxplot <- ggplot(yearly_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_boxplot() +
    scale_x_discrete(breaks = 1:6) +
    scale_fill_manual(values = c("#deebf7", "rosybrown1"),
                      breaks = c("Narnia","Hogsmeade"),
                      name = "Land of magic",
                      labels = c("Narnia", "Hogsmeade")) +
    labs(title = "Species richness by plot", 
         x = "\n Plot number", y = "Number of species \n") + 
    ggplot_theme() +             
    theme(legend.position = "right")) #this overwrites the theme legend position setting

#Maps ----
north_america <- map_data("world", region = c("USA", "Canada"))
north_america <- north_america[!(north_america$subregion %in% "Hawaii"),]
(lter_map1 <- ggplot() +
    geom_map(map = north_america, data = north_america,
             aes(long, lat, map_id = region), 
             color = "gray80", fill = "gray80", size = 0.3) +
    geom_point(data = lter, aes(x = long, y = lat))) #site location points

(lter_map2 <- ggplot() +
    geom_map(map = north_america, data = north_america,
             aes(long, lat, map_id = region), 
             color = "gray80", fill = "gray80", size = 0.3) +
    geom_point(data = lter, aes(x = long, y = lat, fill = ele),
               alpha = 0.8, size = 4, colour = "grey30",
               shape = 21)) #bequtified site location points (apparently)

(lter_map3 <- ggplot(data = north_america) +
    geom_map(map = north_america, 
             aes(long, lat, map_id = region), 
             color = "gray80", fill = "gray80", size = 0.3) +
    # you can change the projection here
    # coord_proj("+proj=wintri") +
    # the wintri one above is good for the whole world, the one below for just North America
    coord_proj(paste0("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96",
                      " +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")) +
    geom_point(data = lter, 
               aes(x = long, y = lat, fill = ele),
               alpha = 0.8, size = 4, colour = "grey30",
               shape = 21))  

       
(lter_map4 <- ggplot() +
    geom_map(map = north_america, data = north_america,
             aes(long, lat, map_id = region), 
             color = "gray80", fill = "gray80", size = 0.3) +
    coord_proj(paste0("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96",
                      " +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"),
               # zooming in by setting specific coordinates
               ylim = c(25, 80), xlim = c(-175, -50)) +
    geom_point(data = lter, 
               aes(x = long, y = lat, fill = ele),
               alpha = 0.8, size = 4, colour = "grey30",
               shape = 21))

(lter_map5 <- ggplot() +
  geom_map(map = north_america, data = north_america,
           aes(long, lat, map_id = region), 
           color = "gray80", fill = "gray80", size = 0.3) +
  coord_cartesian(xlim = c(-175, -50), ylim = c(25, 80)) +
  geom_point(data = lter, 
             aes(x = long, y = lat, fill = ele),
             alpha = 0.8, size = 4, colour = "grey30",
             shape = 21) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_label_repel(data = lter,
                             aes(x = long, y = lat,
                                 label = site),
                             box.padding = 1, size = 4))

(lter_map6 <- ggplot() +
    geom_map(map = north_america, data = north_america,
             aes(long, lat, map_id = region), 
             color = "gray80", fill = "gray80", size = 0.3) +
    coord_cartesian(xlim = c(-175, -50), ylim = c(25, 80)) +
    geom_point(data = lter, 
               aes(x = long, y = lat, fill = ele),
               alpha = 0.8, size = 4, colour = "grey30",
               shape = 21) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    geom_label_repel(data = subset(lter, ele > 2000),
                     aes(x = long, y = lat,
                         label = site),
                     box.padding = 1, size = 4, nudge_x = 1, nudge_y = 12))

(lter_map7 <- ggplot() +
    geom_map(map = north_america, data = north_america,
             aes(long, lat, map_id = region), 
             color = "gray80", fill = "gray80", size = 0.3) +
    coord_cartesian(xlim = c(-175, -50), ylim = c(25, 80)) +
    geom_point(data = lter, 
               aes(x = long, y = lat, fill = ele),
               alpha = 0.8, size = 4, colour = "grey30",
               shape = 21) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    geom_label_repel(data = subset(lter, ele > 2000),
                     aes(x = long, y = lat,
                         label = site),
                     box.padding = 1, size = 4, nudge_x = 1, nudge_y = 12) +
    labs(fill = "Elevation (m)") +
    annotate("text", x = -150, y = 35, colour = "#553c7f",
             label = "At 3528 m above sea level,\nNiwot Ridge is\nthe highest LTER site.",
             size = 4.5, fontface = "bold") +
    scale_fill_viridis(option = "magma", direction = -1, begin = 0.2))

#Distributions ----
theme_niwot <- function(){
  theme_bw() +
    theme(text = element_text(family = "Helvetica Light"),
          axis.text = element_text(size = 16), 
          axis.title = element_text(size = 18),
          axis.line.x = element_line(color="black"), 
          axis.line.y = element_line(color="black"),
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),                                          
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(1, 1, 1, 1), units = , "cm"),
          plot.title = element_text(size = 18, vjust = 1, hjust = 0),
          legend.text = element_text(size = 12),          
          legend.title = element_blank(),                              
          legend.position = c(0.95, 0.15), 
          legend.key = element_blank(),
          legend.background = element_rect(color = "black", 
                                           fill = "transparent", 
                                           size = 2, linetype = "blank"))
}

niwot_richness <- niwot_plant_exp %>% group_by(plot_num, year) %>%
  mutate(richness = length(unique(USDA_Scientific_Name))) %>% ungroup()

(distributions1 <- ggplot(niwot_richness, aes(x = fert, y = richness)) +
    geom_violin())

(distributions2 <- ggplot(niwot_richness, aes(x = fert, y = richness)) +
    geom_violin(aes(fill = fert, colour = fert), alpha = 0.5) + #alpha changes the opacity of the colours
    theme_niwot())

(distributions3 <- ggplot(niwot_richness, aes(x = fert, y = richness)) +
    geom_violin(aes(fill = fert, colour = fert), alpha = 0.5) +
    geom_boxplot(aes(colour = fert), width = 0.2) + #adds a boxplot on top
    theme_niwot())

(distributions4 <- ggplot(niwot_richness, aes(x = fert, y = richness)) +
    geom_violin(aes(fill = fert, colour = fert), alpha = 0.5) +
    geom_jitter(aes(colour = fert), position = position_jitter(0.1), alpha = 0.3) + #adds jitter on top
    theme_niwot())


#Histograms ----
observations <- niwot_plant_exp %>% group_by(USDA_Scientific_Name) %>%
  tally() %>% arrange(desc(n))  #rearanging the data frame so that the most common species are first

carex <- niwot_plant_exp %>%
  filter(str_detect(USDA_Scientific_Name, pattern = "Carex"))

(histogram <- ggplot(carex, aes(x = hits)) +
    geom_histogram(alpha = 0.6, 
                   breaks = seq(0, 100, by = 3),
                   fill = "palegreen4") +
    theme_niwot() +
    scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.1)))) #removes the empty blank space below the bars

#Outline
h <- hist(carex$hits, breaks = seq(0, 100, by = 3), plot = FALSE)
d1 <- data.frame(x = h$breaks, y = c(h$counts, NA))  
d1 <- rbind(c(0, 0), d1)

(histogram2 <- ggplot(carex, aes(x = hits)) +
    geom_histogram(alpha = 0.6, 
                   breaks = seq(0, 100, by = 3),
                   fill = "palegreen4") +
    theme_niwot() +
    scale_y_continuous(limits = c(0, 100), expand = expand_scale(mult = c(0, 0.1))) + #adds the outline
    geom_step(data = d1, aes(x = x, y = y),
              stat = "identity", colour = "palegreen4"))

#Mean line + annotations
(histogram3 <- ggplot(carex, aes(x = hits)) +
    geom_histogram(alpha = 0.6, 
                   breaks = seq(0, 100, by = 3),
                   fill = "palegreen4") +
    theme_niwot() +
    scale_y_continuous(limits = c(0, 100), expand = expand_scale(mult = c(0, 0.1))) +
    geom_step(data = d1, aes(x = x, y = y),
              stat = "identity", colour = "palegreen4") +
    geom_vline(xintercept = mean(carex$hits), linetype = "dotted", colour = "palegreen4", size = 1) +
    annotate("text", x = 50, y = 50, label = "The mean number of\nCarex observations was 16.") +
    geom_curve(aes(x = 50, y = 60, xend = mean(carex$hits) + 2, yend = 60),
               arrow = arrow(length = unit(0.07, "inch")), size = 0.7,
               color = "grey30", curvature = 0.3) +
    labs(x = "\nObservation hits", y = "Count\n"))





