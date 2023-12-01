##%#########################################################################%##
#                                                                             #
#                         Coding club W4 (11.10.2023)                         #
#                                                                             #
##%#########################################################################%##

#WD
setwd("~/") #erases previously set WDs
setwd("Personal repo - zmancekpali/Coding Club/Week 4 - Data Wrangling") #sets a new one
getwd() #check that it's worked

#Libraries
library(dplyr)
library(ggplot2)
library(tidyr)

#Data
elongation <- read.csv("Data/EmpetrumElongation.csv", header = TRUE) 
treatments <- read.csv("Data/EmpetrumTreatments.csv", header = TRUE, sep = ";")
dragons <- read.csv("Data/dragons.csv", header = TRUE)
trees <- read.csv("Data/trees.csv", header = TRUE)

#Data manipulation (https://ourcodingclub.github.io/tutorials/data-manip-intro/) 
#Inspecting the data ----
head(elongation) #first few lines
head(treatments)
str(elongation) #types of variables

elongation$Indiv #gives all "indiv" variables from the data set
length(unique(elongation$Indiv)) #gives the number of distinct indiv values

elongation[2,5] #gives the value from the second row and fifth column
elongation[6,] #gives all values for the sixth row
elongation[6, ]$Indiv #gives the indiv value for the observation in the sixth row
elongation[elongation$Indiv == 603, ] #gives the values for individual 603

#Sub-setting with one condition
elongation[elongation$Zone < 4, ] #gives only the data for zones 2-3
elongation[elongation$Zone <= 4, ] #gives only the data for zones 2-3-4
elongation[!elongation$Zone >= 5, ]   #the ! means exclude; gives only the data the data for zones 2-3-4 

#Sub-setting with two conditions
elongation[elongation$Zone == 2 | elongation$Zone == 7, ] #gives only data for zones 2 and 7
elongation[elongation$Zone == 2 & elongation$Indiv %in% c(300:400), ] #gives data for shrubs in zone 2 whose ID numbers are between 300 and 400

#Sequences and repetitions
seq(300, 350, 10) #sequence from 300 to 350 by 10
rep(c(1, 2), 5) #repeats "1 2" five times

#Changing the object
elong2 <- elongation
names(elong2) #gives the names of the columns of elong2
names(elong2)[1] <- "zone" #changes Zone to zone (the first element of the names vector)
names(elong2)[2] <- "ID" #changes Indiv to ID (the second element of the names vector)

#To change a particular data point in the data set
#Suppose you want to change the value 5.1 for individual 373 in year 2008 to 5.7
elong2[1,4] <- 5.7 #if you know the row and column
elong2[elong2$ID == 373, ]$X2008 <- 5.7 #or if you know the value you want to change

#Creating a factor
str(elong2)
elong2$zone <- as.factor(elong2$zone) 

#Levels 
levels(elong2$zone) #shows the different factor levels
levels(elong2$zone) <- c("A", "B", "C", "D", "E", "F") #changes the factor levels to A-F


#Tidying the data ----
#To Wide/Long formats:
elongation_long <- gather(elongation, Year, Length, #gathers these columns in this order
                          c(X2007, X2008, X2009, X2010, X2011, X2012)) #specifying which columns
elongation_long2 <- gather(elongation, Year, Length, c(3:8))
elongation_wide <- spread(elongation_long, Year, Length) #this does the opposite (from long to wide format)

#Boxplot
boxplot(Length ~ Year, data = elongation_long,
        xlab = "Year", 
        ylab = "Elongation (cm)",
        main = "Annual growth of Empetrum hermaphroditum")

#Rename
elongation_long <- rename(elongation_long, zone = Zone, indiv = Indiv, year = Year, length = Length) #renamed = old name

#Filter
elong_subset <- filter(elongation_long, zone %in% c(2, 3), year %in% c("X2009", "X2010", "X2011")) #filters for zones 2 and 3 and years 2009-2011

#Removing a column
elong_no.zone <- dplyr::select(elongation_long, indiv, year, length)
elong_no.zone <- dplyr::select(elongation_long, -zone) #the minus sign removes the column; does the same as above

#Rename and reorder
elong_no.zone <- dplyr::select(elongation_long, Year = year, Shrub.ID = indiv, Growth = length)

#Mutate
elong_total <- mutate(elongation, total.growth = X2007 + X2008 + X2009 + X2010 + X2011 + X2012)

#Group by
elong_grouped <- group_by(elongation_long, indiv)

#Summarise
summary1 <- summarise(elongation_long, total.growth = sum(length))
summary2 <- summarise(elong_grouped, total.growth = sum(length))
summary3 <- summarise(elong_grouped, total.growth = sum(length),
                                     mean.growth = mean(length),
                                     sd.growth = sd(length))

#Join dataframes by ID code. The column names are spelled differently, so we need to tell the function which columns match
experiment <- left_join(elongation_long, treatments, by = c("indiv" = "Indiv", "zone" = "Zone"))
experiment2 <- merge(elongation_long, treatments, by.x = c("zone", "indiv"), by.y = c("Zone", "Indiv"))  #in the case that the columns have the same names

#Boxplot
boxplot(length ~ Treatment, data = experiment)

#Challenge ----
dragons <- rename(dragons, turmeric = paprika) #renaming
dragons.2 <- mutate(dragons, tabasco = ifelse(species == 'hungarian_horntail', tabasco - 30, tabasco))
#This overwrites tabasco via: if the species is Hungarian Horntail, deduct 30 from the values in the (original) 
#tabasco column; if the species is NOT horntail (i.e. all other species), write the original values.

dragons_long <- gather(dragons, key = 'spice', value = 'plume', c('tabasco', 'jalapeno', 'wasabi', 'turmeric'))
dragons_long <- mutate(dragons_long, plume.m = plume/100) #from cm to m

horntail <- filter(dragons_long, species == 'hungarian_horntail') 
green <- filter(dragons_long, species == 'welsh_green')
shortsnout <- filter(dragons_long, species == 'swedish_shortsnout')

par(mfrow=c(1, 3)) 
boxplot(plume.m ~ spice, data = horntail,
        xlab = 'Spice', ylab = 'Length of fire plume (m)',
        main = 'Hungarian Horntail')


boxplot(plume.m ~ spice, data = green,
        xlab = 'Spice', ylab = 'Length of fire plume (m)',
        main = 'Welsh Green')


boxplot(plume.m ~ spice, data = shortsnout,
        xlab = 'Spice', ylab = 'Length of fire plume (m)',
        main = 'Swedish Shortsnout')





#Efficient data manipulation (https://ourcodingclub.github.io/tutorials/data-manip-efficient/)
#Dplyr and tidyr----
#Grouping, summarising, tallying, filtering via dplyr:
trees.subset <- trees %>%
                filter(CommonName %in% c('Common Ash', 'Rowan', 'Scots Pine')) %>%
                group_by(CommonName, AgeGroup) %>%
                tally()

#Summarise all
summ.all <- summarise_all(trees, mean)

#Case_when
vector <- c(4, 13, 15, 6) 
ifelse(vector < 10, "A", "B")
vector2 <- c("What am I?", "A", "B", "C", "D")
case_when(vector2 == "What am I?" ~ "I am the walrus",
          vector2 %in% c("A", "B") ~ "goo",
          vector2 == "C" ~ "ga",
          vector2 == "D" ~ "joob")

#Changing species names
unique(trees$LatinName)
trees.genus <- trees %>%
  mutate(Genus = case_when(   
    grepl("Acer", LatinName) ~ "Acer",
    grepl("Fraxinus", LatinName) ~ "Fraxinus",
    grepl("Sorbus", LatinName) ~ "Sorbus",
    grepl("Betula", LatinName) ~ "Betula",
    grepl("Populus", LatinName) ~ "Populus",
    grepl("Laburnum", LatinName) ~ "Laburnum",
    grepl("Aesculus", LatinName) ~ "Aesculus",
    grepl("Fagus", LatinName) ~ "Fagus",
    grepl("Prunus", LatinName) ~ "Prunus",
    grepl("Pinus", LatinName) ~ "Pinus",
    grepl("Sambucus", LatinName) ~ "Sambucus",
    grepl("Crataegus", LatinName) ~ "Crataegus",
    grepl("Ilex", LatinName) ~ "Ilex",
    grepl("Quercus", LatinName) ~ "Quercus",
    grepl("Larix", LatinName) ~ "Larix",
    grepl("Salix", LatinName) ~ "Salix",
    grepl("Alnus", LatinName) ~ "Alnus")
  )

trees.genus.2 <- trees %>%
                 tidyr::separate(LatinName, c("Genus", "Species"), sep = " ", remove = FALSE) %>%  
                 dplyr::select(-Species)

trees.genus <- trees.genus %>% 
               mutate(Height.cat = case_when(Height %in% c("Up to 5 meters", "5 to 10 meters") ~ "Short",
                                 Height %in% c("10 to 15 meters", "15 to 20 meters") ~ "Medium",
                                 Height == "20 to 25 meters" ~ "Tall"))

#Reordering
levels(trees.genus$Height.cat)
trees.genus$Height.cat <- factor(trees.genus$Height.cat,
                                 levels = c('Short', 'Medium', 'Tall'), 
                                 labels = c('SHORT', 'MEDIUM', 'TALL'))  
levels(trees.genus$Height.cat)

#ggplot
trees.five <- trees.genus %>%
  filter(Genus %in% c("Acer", "Fraxinus", "Salix", "Aesculus", "Pinus"))
(map.all <- ggplot(trees.five) +
    geom_point(aes(x = Easting, y = Northing, size = Height.cat, colour = Genus), alpha = 0.5) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))

tree.plots <-  
  trees.five  %>%      # the data frame
  group_by(Genus) %>%  # grouping by genus
  do(plots =           # the plotting call within the do function
       ggplot(data = .) +
       geom_point(aes(x = Easting, y = Northing, size = Height.cat), alpha = 0.5) +
       labs(title = paste("Map of", .$Genus, "at Craigmillar Castle", sep = " ")) +
       theme_bw() +
       theme(panel.grid = element_blank(),
             axis.text = element_text(size = 14),
             legend.text = element_text(size = 12),
             plot.title = element_text(hjust = 0.5),
             legend.position = "bottom"))

# You can view the graphs before saving them
tree.plots$plots
tree.plots %>%  
  do(.,
     ggsave(.$plots, filename = paste(getwd(), "/", "map-", .$Genus, ".png", sep = ""), 
            device = "png", height = 12, width = 16, units = "cm", path = "Plots"))

#Challenge
lon <- (max(trees.genus$Easting) - min(trees.genus$Easting))/2 + min(trees.genus$Easting)
lat <- (max(trees.genus$Northing) - min(trees.genus$Northing))/2 + min(trees.genus$Northing)
trees.genus <- trees.genus %>%
  mutate(Quadrant = case_when(
    Easting < lon & Northing > lat ~ 'NW',
    Easting < lon & Northing < lat ~ 'SW',
    Easting > lon & Northing > lat ~ 'NE',
    Easting > lon & Northing < lat ~ 'SE'))

ggplot(trees.genus) +
  geom_point(aes(x = Easting, y = Northing, colour = Quadrant)) +
  theme_bw()

#To remove the NA
trees.genus <- trees.genus %>%
  mutate(Quadrant = case_when(
    Easting <= lon & Northing > lat ~ 'NW',  #using inferior OR EQUAL ensures that no point is forgotten
    Easting <= lon & Northing < lat ~ 'SW',
    Easting > lon & Northing > lat ~ 'NE',
    Easting > lon & Northing < lat ~ 'SE'))

#Richness
sp.richness <- trees.genus %>%
  group_by(Quadrant) %>%
  summarise(richness = length(unique(LatinName)))

#Acer proportion
acer.percent <- trees.genus %>%
  group_by(Quadrant, Genus) %>%
  tally() %>%                      
  group_by(Quadrant) %>%           
  mutate(total = sum(n)) %>%       
  filter(Genus == 'Acer') %>%     
  mutate(percent = n/total) 

ggplot(acer.percent) +
  geom_col(aes(x = Quadrant, y = percent)) +
  labs(x = 'Quadrant', y = 'Proportion of Acer') +
  theme_bw()
