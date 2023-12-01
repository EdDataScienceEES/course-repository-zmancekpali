##%#########################################################################%##
#                                                                             #
#                          Data science W4 (12.10.2023)                       #
#                                                                             #
##%#########################################################################%##

#Good practice for data wrangling
#First, organise the input data: each row is a separate observation and each variable has a separate column
    #No merged cells, special characters, or colours; definitely in csv or txt format
#Next, tidy your data: 
    #Often have to change from a wide to a long format (so you can analyse it)
    #Sort names (keep naming style consistent; I'll use name_name)
    #Make sure columns are in the correct format (numeric, character, or factor)
    #Figure out workflow

#WD
setwd("~/") #erases previously set WDs
setwd("Personal repo - zmancekpali/Weekly DS content/Week 4") #sets a new one
getwd() #check that it's worked

#Libraries:
library(tidyverse)

#Data:
load("/Users/zojamancekpali/DS - Personal repo - Zoja/Weekly DS Content/Week 4/LPI_species.Rdata")

#Questions to answer in this session:

#Rank all of the biomes from most to least well represented with populations monitored? 
levels(LPI_species$biome)
biome_name <- unique(LPI_species$biome)
length(unique(LPI_species$biome)) #17 different biomes

biomes <- LPI_species %>% group_by(biome) %>% 
                          summarize(meanpop = mean(pop)) %>% 
                          arrange(meanpop) %>% 
                          mutate(rank = row_number()) %>% 
                          ungroup()

(barplot <- ggplot(biomes, aes(x = reorder(biome, -meanpop), y = meanpop)) +
  geom_bar(stat = "identity", fill = "pink3") +
  theme_minimal() +
  xlab("Biome") +
  ylab("Mean Population") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))

#What is the biome with the most different genera monitored?
LPI_species <- LPI_species %>% tidyr::separate(Species, into = c("Genus", "Species"), sep = ' ')

(biomes_genera <- LPI_species %>% group_by(biome) %>% 
                                 summarize(unique_genera = n_distinct(Genus)) %>% 
                                 arrange(unique_genera))

(barplot <- ggplot(biomes_genera, aes(x = reorder(biome, -unique_genera), y = unique_genera)) +
    geom_bar(stat = "identity", fill = "pink3") +
    theme_minimal() +
    xlab("Biome") +
    ylab("Number of unique genera") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))



