#Field course code


# Creating diversity for plants and insects -----------------------------------------------------------------
# (using vegan package and raw data)

library(tidyverse)
library(vegan)
library(readxl)

raw_test <- read_excel("bugs.xlsx")

#insects
as.data.frame(colnames(raw_test))
insects <- raw_test[, 47:63] %>% 
  mutate(hymenoptera = hymenoptera + Ants) %>% 
  select(Ants)
insects[is.na(insects)] <- 0
insects$quad <- as.factor(raw_test$quad)
insects$transect_ID <- as.factor(raw_test$transect_ID)

insect_div <- data.frame(quad = insects$quad,
                         transect_ID = insects$transect_ID,
                         insect_div = diversity(as.matrix(insects[, -c(17:18)])))

ggplot(insect_div, aes(x = quad, y = insect_div)) +
  geom_boxplot()

insect_lm <- lm(insect_div ~ quad, data = insect_div)
summary(insect_lm)
anova(insect_lm)

#plants
plants <- raw_test[, c(14:26,29:44)]
plants[is.na(plants)] <- 0
plants$quad <- as.factor(raw_test$quad)
plants$transect_ID <- as.factor(raw_test$transect_ID)
plants <- plants[-14,]

plant_div <- data.frame(quad = plants$quad, 
                        transect_ID = plants$transect_ID, 
                        plant_div = diversity(as.matrix(plants[, -c(30:31)])))

ggplot(plant_div, aes(x = quad, y = plant_div)) +
  geom_boxplot()

plants_lm <- lm(plant_div ~ quad, data = plant_div)
summary(plants_lm)
anova(plants_lm)

#joining all together
all_data <- raw_edge %>% 
  mutate(quad = as.factor(quad)) %>% 
  select(1:13) %>% 
  left_join(insect_div) %>% 
  left_join(plant_div)

all_data$insect_div <- insect_div$div
plant_div[1:13,]

# Creating graphs for poster and models -----------------------------------------------------------------

# relationship between plant diversity and distance

(plantD_boxplot <- ggplot(all_data, aes(x = factor(quad), y = plant_div)) +     
   geom_boxplot(aes(fill = factor(quad))) +  # Convert QUAD to a factor here
   theme_bw() +   
   geom_point()+
   stat_boxplot(geom ='errorbar') + 
   scale_fill_manual(values = c("#009E73", "#56B4E9", "#F0E442", "#56B4E9", "#009E73")) +                   
   scale_colour_manual(values = c("#009E73", "#56B4E9", "#F0E442", "#56B4E9", "#009E73")) +  
   ylab("Plant Shannons Diversity (H')") +                                
   xlab("\nPosition")  +   
   theme(axis.text = element_text(size = 12),         
         axis.title = element_text(size = 14, face = "plain"),                      
         panel.grid = element_blank(),                                                     
         plot.margin = unit(c(1,1,1,1), units = "cm"),  # Added units argument                   
         legend.position = "none")) 

# relationship between insect diversity and distance

(plantD_boxplot <- ggplot(all_data, aes(x = factor(quad), y = insect_div)) +     
    geom_boxplot(aes(fill = factor(quad))) +  # Convert QUAD to a factor here
    theme_bw() +  
    geom_point()+
    stat_boxplot(geom ='errorbar') + 
    scale_fill_manual(values = c("#009E73", "#56B4E9", "#F0E442", "#56B4E9", "#009E73")) +                   
    scale_colour_manual(values = c("#009E73", "#56B4E9", "#F0E442", "#56B4E9", "#009E73")) +  
    ylab("Invertebrate Shannons Diversity (H')") +                                
    xlab("\nPosition")  +   
    theme(axis.text = element_text(size = 12),         
          axis.title = element_text(size = 14, face = "plain"),                      
          panel.grid = element_blank(),                                                     
          plot.margin = unit(c(1,1,1,1), units = "cm"),  # Added units argument                   
          legend.position = "none")) 

# relationship between PAR and distance

(plantD_boxplot <- ggplot(all_data, aes(x = factor(quad), y = par)) +     
    geom_boxplot(aes(fill = factor(quad))) +  # Convert QUAD to a factor here
    theme_bw() +  
    geom_point()+
    stat_boxplot(geom ='errorbar') + 
    scale_fill_manual(values = c("#009E73", "#56B4E9", "#F0E442", "#56B4E9", "#009E73")) +                   
    scale_colour_manual(values = c("#009E73", "#56B4E9", "#F0E442", "#56B4E9", "#009E73")) +  
    ylab("Light Levels (PPFD)") +                                
    xlab("\nPosition")  +   
    theme(axis.text = element_text(size = 12),         
          axis.title = element_text(size = 14, face = "plain"),                      
          panel.grid = element_blank(),                                                     
          plot.margin = unit(c(1,1,1,1), units = "cm"),  # Added units argument                   
          legend.position = "none")) 



################ scactterplot for plant diversity and invert diversity relationship 
plot(insect_div ~ plant_div, data= all_data, xlab="Plant Shannons Diversity (H')", ylab= "Invertebrate Shannons Diversity (H')", col= "#56B4E9", pch = 19)
abline(lm(insect_div ~ plant_div, data= all_data))

#### spearmans for plants and insect diversity to see if relationship instead of anova. 
cor.test(all_data$insect_div, all_data$plant_div, data= all_data, method= 'spearman', exact= FALSE)
# use chat gpt to explain result of this 





####### MODELS



# does distance explain plant diversity= NOT SIGNIFICANT
all_data$quad <- as.factor(all_data$quad)
plot(plant_div ~ quad, data= all_data)
mod8 <- lm(plant_div ~ quad, data= all_data) 
anova(mod8)# not significant
summary(mod8)
plot(mod8)
all_data$quad <- as.numeric(all_data$quad)

# does distance explain invert diversity = NOT SIGNIFICANT
all_data$quad <- as.factor(all_data$quad)
plot(insect_div ~ quad, data= all_data)
mod9 <- lm(insect_div ~ quad, data= all_data) 
anova(mod9) # not signifcant
plot(mod9)
all_data$quad <- as.numeric(all_data$quad)


# does distance explain par
all_data$quad <- as.factor(all_data$quad)
plot(par ~ quad, data= all_data)
mod10 <- lm(par ~ quad, data= all_data) 
anova(mod10) # not signifcant
plot(mod10)
all_data$quad <- as.numeric(all_data$quad)