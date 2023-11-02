##%#########################################################################%##
#                                                                             #
#                 Data science W6 - 8 (26.10.2023 - 9.11.2023)                #
#                                                                             #
##%#########################################################################%##

#Libraries
library(brms)
library(broom)
library(broom.mixed)
library(ggeffects)
library(ggfortify)
library(lme4)
library(MCMCglmm)
library(tidyverse)

#WD
setwd("~/") #erases previously set WDs
setwd("Personal repo - zmancekpali/Weekly DS content/Weeks 6 - 8") #sets a new one
getwd() #check that it's worked

#Data
toolik_plants <- read_csv("toolik_plants.csv")

#Inspection + wrangling ----
head(toolik_plants)
str(toolik_plants)

toolik_plants$Plot <- as.factor(as.character(toolik_plants$Plot))

length(unique(toolik_plants$Site)) #5 sites

unique(toolik_plants$Year) #4 years

#Remove non-species
toolik_plants <- toolik_plants %>%
  filter(!Species %in% c("Woody cover", "Tube",
                         "Hole", "Vole trail",
                         "removed", "vole turds",
                         "Mushrooms", "Water",
                         "Caribou poop", "Rocks",
                         "mushroom", "caribou poop",
                         "animal litter", "vole poop",
                         "Vole poop", "Unk?"))

#Calculate species richness
toolik_plants <- toolik_plants %>%
  group_by(Year, Site, Block, Plot) %>%
  summarise(Richness = length(unique(Species))) %>% 
  ungroup()

#Check distribution
(hist2 <- ggplot(toolik_plants, aes(x = Richness)) +
    geom_histogram() +
    theme_classic())

#Make a new column with a simplified year variable for use in models (2008 becomes year 1)
toolik_plants$Year_simple <- toolik_plants$Year-2007

#Week 6: Linear models ####
#Gaussian family
#Richness over time
mod1 <- lm(Richness ~ Year_simple, data = toolik_plants)
shapiro.test(resid(mod1)) #non-normally distributed residuals
bartlett.test(Richness ~ Year_simple, data = toolik_plants) #heteroscedascity
autoplot(mod1) #this model should not be used to draw any conclusions
(model.table1 <- as.data.frame(tidy(mod1))) #tidying model outptut (v cool)

glm1 <- glm(Richness ~ Year_simple, data = toolik_plants, family = gaussian)
autoplot(glm1) #looks bad
summary(glm1)
(model.table2 <- as.data.frame(tidy(glm1)))

#How does plant richness change over time?
plot(Richness ~ Year_simple, data = toolik_plants) #richness seems to decrease

#What is the rate of change?
coef(mod1) #-0.5398444; richness does decrease with time

#What is the statistical power of the model? sample size = ? number of plots = ?

#What is the predicted species richness in the year 2013?
pred = data.frame(Year_simple = 6)
predict(mod1, pred, type = "response") #16.14413; linear model
predict(glm1, pred, type = "response") #16.20646; glm1


#Trying a different family
glm2 <- glm(Richness ~ Year_simple, data = toolik_plants, family = poisson)
autoplot(glm2) #looks bad
summary(glm2) #lower AIC
(model.table3 <- as.data.frame(tidy(glm2)))

#How has this changed the model diagnostic plots? Does it look better?

#How has this changed the model estimates? (The slope and intercept) 

#Compare these numbers to the numbers from the Gaussian model. Why might there be differences?

#Write a summary sentence of how plant richness has changed over time

#What is the rate of change?

#Which model do you trust more?!
null_model <- lm(Richness ~ 1, data = toolik_plants)
AIC(null_model, mod1, glm1, glm2) #the latter two have lower AIC scores -> fit the data better

#What is the predicted species richness in the year 2013?
predict(glm2, pred, type = "response") #16.20646; glm2; identical to glm1


#Week 7: Hierarchical models -----
