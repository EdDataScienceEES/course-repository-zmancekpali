##%#########################################################################%##
#                                                                             #
#                         Coding club W5 (18.10.2023)                         #
#                                                                             #
##%#########################################################################%##

#Libraries
library(agridat)
library(dplyr)
library(ggeffects)
library(ggplot2)
library(glmmTMB)
library(lme4) 
library(MCMCglmm)
library(MCMCvis)
library(sjPlot)
library(stargazer)
library(tidyverse)


#WD
setwd("~/") #erases previously set WDs
setwd("Personal repo - zmancekpali/Coding Club/Weeks 6 - 8 - Statistics") #sets a new one
getwd() #check that it's worked

#Data
apples <- agridat::archbold.apple
shag <- read.csv("shagLPI.csv", header = TRUE)
sheep <- agridat::ilri.sheep
toolik_plants <- read.csv("toolik_plants.csv")
Weevil_damage <- read.csv("Weevil_damage.csv")


#Linear models ----
#Model 1 - Apple Yield
#Inspection and wrangling
head(apples)
summary(apples)
apples$spacing2 <- as.factor(apples$spacing)

#Model
(apples.p <- ggplot(apples, aes(spacing2, yield)) +
    geom_boxplot(fill = "#CD3333", alpha = 0.8, colour = "#8B2323") +
    theme_classic() +  
    theme(axis.text.x = element_text(size = 12, angle = 0)) +
    labs(x = "Spacing (m)", y = "Yield (kg)"))

apples.m <- lm(yield ~ spacing2, data = apples)
summary(apples.m)

#Model 2 - Sheep
#Wrangling and inspection
sheep <- filter(sheep, ewegen == "R")
head(sheep) 

#Models
sheep.m1 <- lm(weanwt ~ weanage, data = sheep) 
summary(sheep.m1)    

sheep.m2 <- lm(weanwt ~ weanage*sex, data = sheep) #interaction of age and sex
summary(sheep.m2)

(sheep.p <- ggplot(sheep, aes(x = weanage, y = weanwt)) +
    geom_point(aes(colour = sex)) +                              
    labs(x = "Age at weaning (days)", y = "Wean weight (kg)") +
    stat_smooth(method = "lm", aes(fill = sex, colour = sex)) + 
    scale_colour_manual(values = c("#FFC125", "#36648B")) +
    scale_fill_manual(values = c("#FFC125", "#36648B")) +
    theme_classic())

#Checking assumptions
shapiro.test(resid(apples.m)) #test for normality of residuals; reject null (normally distributed residuals) if p < 0.05
bartlett.test(yield ~ spacing2, data = apples)  #test for homoscedascity; reject null (homoscedascity in the data) if p < 0.05

#Diagnostic plots
plot(apples.m)

#Generalised linear models (Poisson distribution) ----
#Wrangling
shag$year <- as.numeric(shag$year)  # transform year from character into numeric variable

#Visualisation
(shag.hist <- ggplot(shag, aes(pop)) + 
    geom_histogram(binwidth = 30) + 
    theme_classic())

#GLM
shag.m <- glm(pop ~ year, family = poisson, data = shag)
summary(shag.m)

#Scatterplot
(shag.p <- ggplot(shag, aes(x = year, y = pop)) +
    geom_point(colour = "#483D8B") +
    geom_smooth(method = glm, colour = "#483D8B", fill = "#483D8B", alpha = 0.6) +
    scale_x_continuous(breaks = c(1975, 1980, 1985, 1990, 1995, 2000, 2005)) +
    theme_classic() +
    labs(x = " ", y = "European Shag abundance"))

#Generalised linear models (Binomial distribution) ----
#Wrangling
Weevil_damage$block <- as.factor(Weevil_damage$block)

#Model
weevil.m <- glm(damage_T_F ~ block, family = binomial, data = Weevil_damage)
summary(weevil.m)




#Linear models ----
#Assumptions:
    #The residuals are normally and equally distributed.
    #The data points are independent of one another.
    #The relationship between the variables we are studying is actually linear.

#H0: Plant species richness has not changed over time at Toolik Lake.
#H1: Plant species richness has increased over time at Toolik Lake
#H2: Plant species richness has decreased over time at Toolik Lake

#Inspection and wrangling
head(toolik_plants)
str(toolik_plants)

toolik_plants <-
  toolik_plants %>%
  mutate(across(c(Site, Block, Plot), as.factor))

length(unique(toolik_plants$Site)) #5 sites

toolik_plants %>% group_by(Site) %>%
  summarise(block.n = length(unique(Block)))

toolik_plants %>% group_by(Block) %>%
  summarise(plot.n = length(unique(Plot))) #sample blocks within each site

unique(toolik_plants$Year) #4 years
length(unique(toolik_plants$Species)) #129 observations
unique(toolik_plants$Species) #not all species:
toolik_plants <- toolik_plants %>%
  filter(!Species %in% c("Woody cover", "Tube",
                         "Hole", "Vole trail",
                         "removed", "vole turds",
                         "Mushrooms", "Water",
                         "Caribou poop", "Rocks",
                         "mushroom", "caribou poop",
                         "animal litter", "vole poop",
                         "Vole poop", "Unk?"))
length(unique(toolik_plants$Species)) #115 species

#Species per plot per year
toolik_plants <- toolik_plants %>%
  group_by(Year, Site, Block, Plot) %>%
  mutate(Richness = length(unique(Species))) %>%
  ungroup()

(hist <- ggplot(toolik_plants, aes(x = Richness)) +
    geom_histogram() +
    theme_classic())

(hist2 <- ggplot(toolik_plants, aes(x = Relative.Cover)) +
    geom_histogram() +
    theme_classic())

#Model
plant_m <- lm(Richness ~ I(Year-2007), data = toolik_plants) #the (Year-2007) makes 2008 the first year (so the model estimates across the years)
summary(plant_m)
plot(plant_m) #doesn't meet the assumptions

#Hierarchical models (lme4)----
#Assumptions:
    #The residuals are normally and equally distributed.
    #The data points are independent of one another.
    #The relationship between the variables we are studying is actually linear.
    #Plots represent the spatial replication and years represent the temporal replication in our data.

#Assumptions not accounted for:
    #Spatial and temporal autocorrelation

plant_m_plot <- lmer(Richness ~ I(Year-2007) + (1|Site), data = toolik_plants)
summary(plant_m_plot)

#Nested:
plant_m_plot2 <- lmer(Richness ~ I(Year-2007) + (1|Site/Block), data = toolik_plants) #nested
summary(plant_m_plot2)

plant_m_plot3 <- lmer(Richness ~ I(Year-2007) + (1|Site/Block/Plot), data = toolik_plants)
summary(plant_m_plot3)

plot(plant_m_plot3) 

#Visualising the effect sizes:
(re.effects <- plot_model(plant_m_plot3, type = "re", show.values = TRUE)) #random effects
(fe.effects <- plot_model(plant_m_plot3, show.values = TRUE)) #fixed effect

#Temperature effects
plant_m_temp <- lmer(Richness ~ Mean.Temp + (1|Site/Block/Plot) + (1|Year),
                     data = toolik_plants)
summary(plant_m_temp)
(temp.fe.effects <- plot_model(plant_m_temp, show.values = TRUE)) #high uncertainty about the effect of temperature on richness
(temp.re.effects <- plot_model(plant_m_temp, type = "re", show.values = TRUE))

#Random slopes vs random intercepts
plant_m_rs <- lmer(Richness ~ Mean.Temp + (Mean.Temp|Site/Block/Plot) + (1|Year),
                   data = toolik_plants)
summary(plant_m_rs) #doesn't converge

plant_m_rs <- lmer(Richness ~ Mean.Temp + (Mean.Temp|Site) + (1|Year),
                   data = toolik_plants)
summary(plant_m_rs)
(plant.fe.effects <- plot_model(plant_m_rs, show.values = TRUE))
(plant.re.effects <- plot_model(plant_m_rs, type = "re", show.values = TRUE))
ggpredict(plant_m_rs, terms = c("Mean.Temp", "Site"), type = "re") %>% plot() +
  theme(legend.position = "bottom")

predictions <- ggpredict(plant_m_rs, terms = c("Mean.Temp"))

(pred_plot1 <- ggplot(predictions, aes(x, predicted)) +
    geom_line() +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
    scale_y_continuous(limits = c(0, 35)) +
    labs(x = "\nMean annual temperature", y = "Predicted species richness\n")) #a more honest graph

predictions_rs_ri <- ggpredict(plant_m_rs, terms = c("Mean.Temp", "Site"), type = "re")

(pred_plot2 <- ggplot(predictions_rs_ri, aes(x = x, y = predicted, colour = group)) +
    stat_smooth(method = "lm", se = FALSE)  +
    scale_y_continuous(limits = c(0, 35)) +
    theme(legend.position = "bottom") +
    labs(x = "\nMean annual temperature", y = "Predicted species richness\n")) #more honest random slope graph 




