##%#########################################################################%##
#                                                                             #
#                             Coding club W6 - 8                              #
#                         (25.10.2023 - 8.11.2023)                            #
#                                                                             #
##%#########################################################################%##

#WD
setwd("~/") #erases previously set WDs
setwd("Personal repo - zmancekpali/Coding Club/Weeks 6 - 8 - Statistics") #sets a new one
getwd() #check that it's worked

#Libraries
library(agridat)
library(brms) 
library(dplyr)
library(ggeffects)
library(ggplot2)
library(glmmTMB)
library(lme4) 
library(MCMCglmm)
library(MCMCvis)
library(sjPlot)
library(stargazer)
library(tidybayes)
library(tidyverse)

#Data
apples <- agridat::archbold.apple
shag <- read.csv("Data/shagLPI.csv", header = TRUE)
sheep <- agridat::ilri.sheep
toolik_plants <- read.csv("Data/toolik_plants.csv")
Weevil_damage <- read.csv("Data/Weevil_damage.csv")


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


#Bayesian modelling ----
France <- read_csv("red_knot.csv")

head(France) 
str(France)

(hist_france <- ggplot(France, aes(x = pop)) +
    geom_histogram(colour = "#8B5A00", fill = "#CD8500") +
    theme_bw() +
    ylab("Count\n") +
    xlab("\nCalidris canutus abundance") +  # latin name for red knot
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain")))     

unique(France$year) #35 years

france1_mbrms <- brms::brm(pop ~ I(year - 1975),
                           data = France, family = poisson(), chains = 3,
                           iter = 3000, warmup = 1000)
summary(france1_mbrms)
#The interesting part is what is written under Population-Level Effects. The model gives us an 
#Estimate aka the mean of our posterior distribution for each variable. As explained earlier, these estimates 
#can be used as the intercept and slope for the relationship between our two variables. Est.Error is the error 
#associated with those means (the standard error). The other important part of that summary is the 95%
#Credibility Interval (CI), which tells us the interval in which 95% of the values of our posterior distribution 
#fall. The thing to look for is the interval between the values of l-95% CI and u-95% CI. If this interval is 
#strictly positive or negative, we can assume that the effect is significant (and positive or negative respectively). 
#However, if the interval encompasses 0, then we can’t be sure that the effect isn’t 0, aka non-significant. In addition, 
#the narrower the interval, the more precise the estimate of the effect. In our case, the slope 95% CI does not 
#encompass 0 and it is strictly positive, so we can say that time has a significantly positive effect on red knot abundance.

plot(france1_mbrms)
#We call this the trace or caterpillar plots. If you focus on the right hand plots, you want to see a sort of 
#fuzzy caterpillar, or a festive tinsel. If this is the case, it means your model explored all the possible 
#values it could look at, so it convergedwell. On the x-axis of those trace plots, we have the iterations done after the warmup 
#(so 3000-1000 = 2000 in our case). And on the y-axis are all the values of the mean of the posterior 
#distribution that have been assessed by our model. On the left side, the density plots shows all of those mean 
#values again, plotted by the amount of times the model got this value (so the distribution of means basically). 
#And if you look closely, the mean of this density plot is going to be the mean value that has been found by the 
#model most often, so probably the most “correct” one. And that value should be very close to the actual estimate
#that the summary function gave us. In our case, the top plot is the intercept and that density plot seems to be 
#centered around 8.70, which is the estimate value that we got in the summary!

pp_check(france1_mbrms) #similar enough

#adding random effects
france2_mbrms <- brms::brm(pop ~ I(year - 1975) + (1|year),
                           data = France, family = poisson(), chains = 3,
                           iter = 3000, warmup = 1000)
summary(france2_mbrms)
plot(france2_mbrms) #looks good

unique(France$Location.of.population)  #observations come from 2 locations

(boxplot_location <- ggplot(France, aes(Location.of.population, pop)) +
    geom_boxplot() +  #could be a significant effect between locations so should look at that
    theme_bw() +
    xlab("Location\n") +
    ylab("\nCalidris canutus abundance") +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"))) #difference between the sites

france3_mbrms <- brms::brm(pop ~ I(year - 1975) + Location.of.population,
                           data = France, family = poisson(), chains = 3,
                           iter = 3000, warmup = 1000)
summary(france3_mbrms)
plot(france3_mbrms)
pp_check(france3_mbrms)
loo(france1_mbrms,france2_mbrms, france3_mbrms, compare = TRUE) #the one that = 0 is the best (aka france3_mbrms)

(model_fit <- France %>%
    add_predicted_draws(france3_mbrms) %>%  # adding the posterior distribution
    ggplot(aes(x = year, y = pop)) +  
    stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50),  # regression line and CI
                    alpha = 0.5, colour = "black") +
    geom_point(data = France, colour = "darkseagreen4", size = 3) +   # raw data
    scale_fill_brewer(palette = "Greys") +
    ylab("Calidris canutus abundance\n") +  # latin name for red knot
    xlab("\nYear") +
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.position = c(0.15, 0.85)))

(location_fit <- France %>%
    group_by(Location.of.population) %>%
    add_predicted_draws(france3_mbrms) %>%
    ggplot(aes(x = year, y = pop, color = ordered(Location.of.population), fill = ordered(Location.of.population))) +
    stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50), alpha = 1/4) +
    geom_point(data = France) +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Dark2") +
    theme_bw() +
    ylab("Calidris canutus abundance\n") +
    xlab("\nYear") +
    theme_bw() +
    theme(legend.title = element_blank()))
