##%#########################################################################%##
#                                                                             #
#                 Data science W6 - 8 (26.10.2023 - 9.11.2023)                #
#                                                                             #
##%#########################################################################%##

#WD
setwd("~/") #erases previously set WDs
setwd("Personal repo - zmancekpali/Weekly DS content/Weeks 6 - 8") #sets a new one
getwd() #check that it's worked

#Libraries
library(brms)
library(broom)
library(broom.mixed)
library(ggeffects)
library(ggfortify)
library(lme4)
library(MCMCglmm)
library(sjPlot)
library(StatisticalModels)
library(tidyverse)
remotes::install_github("timnewbold/StatisticalModels")

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
mod1 <- lm(Richness ~ Year_simple, data = toolik_plants)
summary(mod1)
#p-value = 0.00241 and estimate = -0.5390
#species richness decreases by a value of 0.5 every year. 

#Y1
19.3870 + 1*-0.5390 # = 18.848
#Y2
19.3870 + 2*-0.5390 # = 18.309
#Y5
19.3870 + 5*-0.5390 #16.692

ggpredict(mod1, terms = c("Year_simple"))

coef(mod1)
autoplot(mod1)
shapiro.test(resid(mod1)) #non-normally distributed residuals
bartlett.test(Richness ~ Year_simple, data = toolik_plants) #heteroscedascity

hist(toolik_plants$Richness)

mod2 <- glm(Richness ~ Year_simple, data = toolik_plants, family = poisson)
summary(mod2)
autoplot(mod2)

#Y1
2.966900 + 1*-0.030160
exp(2.93674) #18.85428

#Y2
2.966900 + 3*-0.030160
exp(2.87642) #17.75061

#Y3
2.966900 + 5*-0.030160
exp(2.8161) #16.71155

ggpredict(mod2, terms = c("Year_simple"))

(415.49-402.47)/415.49 #0.03133649; about 3% of the variance is explained by the model  

mod3 <- glmer(Richness ~ Year_simple + (1|Site/Block), family = poisson, 
              data = toolik_plants)

summary(mod3)
R2GLMER(mod3)
#$conditional - 0.0851222 - amount explained by both fixed and random effects
#$marginal - 0.001844425 - amount explained by just fixed effects

plot_model(mod3, type = "re", show.values = TRUE)

#Week 8: Bayesian statistics ----
plant_mbrms <- brm(Richness ~ Year_simple + (1|Site),  #random intercept (random slope (year | Site))
                   data = toolik_plants, family = poisson()) #this takes a long time to run
save(plant_mbrms, file = "plant_mbrms.Rdata")
summary(plant_mbrms)
#Species richness is decreasing slightly (coefficient is -0.03)
#No p-value given from these models
##Is species richness declining? 
#Intercept: start with 3 species and 
exp(-0.03)
#0.9704455
#Do we see a 'p-value'? No 
#Do the CIs span zero? No
#Compute errors for you: does the maths: value of the lowest --> don't include zero in its range 
#Is the model 'significant'? No with the CI
#If parameter significantly affecting the response: very sure so distribution is very narrow 
#Did the model converge? No 

ggpredict(plant_mbrms, terms = c("Year_simple")) %>% plot()
#Credible interval: so large 
#Primary plot you'd want to show 

plot(conditional_effects(plant_mbrms))

hier_prior <- c(set_prior(prior = 'normal(0,6)', class='b', coef='Year_simple'), 	# global slope
                set_prior(prior = 'normal(0,6)', class='Intercept', coef=''), 		# global intercept
                set_prior(prior = 'cauchy(0,2)', class='sd'))							# group-level intercepts and slopes
#Y = B1X1+C: X1 year & B1 coeeficient & C intercept 

plant_mbrms2 <- brm(Richness ~ Year_simple + (1|Site),
                    family = 'poisson', data = toolik_plants,
                    prior = hier_prior, iter = 2000,
                    warmup = 500,
                    init = '0',
                    control = list(adapt_delta = 0.80),
                    cores = 2, chains = 2)

summary(plant_mbrms2)

#how many iterations? 2000
#how does this code differ from the earlier brms model? it has manually defined priors 
#does this model have random intercepts? yes 
#doest this model have random slopes? no 

ggpredict(plant_mbrms, terms = c("Year_simple")) %>% plot()
plot(conditional_effects(plant_mbrms2))

#Extract slopes for each cell
slopes_plants <- as.data.frame(coef(plant_mbrms2))
#can analyse it by looking at significance 
#more or less effect #
#can include prior information: most basics: family distribution & also (power of Bayesian stat): prior info on data -> coefficients 
#can change the adapt delta -> send to the url 
#simplifying model also helped 

