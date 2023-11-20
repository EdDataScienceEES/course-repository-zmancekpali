##%#########################################################################%##
#                                                                             #
#                           Data science Challenge 3                          #
#                               Zoja Manček Páli                              #
#                                                                             #
##%#########################################################################%##

#WD
setwd("~/") #erases previously set WDs
setwd("Challenge 3/Challenge") #sets WD to this folder
getwd() #check that it's worked

#Libraries
library(brms)
library(car)
library(ggeffects)
library(gridExtra)
library(lme4)
library(lmtest)
library(lattice)
library(MASS)
library(mgcv)
library(sjPlot)
library(tidyverse)
library(tidybayes)

#Data
load("LPI_data.Rdata")

#Select species
unique(data$Common.Name) #I selected the Orca

#Wrangle the data
orca <- data %>% 
  filter(Common.Name == "Killer whale / Orca",
         Country.list %in% c("Canada", "United States")) %>% 
  pivot_longer(cols = 25:69, names_to = "Year", values_to = "Abundance") %>% 
  filter(Units %in% c("Number observed", "total population", "No. of Individuals", 
                      "No.of Individuals", "Number of individuals", 
                      "Number of sightings","Estimated density"),
         !is.na(Abundance),
         Location.of.population != "Inside Passage, between the British Columbia-Washington and the British Columbia-Alaska borders") %>% 
  mutate(Year = as.numeric(sub("^X", "", Year)),
         Abundance = as.numeric(ifelse(Abundance == "NULL", NA, Abundance))) %>% 
  select(id, Location.of.population, Country.list, Units, Year, Abundance) %>% 
  mutate(Location.of.population = recode(Location.of.population, 
         "Northern residents, British Columbia" = "N. residents, BC",
         "Southern residents, British Columbia" = "S. residents, BC", 
         "Prince William Sound, Alaska, AT1 population" = "AT1, Alaska",
         "Prince William Sound, Alaska, AB Pod population" = "AB, Alaska",
         "Prince William Sound / Kenai Fjords, Alaska. PWS/KF population (7 pods)" = 
           "PWS/KF, Alaska", 
         "Prince William Sound / Kenai Fjords, Alaska. Southeastern Alaska population (3 pods)" = 
           "SE Alaska", 
         "East North Pacific Resident stock" = "NE Pacific resident")) %>%  #recode the names for ease
  mutate(year = I(Year-1970))
  
  
#Exploring the data ----
#Distribution histogram
png("Plots/orca_hist.png", width = 800, height = 600) #saves the plot as a png
orca_seq <- seq(min(orca$Abundance, na.rm = TRUE), max(orca$Abundance, na.rm = TRUE), 
                length = 212)
fun <- dnorm(orca_seq, mean = mean(orca$Abundance, na.rm = TRUE), 
             sd = sd(orca$Abundance, na.rm = TRUE))
orca_hist_data <- hist(orca$Abundance, breaks = 25)
scaled_density <- fun * sum(orca_hist_data$counts) * diff(orca_hist_data$breaks[1:2])
(orca_hist <- hist(orca$Abundance, breaks = 25, 
                   col = yarrr::transparent("navyblue", trans.val = .6),
     xlab = "Orca abundance",
     ylab = "Count",
     main = "Orca abundance histogram"))
lines(orca_seq, scaled_density, col = "red", lwd = 4) #appears to be a Poisson distribution
png("Plots/orca_hist.png", width = 800, height = 600) #saves the plot as a png
dev.off() #necessary to run if you want any other plots to show later

shapiro.test(orca$Abundance) #non-normal

#Random effects exploration
(location_bp <- ggplot(orca, aes(x = Abundance, y = Location.of.population)) +
  geom_boxplot(color = "navyblue") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +  # Adjust the angle as needed
  labs(y = "Location of Population", x = "Abundance", 
       title = "Abundance Variation Between Sites") +
  theme_classic() +
  theme(legend.position = 'none')) #we see clear variation between the sites
ggsave("location_bp.png", location_bp, path = "Plots", units = "cm", height = 30, 
       width = 50)

(country_bp <- ggplot(orca, aes(x = Abundance, y = Country.list)) +
    geom_boxplot(color = "navyblue") +
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) +  # Adjust the angle as needed
    labs(y = "Country of Population", x = "Abundance", 
         title = "Abundance Variation Between Sites") +
    theme_classic() +
    theme(legend.position = 'none')) #we see clear variation between the sites
ggsave("country_bp.png", country_bp, path = "Plots", units = "cm", height = 30, 
       width = 30)

(site_grid <- grid.arrange(location_bp, country_bp, ncol = 2))
ggsave("site_grid.png", site_grid, path = "Plots", units = "cm", height = 20, 
       width = 50)


#Data visualisation  ----
(general_plot <- ggplot(orca, aes(x = Year, y = Abundance)) +
  geom_point(color = "navyblue") +
  geom_smooth(color = "red") +
  labs(x = "Year", y = "Abundance") +
  theme(legend.position = "top") +
  theme_classic()) #we can see that the trend is very unclear (clearly influenced by something other than time)
ggsave("general_trend.png", general_plot, path = "Plots", units = "cm", height = 20,
       width = 30)  #increase font sizes for these later

(by_site_1 <- ggplot(orca, aes(x = Year, y = Abundance, color = Location.of.population)) +
  geom_point() +
  geom_line() + 
  geom_smooth() +
  labs(x = "Year", y = "Abundance") +
  theme_classic() + 
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("#B72EE8", "#269642", "#D98B32", "#B32422", "#8266C4", "#2C13D1", "#3BC412", "#1B8EB5", "#C4CC2F"))) #by examining the data this way, we can see significant variation between the sites in Canada
ggsave("trends_by_site1.png", by_site_1, path = "Plots", units = "cm", height = 25,
       width = 40) #increase font sizes for these later

(trends_grid <- grid.arrange(general_plot, by_site_1, ncol = 2))
ggsave("trends_grid.png", trends_grid, path = "Plots", units = "cm", height = 25,
       width = 50) #increase font sizes for these later



#Modelling ----
#Generalised linear mixed effects models ----
model_example <- glmer(Abundance ~ Year + Country.list + (1|Location.of.population) + 
                         (1|Year), family = poisson, data = orca)
#The model on its own like this isn't identifiable (i.e. there is numerical instability in the model),
#so we can round (to integers) the abundance values and scale the year values and try again.
orca$abundance <- round(orca$Abundance) #round values to integers
orca$year <- scale(orca$Year) #scale the year values to allow for identifiable models (in a new column)

#See below for each model and the associated null models.

#MODEL 1: tests the location of each orca population and the year as random effects.
model1 <- glmer(abundance ~ year + Country.list + (1|Location.of.population) + (1|year), 
                family = poisson, data = orca)
null_mod_1 <- glmer(abundance ~ 1 + (1 | Location.of.population) + (1 | year), 
                    family = poisson, data = orca)

#I've included year as a fixed and random effect (in model 1) to account for variability 
#within and between the years, however, I would still like to compare the model to one 
#without year as a random effect in their ability to explain the variation within the data.

#MODEL 2: tests the location of each orca population as a random effect .
model2 <- glmer(abundance ~ year + Country.list + (1|Location.of.population), family = poisson, 
                data = orca)
null_mod_2 <- glmer(abundance ~ 1 + (1|Location.of.population), family = poisson, 
                    data = orca)

#Now that we have all 4 of the models, we can first compare them for their ability to explain
#the data without over fitting via the AIC:
(aic <- AIC(null_mod_1, model1, null_mod_2, model2))
#From these outputs, we can see that model1 has the lowest AIC score, meaning it fits the data best without overfitting

#We can now test the relationships:
summary(model1) #year has a significant effect on orca abundance; country does not

(residual_plot_fixed <- plot_model(model1, show.values = TRUE) +
    theme_classic()) 
ggsave("fixed_effect_plot_model1.png", residual_plot_fixed, path = "Plots", units = "cm",
       width = 30, height = 20)

#Relating these back to my RQs we could conclude that:
#Orca abundance increases over time (on average) in North American marine habitats; significant effect (positive intercept; 0.3718)
#The abundance of O. orca is increasing over time in North American marine habitats (but country has no effect).
#The random effects account for variability between different years and locations within North American marine habitats

#But before we can conclude this way, we need to evaluate the model further:
(linearity_check <- plot(ggpredict(model1, terms = c("year"))) + 
    theme_classic())
ggsave("linearity_check.jpg", linearity_check, path = "Plots", units = "cm",
       width = 30, height = 20) #the relationship doesn't look linear

homoscedascity_data <- data.frame(
  Fitted_values = predict(model1),
  Residuals = residuals(model1))

(homoscedascity_check <- ggplot(data = homoscedascity_data, aes(x = predict(model1), y = residuals(model1))) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  xlab("Fitted values") +
  ylab("Residuals") +
  ggtitle("Homoscedasticity check") +
  theme_classic())
ggsave("homoscedascity_check.jpg", homoscedascity_check, path = "Plots", units = "cm",
       width = 30, height = 20)

levene_model1 <- lm(Residuals ~ Fitted_values, data = homoscedascity_data) #ANOVA of the squared residuals
anova(levene_model1) #homoscedascity

(normality_check <- ggplot(data.frame(residuals = residuals(model1)), aes(sample = residuals)) +
    stat_qq() +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    labs(title = "Normal Q-Q Plot of Residuals") +
    theme_classic())
ggsave("normality_check.png", normality_check, path = "Plots", units = "cm",
       width = 30, height = 20) #non-normal distribution of residuals

shapiro.test(resid(model1)) #also confirms that residuals not distributed normally 
qqmath(model1, id = 0.001) #this shows the values that exert undue influence on the model

(assumptions_grid <- grid.arrange(linearity_check, homoscedascity_check, 
                                  normality_check, ncol = 3))
ggsave("assumptions_checks_grid.jpg", assumptions_grid, path = "Plots", units = "cm",
       width = 50, height = 20)

#The model does not meet the assumptions, so we need to transform/choose another framework.
#Because the linearity assumption was not met, I will try transforming the year value to account
#for the exponential-looking relationship:
#MODEL 3: tests the same relationship as model1 but tries to account for the non-linearity (via a square-root transformation):
#I was also unable to transform the abundance variable as it made the model not converge (AIC = infiniy)
model3 <- glmer(abundance ~ sqrt(year) + Country.list + (1|Location.of.population) + 
                  (1|year), family = poisson, data = orca)
null_mod_3 <- glmer(abundance ~ 1 + (1 | Location.of.population) + (1 | year), 
                    family = poisson, data = orca)


(aic2 <- AIC(model1, null_mod_1, model3, null_mod_3))

(residual_plot_model3 <- plot_model(model3, show.values = TRUE) +
    theme_classic()) 
ggsave("fixed_effect_plot_model3.png", residual_plot_model3, path = "Plots", units = "cm",
       width = 30, height = 20)

#the transformed model3 produces a much lower AIC score, so let's test the assumptions again:
(linearity_check_sqrt <- plot(ggpredict(model3, terms = c("year"))) + 
    theme_classic())
ggsave("linearity_check_sqrt.jpg", linearity_check_sqrt, path = "Plots", units = "cm",
       width = 30, height = 20) #looks more linear than before but still not what we want.

homoscedascity_data_sqrt <- data.frame(
  Fitted_values = predict(model3),
  Residuals = residuals(model3))

(homoscedascity_check_sqrt <- ggplot(data = homoscedascity_data_sqrt, aes(x = predict(model3), y = residuals(model3))) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    xlab("Fitted values") +
    ylab("Residuals") +
    ggtitle("Homoscedasticity check") +
    theme_classic())
ggsave("homoscedascity_check_sqrt.jpg", homoscedascity_check_sqrt, path = "Plots", units = "cm",
       width = 30, height = 20) 

levene_model3 <- lm(Residuals ~ Fitted_values, data = homoscedascity_data_sqrt) #ANOVA of the squared residuals
anova(levene_model3) #homoscedascity

(normality_check_sqrt <- ggplot(data.frame(residuals_sqrt = residuals(model3)), aes(sample = residuals_sqrt)) +
    stat_qq() +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    labs(title = "Normal Q-Q Plot of Residuals") +
    theme_classic())
ggsave("normality_check_sqrt.png", normality_check_sqrt, path = "Plots", units = "cm",
       width = 30, height = 20) #non-normal distribution of residuals

shapiro.test(resid(model3)) #also confirms that residuals not distributed normally (but just barely; the p-value = 0.046)
qqmath(model3, id = 0.01) #this shows the values that exert undue influence on the model

(assumptions_grid_sqrt <- grid.arrange(linearity_check_sqrt, homoscedascity_check_sqrt, 
                                  normality_check_sqrt, ncol = 3))
ggsave("assumptions_checks_sqrt_grid.jpg", assumptions_grid_sqrt, path = "Plots", units = "cm",
       width = 50, height = 20)


#While I could not make the model meet the assumptions, I will still discuss the results
#in the report (especially considering the shapiro test p-value is marginally significant). 
summary(model3)
model3_summary <- capture.output(summary(model3))
writeLines(model3_summary, "Tables/model3_summary.txt")

# Plot the observed vs predicted values
plot(prediction_data$observed, prediction_data$predicted,
     xlab = "Observed Abundance", ylab = "Predicted Abundance",
     main = "Observed vs Predicted Abundance",
     pch = 16, col = "blue")
abline(0, 1, col = "red") 

# Scatter plot of fitted vs. observed values
plot(predictions_data$Observed, predictions_data$Predicted,
     xlab = "Observed", ylab = "Predicted",
     main = "Fitted vs. Observed Values",
     pch = 16, col = "blue")

# Add a line representing a perfect fit
abline(a = 0, b = 1, col = "red", lty = 2)

#Generalised additive models ----
#However, I also attempt below a different framework. I was not here during the lecture on Bayesian 
#frameworks and did not have time to catch up while at home, so I employed a generalised additive model 
#instead (which is more receptive to the type of data I am working with - a seemingly non-linear
#relationship between the predictor and response variables):
#GAM 1: 
orca$Location.of.population <- as.factor(orca$Location.of.population)
orca$Country.list <- as.factor(orca$Country.list)
gam1 <- gam(Abundance ~ s(Year) + s(Country.list, bs = "fs") + #bs = "fs" because the country is a categorical variable
              s(Location.of.population, bs = "re"), #bs = "re' specifies the random effect
            data = orca, family = poisson)

null_model_gam <- gam(Abundance ~ 1, data = orca, family = poisson)

summary(gam1) #non-linear relationship between abundance and year; significant variability in orca abundance between sites
#adjusted r^2 is 0.949 (very high); deviance explained = 93%
#UBRE = 1.6485 (benchmark = 1); reasonable model fit
#the GAM model indicates a strong non-linear relationship between abundance and the year, as well as significant variability in abundance among different locations
plot(gam1)
plot(residuals(gam1))

png("Plots/gam_check.png", width = 800, height = 600)
gam_check <- gam.check(gam1) #this suggests the k might be slightly too low; I will increase it in the following model
dev.off()

#GAM 2: 
gam2 <- gam(Abundance ~ s(Year, k = 40) + s(Country.list, bs = "fs") + #bs = "fs" because the country is a categorical variable
              s(Location.of.population, bs = "re"), #bs = "re' specifies the random effect
            data = orca, family = poisson)

summary(gam2) #non-linear relationship between abundance and year; significant variability in orca abundance between sites
#adjusted r^2 is 0.951 (very high); deviance explained = 93.3%
#UBRE = 1.618 (benchmark = 1); reasonable model fit
#the GAM model indicates a strong non-linear relationship between abundance and the year, as well as significant variability in abundance among different locations
plot(gam2)
plot(residuals(gam2))

png("Plots/gam_check_2.png", width = 800, height = 600)
gam_check_2 <- gam.check(gam2) #this suggests the k might be slightly too low; I will increase it in the following model
dev.off()

#Finally, I want to include year as a random effect and test those relationships as well:
#GAM 3:
gam3 <- gam(Abundance ~ s(Year) + s(Country.list, bs = "fs") + #bs = "fs" because the country is a categorical variable
              s(Location.of.population, bs = "re") + #bs = "re' specifies the random effect
            s(Year, bs = "re"), data = orca, family = poisson)
summary(gam3) #non-linear relationship between abundance and year; significant variability in orca abundance between sites
#adjusted r^2 is 0.949 (very high); deviance explained = 93%
#UBRE = 1.6485 (benchmark = 1); reasonable model fit
#the GAM model indicates a strong non-linear relationship between abundance and the year, as well as significant variability in abundance among different locations
#no effect of inter-year or inter-country variation on orca abundance
plot(gam3)
plot(residuals(gam3))

png("Plots/gam_check_3.png", width = 800, height = 600)
gam_check_3 <- gam.check(gam3) #this suggests the k might be slightly too low; I will increase it in the following model
dev.off()

#GAM 4: 
gam4 <- gam(Abundance ~ s(Year, k = 40) + s(Country.list, bs = "fs") + #bs = "fs" because the country is a categorical variable
              s(Location.of.population, bs = "re") + #bs = "re' specifies the random effect
              s(Year, bs = "re", k = 40), data = orca, family = poisson)
summary(gam4) #non-linear relationship between abundance and year; significant variability in orca abundance between sites
#adjusted r^2 is 0.951 (very high); deviance explained = 93.3%
#UBRE = 1.618 (benchmark = 1); reasonable model fit
#the GAM model indicates a strong non-linear relationship between abundance and the year, as well as significant variability in abundance among different locations
#no effect of inter-year or inter-country variation on orca abundance
plot(gam4)
png("Plots/resid_gam4.png", width = 800, height = 600)
plot(residuals(gam4))
dev.off()
shapiro.test(resid(gam4)) #normal if p > 0.01

png("Plots/gam_check_4.png", width = 800, height = 600)
gam_check_4 <- gam.check(gam4) #this suggests the k might be slightly too low; I will increase it in the following model
abline(a = 0, b = 1, col = "red")
dev.off()

#to determine which GAM is best, I will use the AIC and the LRT:
(aic_gam <- AIC(gam1, gam2, gam3, gam4, null_model_gam)) #gams 2 and 4 appear to be identical in their fit to the data
(lr_test_gams <- lrtest(gam2, gam4)) #the more complex model (gam4) is better fit to the data

gam4_summary <- capture.output(summary(gam4))
writeLines(gam4_summary, "Tables/gam4_summary.txt")



#Bayesian models ----
orca1_mbrms <- brms::brm(Abundance ~ I(Year - 1970),
                           data = orca, family = poisson(), chains = 3,
                           iter = 3000, warmup = 1000)
summary(orca1_mbrms)
plot(orca1_mbrms) #looks good
pp_check(orca1_mbrms) #these look very very different

orca2_mbrms <- brms::brm(Abundance ~ I(Year - 1970) + (1|Year) +
                           (1|Location.of.population),
                         data = orca, family = poisson(), chains = 3,
                         iter = 3000, warmup = 1000)
summary(orca2_mbrms)
png("Plots/orca_bayes.png", width = 800, height = 600)
plot(orca2_mbrms) #looks good
pp_check(orca2_mbrms) #looks much better

(bayes_plot<- plot(orca2_mbrms))
png("Plots/bayes_plot.png", width = 800, height = 600)
print(bayes_plot)
dev.off()


(pp_plot <- pp_check(orca2_mbrms)) #looks much better than the previous model
png("Plots/pp_plot.png", width = 800, height = 600)
print(pp_plot)
dev.off()

loo(orca1_mbrms, orca2_mbrms, compare = TRUE) #the second model is better

bayes_orca_summary <- capture.output(summary(orca2_mbrms))
writeLines(bayes_orca_summary, "Tables/bayes_orca_summary.txt")


(model_fit <- orca %>%
    add_predicted_draws(orca2_mbrms) %>%  # adding the posterior distribution
    ggplot(aes(x = Year, y = Abundance)) +  
    stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50),  # regression line and CI
                    alpha = 0.5, colour = "black") +
    geom_point(data = orca, colour = "navyblue", size = 3) +   # raw data
    scale_fill_brewer(palette = "Greys") +
    ylab("Orca abundance\n") +  # latin name for red knot
    xlab("\nYear") +
    theme_classic() +
    theme(legend.title = element_blank(),
          legend.position = c(0.15, 0.85)))

(location_fit <- orca %>%
    group_by(Location.of.population) %>%
    add_predicted_draws(orca2_mbrms) %>%
    ggplot(aes(x = Year, y = Abundance, color = ordered(Location.of.population), 
               fill = ordered(Location.of.population))) +
    stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50), alpha = 1/4) +
    geom_point(data = orca) +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Dark2") +
    theme_bw() +
    ylab("Orca abundance\n") +
    xlab("\nYear") +
    theme_classic() +
    theme(legend.title = element_blank()))

(bayes_grid <- grid.arrange(model_fit, location_fit, ncol = 2))
ggsave("bayes_grid.jpg", bayes_grid, path = "Plots", units = "cm", 
       width = 50, height = 20)
