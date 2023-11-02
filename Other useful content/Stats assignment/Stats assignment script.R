##%#########################################################################%##
#                                                                             #
#                             Data science Tutorial?                          #
#                               Zoja Manček Páli                              #
#                                                                             #
##%#########################################################################%##

#Libraries
library(car)
library(dplyr)
library(e1071)
library(ggfortify)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(lmtest)
library(MASS)
library(yarrr) 

#WD
setwd("~/") #erases previously set WDs
setwd("Personal repo - zmancekpali/Other useful content/Stats assignment") #sets a new one
getwd() #check that it's worked


#data
inga <- read.csv('Inga_traits.csv')


#QUESTION 1 ----
#1a ----
x3 <- seq(min(inga$Leaf_Area, na.rm = TRUE), max(inga$Leaf_Area, na.rm = TRUE), length = 40)
fun1 <- dnorm(x3, mean = mean(inga$Leaf_Area, na.rm = TRUE), sd = sd(inga$Leaf_Area, na.rm = TRUE))
hist_data <- hist(inga$Leaf_Area, breaks = 25, plot = FALSE)
scaled_density_1 <- fun1 * sum(hist_data$counts) * diff(hist_data$breaks[1:2])
hist(inga$Leaf_Area, breaks = 25, col = yarrr::transparent("chartreuse3", trans.val = .6),
     xlab = expression(Leaf ~ area ~(cm^2)),
     main = "Leaf area distribution in inga species")
lines(x3, scaled_density_1, col = "forestgreen", lwd = 4)
skewness(inga$Leaf_Area, na.rm = TRUE) #1.230511; positively skewed
shapiro.test(inga$Leaf_Area) 
kurtosis(inga$Leaf_Area)

#finding the species with the max and min leaf areas
max <- inga %>% filter(Leaf_Area == max(Leaf_Area))
min <-  inga %>%  filter(Leaf_Area == min(Leaf_Area))
cat("Maximum Leaf Area:", max$Leaf_Area, "for species:", max$Species, "\n")
cat("Minimum Leaf Area:", min$Leaf_Area, "for species:", min$Species, "\n")


#1b ----
x4 <- seq(min(log(inga$Leaf_Area), na.rm = TRUE), max(log(inga$Leaf_Area), na.rm = TRUE), length = 40)
fun2 <- dnorm(x4, mean = mean(log(inga$Leaf_Area), na.rm = TRUE), sd = sd(log(inga$Leaf_Area), na.rm = TRUE))
hist_data_2 <- hist(log(inga$Leaf_Area), breaks = 25, plot = FALSE)
scaled_density_2 <- fun2 * sum(hist_data_2$counts) * diff(hist_data_2$breaks[1:2])
hist(log(inga$Leaf_Area), breaks = 25, col = yarrr::transparent("orchid", trans.val = .6),
     xlab = expression(Leaf ~ area ~(cm^2)),
     main = "Log-transformed leaf area distribution in inga species")
lines(x4, scaled_density_2, col = "maroon2", lwd = 4)
skewness(log(inga$Leaf_Area), na.rm = TRUE) #-0.259, slightly negatively skewed
shapiro.test(log(inga$Leaf_Area))
kurtosis(log(inga$Leaf_Area))


#QUESTION 2 ----
#2a ----
boxplot(P_Leaf ~ Habitat, data = inga)
inga <- inga %>% mutate(Habitat = dplyr::recode(Habitat, "floodplain" = "Floodplain",
                                                "generalist" = "Generalist",
                                                "upland" = "Upland"))

ggboxplot(inga, x = "Habitat", y = "P_Leaf",
          fill = "Habitat",
          ylab = "P concentration (mg/g)",
          legend = "none",
          bxp.errorbar = TRUE, bxp.errorbar.width = 0.15,
          add = "jitter") + #adds the raw data dots
  scale_fill_manual(values = c("dodgerblue3", "plum3", "chartreuse4")) +
  theme(text = element_text(size=9)) +
  theme(axis.title.x=element_blank())


#2b ----
gen <- subset(inga, Habitat == "generalist")
flood <- subset(inga, Habitat == "floodplain")
up <- subset(inga, Habitat == "upland")

x5 <- seq(min(gen$P_Leaf, na.rm = TRUE), max(gen$P_Leaf, na.rm = TRUE), length = 40)
fun3 <- dnorm(x5, mean = mean(gen$P_Leaf, na.rm = TRUE),
              sd = sd(gen$P_Leaf, na.rm = TRUE)) #this gives the normal curve
hist_data_3 <- hist(gen$P_Leaf, breaks = 25, plot = FALSE)
scaled_density_3 <- fun3 * sum(hist_data_3$counts) * diff(hist_data_3$breaks[1:2])
hist(gen$P_Leaf, breaks = 25, col = yarrr::transparent("orange", trans.val = .6),
     xlab = "Leaf P concentration (mg/g)",
     main = "Leaf P concentration distribution in generalist habitats")
lines(x5, scaled_density_3, col = "orange", lwd = 4)
skewness(gen$P_Leaf, na.rm = TRUE) #0.2754614

x6 <- seq(min(flood$P_Leaf, na.rm = TRUE), max(flood$P_Leaf, na.rm = TRUE), length = 40)
fun4 <- dnorm(x6, mean = mean(flood$P_Leaf, na.rm = TRUE),
              sd = sd(flood$P_Leaf, na.rm = TRUE)) #this gives the normal curve
hist_data_4 <- hist(flood$P_Leaf, breaks = 25, plot = FALSE)
scaled_density_4 <- fun4 * sum(hist_data_4$counts) * diff(hist_data_4$breaks[1:2])
hist(flood$P_Leaf, breaks = 25, col = yarrr::transparent("lightblue", trans.val = .6),
     xlab = "Leaf P concentration (mg/g)",
     main = "Leaf P concentration distribution in floodplain habitats")
lines(x6, scaled_density_4, col = "blue", lwd = 4)
skewness(flood$P_Leaf, na.rm = TRUE) #0.6370701

x7 <- seq(min(up$P_Leaf, na.rm = TRUE), max(up$P_Leaf, na.rm = TRUE), length = 40)
fun5 <- dnorm(x7, mean = mean(up$P_Leaf, na.rm = TRUE),
              sd = sd(up$P_Leaf, na.rm = TRUE)) #this gives the normal curve
hist_data_5 <- hist(up$P_Leaf, breaks = 25, plot = FALSE)
scaled_density_5 <- fun5 * sum(hist_data_5$counts) * diff(hist_data_5$breaks[1:2])
hist(up$P_Leaf, breaks = 25, col = yarrr::transparent("lightgreen", trans.val = .6),
     xlab = "Leaf P concentration (mg/g)",
     main = "Leaf P concentration distribution in upland habitats")
lines(x7, scaled_density_5, col = "darkgreen", lwd = 4)
skewness(up$P_Leaf, na.rm = TRUE) #0.1737009


#first lm
mod1 <- lm(P_Leaf ~ Habitat, data = inga, na.action = na.omit)
plot(mod1)
autoplot(mod1) #this does the same as plot(mod1)

#checking assumptions:
durbinWatsonTest(mod1) #data are independent
shapiro.test(resid(mod1)) #p = 0.1193; residuals are normal
ggqqplot(inga$P_Leaf, ylab = "Leaf P Concentration")
bartlett.test(P_Leaf ~ Habitat, data = inga) #0.006; heteroscedascity 

##log-transforming the data for the model
mod2 <- lm(log(P_Leaf) ~ Habitat, data = inga)
autoplot(mod2)
shapiro.test(resid(mod2)) #0.60 = normal
durbinWatsonTest(mod2) #data are independent
bartlett.test(log(P_Leaf) ~ Habitat, data = inga) #0.12; homoscedascity
anova(mod2)
summary(mod2)$r.squared


#2c and 2d ----
mod3 <- lm(log(P_Leaf) ~ Habitat + Clade, data = inga)
autoplot(mod3) 
shapiro.test(resid(mod3)) #0.8101 = normal
durbinWatsonTest(mod3) #data are independent
bptest(log(P_Leaf) ~ Habitat + Clade, data = inga) #homoscedascity
anova(mod3)
summary(mod3)$r.squared

#2e ----
group_by(inga, Habitat) %>%  summarise(      
  count = n(),  
  mean = mean(P_Leaf, na.rm = TRUE),  
  sd = sd(P_Leaf, na.rm = TRUE)) #summarise one group from inga

#QUESTION 3 ----
#3a ----
mod5 <- lm(P_Leaf ~ C_Leaf, data = inga)
autoplot(mod5) 
shapiro.test(resid(mod5)) #p = 0.0096; residuals are non-normal
bartlett.test(P_Leaf ~ Habitat, data = inga) #homoscedascity
cor(inga$C_Leaf, inga$P_Leaf, use = "pairwise.complete.obs") #weak negative correlation; -0.23
kruskal.test(P ~ C, data = my_data_1) #bc i was unable to transform the data, I would normally use a non-parametric test


###drawing a linear reg line onto a scatterplot with ggplot only
up <- subset(inga, Habitat == "Upland")
flood <- subset(inga, Habitat == "Floodplain")
gen <- subset(inga, Habitat == "Generalist")

mod6 <- lm(P_Leaf ~ C_Leaf, data = flood)
summary(mod6) #p = 0.2366; r^2 = 0.1145; F = 1.552
coef(mod6) #slope = -0.01327998

mod7 <- lm(P_Leaf ~ C_Leaf, data = gen)
summary(mod7) #p = 0.3646; r^2 = 0.2747; F = 1.136
coef(mod7) #slope = 0.008191453

mod8 <- lm(P_Leaf ~ C_Leaf, data = up)
summary(mod8) #p = 0.0172; r^2 = 0.4853; F = 8.486 
coef(mod8) #slope = 0.01332

ggplot(inga, aes(x = C_Leaf, y = P_Leaf, color = Habitat)) + 
  geom_point(aes(color = Habitat, shape = Habitat), size = 2) +
  scale_color_manual(values = c("darkorange", "violetred", "skyblue")) +
  scale_shape_manual(values = c(15, 16, 17)) +
  stat_smooth(method = "lm", se = FALSE, aes(fill=Habitat), fullrange = TRUE) + #THE LINES FOR EACH group are not extended
  theme_classic() + #also the fill = group represents the colour of the se shaded areas, but I didnt want any so se = FALSE
  labs(x=expression(Leaf ~ C ~ concentration~(mg ~ g^-1)), y = expression(Leaf ~ P ~ concentration~(mg ~ g^-1))) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  theme(legend.background = element_rect(color="black")) +
  annotate("text", x = 49.8, y = 0.2, parse = TRUE,
           label = "R^2==0.1145", color = "darkorange") +
  annotate("text", x = 49.75, y = 0.19, parse = TRUE,
           label = "p-value == 0.2366", color = "darkorange") +
  annotate("text", x = 49.8, y = 0.17, parse = TRUE,
           label = "R^2==0.2747", color = "violetred") +
  annotate("text", x = 49.75, y = 0.16, parse = TRUE,
           label = "p-value == 0.3646", color = "violetred") +
  annotate("text", x = 49.8, y = 0.1, parse = TRUE,
           label = "R^2==0.4853", color = "skyblue") +
  annotate("text", x = 49.75, y = 0.09, parse = TRUE,
           label = "p-value == 0.0172", color = "skyblue")

###drawing a linear reg line onto scatterplot with a function
#here the R2, p, and slope values are for the overall reg line, I can't figure out how to do it for each individual one
ggplotReg <- function(data, fit) {
  ggplot(data, aes(x = P, y = C, color = group, shape = group)) +
    geom_point(size = 2) +
    theme_classic()+
    scale_color_manual(values = c("darkorange", "violetred", "skyblue")) +
    scale_shape_manual(values = c(15, 16, 17)) +
    geom_smooth(data = subset(data, !is.na(P) & !is.na(C)), method = "lm", se = FALSE) +
    labs(title = paste("Adj R2 =", signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =", signif(fit$coef[[1]], 5),
                       "Slope =", signif(fit$coef[[2]], 5),
                       "P =", signif(summary(fit)$coef[2, 4], 5)))
}

fit1 <- lm(C ~ P, data = my_data_1)
ggplotReg(my_data_1, fit1)

#3b ----
#grouping into a new category
inga$Category <- ifelse(inga$Habitat == 'Floodplain', 'Floodplain', 'Non-floodplain')

mod9 <- lm(P_Leaf ~ Category * C_Leaf, data = inga, na.action = na.omit)
residuals9 <- resid(mod9) 
shapiro.test(residuals9) #p = 0.000135; residuals are non-normal
summary(mod9)$r.squared #46.769
durbinWatsonTest(mod9)
bptest(mod9) #test for heteroscedascity with interaction term models
anova(mod9)

#3c ---- 
#trying log first
mod_log <- lm(log(P_Leaf) ~ Category * C_Leaf, data = inga, na.action = na.omit)
residuals_log <- resid(mod_log) 
shapiro.test(residuals_log) #p = 0.0003528; residuals are non-normal
summary(mod_log)$r.squared #53.393755
durbinWatsonTest(mod_log) #p = 0.692
bptest(mod_log) #test for heteroscedascity with interaction term models; equal variances
anova(mod_log)


#so will transform P
#do Box-Cox to determine which transformation
#Look for the highest point or peak on the curve and note the corresponding lambda value.
boxcox_P <- boxcox(inga$P_Leaf ~ 1) #Find the lambda value for the highest peak on the curve for P
P <- inga$P_Leaf
lambda <- boxcox_P$x[which.max(boxcox_P$y)]
inga$trans_P <- (P ^ lambda - 1) / lambda

model_trans <- lm(trans_P ~ Category * C_Leaf, data = inga)
plot(model_trans)
shapiro.test(resid(model_trans)) #p = 0.05074; normal
bptest(model_trans) #variances are equal
summary(model_trans)$r.squared #60.08178
anova(model_trans)

#this below was a mistake but could still be useful (if no transformations work) ----
#still don't meet the ANOVA assumtions so non-parametric test
kruskal.test(P ~ group * C, data = inga) #doesnt work bc of the interaction term

#non-parametric tests cannot account for interactions, so GLM
#can still use the gamma family bc GLMs account for non-normality
mod_glm_log <- glm(P_Leaf ~ Category * C_Leaf, data = inga, family = Gamma(link = "log"))
summary(mod_glm_log) #AIC = -130.67

mod_glm_identity <- glm(P_Leaf ~  Category * C_Leaf, data = inga, family = Gamma)
summary(mod_glm_identity) #AIC = -130.56

mod_glm_inverse <- glm(P_Leaf ~  Category * C_Leaf, family = Gamma(link = "inverse"), data = inga)
summary(mod_glm_inverse) #AIC = -130.75

mod_glm_logit <- glm(P_Leaf ~  Category * C_Leaf, family = binomial(link = "logit"), data = inga)
summary(mod_glm_logit) #AIC = 16.823

mod_glm_probit <- glm(P_Leaf ~  Category * C_Leaf, family = binomial(link = "probit"), data = inga)
summary(mod_glm_probit) #AIC = 16.823

#Comparing the summaries of the log GLM and identity GLM models, we can evaluate their goodness of fit and assess which one fits the data better. Here are some key observations:
#Deviance Residuals: The deviance residuals provide a measure of the model's fit to the data. Both models have similar patterns in the distribution of the residuals, with values ranging from -0.47 to 0.61. There is no substantial difference in the overall fit based on the deviance residuals.
#Coefficients: The coefficient estimates for the predictors (group and C) and their interactions are different between the log GLM and identity GLM models. However, the significance levels (p-values) for these coefficients are similar in both models. This suggests that the impact of the predictors on the response variable is not significantly different between the two models.
#AIC: The Akaike Information Criterion (AIC) is a measure of the model's goodness of fit, adjusted for the number of parameters. In this case, the AIC values for the log GLM and identity GLM are very close, with the log GLM having an AIC of -109.07 and the identity GLM having an AIC of -109.06. The AIC values indicate that both models provide a similar level of fit to the data.
#Based on these observations, it seems that both the log GLM and identity GLM models have similar goodness-of-fit and provide comparable results. Therefore, you can choose the identity GLM for simplicity, as it does not require a transformation of the results.
#I will use the identity link so no need for transformation back from log







#QUESTION 4 ----
#first make sure that all data have the same length (equal NAs - to allow for AIC comparison)
subset_fr <- inga[complete.cases(inga$Expansion, inga$Trichome_Density), ] #both do the same here

#4a ----
mod_glm_trich <- glm(Mevalonic_Acid ~ Trichome_Density, data = subset_fr, family = binomial)
summary(mod_glm_trich) #AIC = 33.844; p = 0.197

mod_glm_exp <- glm(Mevalonic_Acid ~ Expansion, data = subset_fr, family = binomial)
summary(mod_glm_exp) #AIC = 34.528; p = 0.0450*

#4b ----
mod_glm_multiple <- glm(Mevalonic_Acid ~ Expansion + Trichome_Density, 
                        family = binomial, data = subset_fr)
summary(mod_glm_multiple) #AIC = 28.865

mod_glm_int <- glm(Mevalonic_Acid ~ Expansion * Trichome_Density, 
                   family = binomial, data = subset_fr)
summary(mod_glm_int) #AIC = 30.717

AIC(mod_glm_trich, mod_glm_exp, mod_glm_multiple, mod_glm_int)
#multiple is best

#4c ----
newdata = data.frame(Expansion = 30, Trichome_Density = 43)
predict(mod_glm_multiple, newdata, type = "response") #0.0006448827

-3.9669 + 0.1064*30 - 0.1528*43
2.718281828459045^(-7.3453)/(1 + 2.718281828459045^(-7.3453))

#4e ----
subset_fr$Mevalonic_Acid <- as.factor(subset_fr$Mevalonic_Acid)
mev_acid_levels <- levels(subset_fr$Mevalonic_Acid <- c("Absent", "Present"))

#cd plot
par(mar=c(4, 4, 1, 4))
cdplot(as.factor(Mevalonic_Acid) ~ Expansion, data = subset_fr,
       ylab = "Mevalonic acid presence",
       xlab = "Expansion rate (% / day)",
       col = c("lightblue", adjustcolor("mediumvioletred", alpha.f = 0.5)))
mtext("Probability", side = 4, line = 2.5)
mtext("Awesome Y variable", side=2, line=2.2, cex=2)

#when expansion rate reaches 70 and above, you are most likely to find mevalonic acid in the leaf

#line plot
ggplot(subset_fr, aes(x = Expansion, y = as.numeric(Mevalonic_Acid))) +
  geom_point(size = 2) +
  theme_classic() +
  xlab("Expansion rate (% / day)") +
  ylab("Mevalonic acid presence") + 
  stat_smooth(method = 'glm',
              method.args = list(family = 'binomial'),
              formula = y~x,
              alpha = 1, size = 3, se = F, colour = "mediumvioletred")