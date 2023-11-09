##%#########################################################################%##
#                                                                             #
#                    Dissertation script - Zoja Manček Páli                   #
#                              Started: 30.9.2023                             #
#                                                                             #
##%#########################################################################%##

#WD
setwd("~/") #erases previously set WDs
setwd("Personal repo - zmancekpali/Dissertation") #sets a new one
getwd() #check that it's worked


#Libraries
library(ape)
library(cowplot)
library(dunn.test)
library(e1071)
library(ggfortify)
library(ggpubr)
library(gridExtra)
library(lme4)
library(MASS)
library(multcomp)
library(stats)
library(tidyverse)
library(vegan)

#Data
trees <- read.csv("traits_analysis.csv")
trees <- trees %>% 
  mutate(canopy_pos = recode(canopy_pos, 
                             "L" = "Lower",
                             "U" = "Upper")) %>%  #recode canopy positions from abbreviations
  mutate(code_two = recode(code_two,
                           "CB" = "C. bullatus",
                           "QC" = "Q. cerris",
                           "QI" = "Q. ilex",
                           "RPS" = "R. pseudoacacia semperflorens")) %>% #recode alien species names
  filter(A >= 0) %>% 
  arrange(code_two = factor(type, levels = c('Native', 'Naturalised', 'Invasive', 
                                             'C. bullatus', 'Q. cerris', 'Q. ilex', 
                                             'R. pseudoacacia semperflorens'))) #rearranges the categories in this order

trees$age <- as.numeric(trees$age)
  
nns <- trees %>% 
  filter(type %in% c('Native', 'Naturalised', "Invasive")) %>% #excluding the alien group for initial analysis
  mutate(canopy_pos = recode(canopy_pos, 
                             "L" = "Lower",
                             "U" = "Upper")) %>%  #recode canopy positions from abbreviations
  arrange(type = factor(type, levels = c('Native', 'Naturalised', 'Invasive'))) %>%  #rearranges the categories in this order
  filter(A >= 0) #removed negative A values (they were dead leaves)

traits.palette <- c("#CD6090", "#698B69", "#EEC900")    #defining 3 colours
traits.palette2 <- c("#CD6090", "#698B69", "#EEC900", "#5EA8D9", "#245C82", "#4A3E87", "#5A5DC7")

#Exploration
head(nns)
str(nns)

#Step 1: see whether NN and I species differ in their traits + respective box plots + post-hoc tests if significant
#LMA ----
lma_mod <- lm(lma ~ type, data = nns)
autoplot(lma_mod)
shapiro.test(resid(lma_mod)) #residuals not distributed normally
bartlett.test(lma ~ type, data = nns) #heteroscedascity

#Attempt mathematical transformation first to meet ANOVA assumptions:
lma_boxcox <- boxcox(lma ~ 1, data = nns) #the λ is the highest point on the curve
(lma_lambda <- lma_boxcox$x[which.max(lma_boxcox$y)]) #λ = -0.3838384
nns <- nns %>% mutate(transformed_lma = (lma ^ (lma_lambda - 1)) / lma_lambda) #Box-Cox transformation applied in a new column

lma_mod_trans <- lm(transformed_lma ~ type, data = nns)
autoplot(lma_mod_trans)
shapiro.test(resid(lma_mod_trans)) #residuals not distributed normally
bartlett.test(transformed_lma ~ type, data = nns) #heteroscedascity

#Transformation did not work, moving on to non-parametric alternative:
(lma_kw <- kruskal.test(lma ~ type, data = nns)) #p-value = 0.000254; significant

(lma_boxplot <- ggplot(nns, 
                       aes(x = factor(type, levels = c('Native', 'Naturalised', 'Invasive')), #reorders the types 
                           y = lma, fill = type)) + 
    geom_boxplot() + #creates the boxplot
    stat_boxplot(geom ='errorbar', width = 0.3) + #adds the whisker ends
    geom_jitter() + #adds the jitter
    scale_fill_manual(values = traits.palette) + 
    labs(x = "\n Invasion status", 
         y = expression(atop("LMA (g/cm"^2*")"))) + 
    theme_classic() + 
    theme(axis.text = element_text(size = 10), 
          axis.title = element_text(size = 11), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

ggsave("lma_boxplot.jpg", lma_boxplot, path = "Plots", units = "cm", width = 20, height = 15) 

#Dunn post-hoc test
dunn_lma <- dunn.test(nns$lma, nns$type, method = "bonferroni") #invasives differ significantly from natives yay
#naturalised also differ significantly from natives


#Average chlorophyll ----
chl_mod <- lm(chl ~ type, data = nns)
autoplot(chl_mod)
shapiro.test(resid(chl_mod)) #residuals not distributed normally
bartlett.test(chl ~ type, data = nns) #heteroscedascity

#Attempt mathematical transformation first to meet ANOVA assumptions:
chl_boxcox <- boxcox(nns$chl ~ 1)
(chl_lambda <- chl_boxcox$x[which.max(chl_boxcox$y)]) #λ = -0.2626263
nns <- nns %>% mutate(transformed_chl = (chl ^ (chl_lambda - 1)) / chl_lambda) #Box-Cox transformation applied in a new column

chl_mod_trans <- lm(transformed_chl ~ type, data = nns)
autoplot(chl_mod_trans)
shapiro.test(resid(chl_mod_trans)) #residuals not distributed normally
bartlett.test(transformed_chl ~ type, data = nns) #heteroscedascity           

#Transformation did not work, moving on to non-parametric alternative:
kruskal.test(chl ~ type, data = nns) #0.0004246; significant

(chl_boxplot <- ggplot(nns, 
                       aes(x = factor(type, levels = c('Native', 'Naturalised', 'Invasive')), #reorders the types 
                           y = chl, fill = type)) + 
    geom_boxplot() + #creates the boxplot
    stat_boxplot(geom ='errorbar', width = 0.3) + #adds the whisker ends
    geom_jitter() + #adds the jitter
    scale_fill_manual(values = traits.palette) + 
    labs(x = "\n Invasion status", 
         y = expression(atop("Average chlorophyll (SPAD)"))) + 
    theme_classic() + 
    theme(axis.text = element_text(size = 10), 
          axis.title = element_text(size = 11), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

ggsave("chl_boxplot.jpg", chl_boxplot, path = "Plots", units = "cm", width = 20, height = 15) 

#Dunn post-hoc test
dunn_chl <- dunn.test(nns$chl, nns$type, method = "bonferroni") #invasive differ significantly from natives and naturalised yay
#naturalised and natives show no significant difference

#Assimilation rate ----
a_mod <- lm(A ~ type, data = nns)
autoplot(a_mod)
shapiro.test(resid(a_mod)) #residuals not distributed normally
bartlett.test(A ~ type, data = nns) #homoscedascity

#Attempt mathematical transformation first to meet ANOVA assumptions:
a_boxcox <- boxcox(nns$A ~ 1)
(a_lambda <- a_boxcox$x[which.max(a_boxcox$y)]) #λ = 0.9090909
nns <- nns %>% mutate(transformed_A = (A ^ (a_lambda - 1)) / a_lambda) #Box-Cox transformation applied in a new column

a_mod_trans <- lm(transformed_A ~ type, data = nns)
autoplot(a_mod_trans)
shapiro.test(resid(a_mod_trans)) #residuals not distributed normally
bartlett.test(transformed_A ~ type, data = nns) #heteroscedascity           

#Transformation did not work, moving on to non-parametric alternative:
kruskal.test(A ~ type, data = nns) #0.002847; significant

(a_boxplot <- ggplot(nns, 
                     aes(x = factor(type, levels = c('Native', 'Naturalised', 'Invasive')), #reorders the types 
                         y = A, fill = type)) + 
    geom_boxplot() + #creates the boxplot
    stat_boxplot(geom ='errorbar', width = 0.3) + #adds the whisker ends
    geom_jitter() + #adds the jitter
    scale_fill_manual(values = traits.palette) + 
    labs(x = "\n Invasion status", 
         y = expression(atop(paste("Assimilation rate (", mu, "mol CO"[2]~"m"^-2*~"s"^-1, ")")))) +
    theme_classic() + 
    theme(axis.text = element_text(size = 10), 
          axis.title = element_text(size = 11), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none")) 

ggsave("a_boxplot.jpg", a_boxplot, path = "Plots", units = "cm", width = 20, height = 15) 

#Dunn post-hoc test
dunn_a <- dunn.test(nns$A, nns$type, method = "bonferroni") #invasives differ significantly from natives and naturalised species
#naturalised and natives show no significant difference



#LDCM ----
ldcm_mod <- lm(ldcm ~ type, data = nns)
autoplot(ldcm_mod)
shapiro.test(resid(ldcm_mod)) #residuals distributed normally
bartlett.test(ldcm ~ type, data = nns) #heteroscedascity

#Attempt mathematical transformation first to meet ANOVA assumptions:
ldcm_boxcox <- boxcox(nns$ldcm ~ 1)
(ldcm_lambda <- ldcm_boxcox$x[which.max(ldcm_boxcox$y)]) #λ = 1.474747
nns <- nns %>% mutate(transformed_ldcm = (ldcm ^ (ldcm_lambda - 1)) / ldcm_lambda) #Box-Cox transformation applied in a new column

ldcm_mod_trans <- lm(transformed_ldcm ~ type, data = nns)
autoplot(ldcm_mod_trans)
shapiro.test(resid(ldcm_mod_trans)) #residuals not distributed normally
bartlett.test(transformed_ldcm ~ type, data = nns) #heteroscedascity           

#Transformation did not work, moving on to non-parametric alternative:
kruskal.test(ldcm ~ type, data = nns) #0.0008243; significant


(ldmc_boxplot <- ggplot(nns, 
                        aes(x = factor(type, levels = c('Native', 'Naturalised', 'Invasive')), #reorders the types 
                            y = ldcm, fill = type)) + 
    geom_boxplot() + #creates the boxplot
    stat_boxplot(geom ='errorbar', width = 0.3) + #adds the whisker ends
    geom_jitter() + #adds the jitter
    scale_fill_manual(values = traits.palette) + 
    labs(x = "\n Invasion status", 
         y = expression(atop(paste("Leaf dry matter concentration (g" ~ "g"^-1~")")))) +
    theme_classic() + 
    theme(axis.text = element_text(size = 10), 
          axis.title = element_text(size = 11), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

ggsave("ldmc_boxplot.jpg", ldmc_boxplot, path = "Plots", units = "cm", width = 20, height = 15) 

#Dunn post-hoc test
dunn_ldmc <- dunn.test(nns$ldcm, nns$type, method = "bonferroni") #invasive differ significantly from natives and naturalised species
#naturalised and natives show no significant difference


#Evapotransiration (??) ----
e_mod <- lm(E ~ type, data = nns)
autoplot(e_mod)
shapiro.test(resid(e_mod)) #residuals distributed normally
bartlett.test(E ~ type, data = nns) #homoscedascity
anova(e_mod) #NS; p-value = 0.3313

(e_boxplot <- ggplot(nns, 
                     aes(x = factor(type, levels = c('Native', 'Naturalised', 'Invasive')), #reorders the types 
                         y = E, fill = type)) + 
    geom_boxplot() + #creates the boxplot
    stat_boxplot(geom ='errorbar', width = 0.3) + #adds the whisker ends
    geom_jitter() + #adds the jitter
    scale_fill_manual(values = traits.palette) + 
    labs(x = "\n Invasion status", 
         y = expression(atop(paste("Evapotranspiration rate (", mu, "mol CO"[2] ~ "m"^-2*~"s"^-1, ")")))) +
    theme_classic() + 
    theme(axis.text = element_text(size = 10), 
          axis.title = element_text(size = 11), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

ggsave("e_boxplot.jpg", e_boxplot, path = "Plots", units = "cm", width = 20, height = 15) 

#Tukey's Honestly Significant Difference post-hoc test
aov_e <- aov(E ~ type, data = nns)
tukey_e <- TukeyHSD(aov_e)
#no significant differences


#GH20----
g_mod <- lm(g ~ type, data = nns)
autoplot(g_mod)
shapiro.test(resid(g_mod)) #residuals not distributed normally
bartlett.test(g ~ type, data = nns) #homoscedascity

#Attempt mathematical transformation first to meet ANOVA assumptions:
g_boxcox <- boxcox(nns$g ~ 1)
(g_lambda <- g_boxcox$x[which.max(g_boxcox$y)]) #λ = -0.6666667
nns <- nns %>% mutate(transformed_g = (g ^ (g_lambda - 1)) / g_lambda) #Box-Cox transformation applied in a new column

g_mod_trans <- lm(transformed_g ~ type, data = nns)
autoplot(g_mod_trans)
shapiro.test(resid(g_mod_trans)) #residuals distributed normally
bartlett.test(transformed_g ~ type, data = nns) #heteroscedascity           

#Transformation did not work, moving on to non-parametric alternative:
kruskal.test(g ~ type, data = nns) #0.03313; significant

(g_boxplot <- ggplot(nns, 
                     aes(x = factor(type, levels = c('Native', 'Naturalised', 'Invasive')), #reorders the types 
                         y = g, fill = type)) + 
    geom_boxplot() + #creates the boxplot
    stat_boxplot(geom ='errorbar', width = 0.3) + #adds the whisker ends
    geom_jitter() + #adds the jitter
    scale_fill_manual(values = traits.palette) + 
    labs(x = "\n Invasion status", 
         y = expression(atop("Average chlorophyll (SPAD)"))) + 
    theme_classic() + 
    theme(axis.text = element_text(size = 10), 
          axis.title = element_text(size = 11), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

ggsave("g_boxplot.jpg", g_boxplot, path = "Plots", units = "cm", width = 20, height = 15) 

#Dunn post-hoc test
dunn_g <- dunn.test(nns$g, nns$type, method = "bonferroni") #invasive differ significantly from natives and naturalised yay
#naturalised and natives show no significant difference


#dark respiration models and boxplot ----
dr_mod1 <- lm(Dark_resp ~ type, data = nns)
autoplot(dr_mod1)
shapiro.test(resid(dr_mod1)) #residuals not distributed normally
bartlett.test(Dark_resp ~ type, data = nns) #homoscedascity (barely)

#can't do a Box-Cox bc the values are negative
kruskal.test(Dark_resp ~ type, data = nns) #p-value = 0.7791; NS

(dr_boxplot <- ggplot(nns, 
                      aes(x = factor(type, levels = c('Native', 'Naturalised', 'Invasive')), #reorders the types 
                          y = Dark_resp, fill = type)) + 
    geom_boxplot() + #creates the boxplot
    stat_boxplot(geom ='errorbar', width = 0.3) + #adds the whisker ends
    geom_jitter() + #adds the jitter
    scale_fill_manual(values = traits.palette) + 
    labs(x = "\n Invasion status", 
         y = expression(atop(paste("Dark respiration rate (", mu, "mol CO"[2] ~ "m"^-2*~"s"^-1, ")")))) +
    theme_classic() + 
    theme(axis.text = element_text(size = 10), 
          axis.title = element_text(size = 11), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

ggsave("dr_boxplot.jpg", dr_boxplot, path = "Plots", units = "cm", width = 20, height = 15) 

#No dunn since no significant effect?




#Step 2: compare alien species ----
#LMA ----
lma_mod2 <- lm(lma ~ code_two, data = trees)
autoplot(lma_mod2)
shapiro.test(resid(lma_mod2)) #residuals not distributed normally
bartlett.test(lma ~ code_two, data = trees) #heteroscedascity

#Attempt mathematical transformation first to meet ANOVA assumptions:
lma_boxcox2 <- boxcox(lma ~ 1, data = trees) #the λ is the highest point on the curve
(lma_lambda2 <- lma_boxcox2$x[which.max(lma_boxcox2$y)]) #λ = -0.1818182
trees <- trees %>% mutate(transformed_lma = (lma ^ (lma_lambda2 - 1)) / lma_lambda2) #Box-Cox transformation applied in a new column

lma_mod_trans2 <- lm(transformed_lma ~ type, data = trees)
autoplot(lma_mod_trans2)
shapiro.test(resid(lma_mod_trans2)) #residuals not distributed normally
bartlett.test(transformed_lma ~ type, data = trees) #heteroscedascity

#Transformation did not work, moving on to non-parametric alternative:
(lma_kw2 <- kruskal.test(lma ~ type, data = trees)) #p-value = 6.206e-07; significant



(lma_boxplot2 <- ggplot(trees, 
                       aes(x = factor(code_two, levels = 
                                        c('Native', 'Naturalised', 'Invasive', 
                                          'C. bullatus', 'Q. cerris', "Q. ilex", 
                                          'R. pseudoacacia semperflorens')), #reorders the types 
                           y = lma, fill = code_two)) + 
    geom_boxplot() + #creates the boxplot
    stat_boxplot(geom ='errorbar', width = 0.3) + #adds the whisker ends
    geom_jitter() + #adds the jitter
    scale_fill_manual(values = c("Invasive" = "#CD6090", "Native" = "#698B69",
                                 "Naturalised" = "#EEC900", "C. bullatus" = "steelblue3",
                                 "Q. cerris" = "steelblue3", "Q. ilex" = "steelblue3",
                                 "R. pseudoacacia semperflorens" = "steelblue3")) + #colours each boxplot this particular colour
    labs(x = "\n Invasion status", 
         y = expression(atop("LMA (g/cm"^2*")"))) + 
    theme_classic() + 
    theme(axis.text = element_text(size = 10), 
          axis.title = element_text(size = 11), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "top"))

ggsave("lma_boxplot2.jpg", lma_boxplot2, path = "Plots", units = "cm", width = 30, height = 15) 



#Dunn post-hoc test
dunn_lma_2 <- dunn.test(trees$lma, trees$code_two, method = "bonferroni") #invasives differ significantly from natives yay
#naturalised also differ significantly from natives







#Step 3 - Mixed effect models?? ----
#random effect models?? ----
null_lma <- lm(lma ~ 1, data = nns)
model_lma <- lmer(lma ~ type + (1 | ever_dec), data = nns)
model_lma_1 <- lmer(lma ~ type + (1 | code) + (1 | age) +  (1 | ever_dec), data = nns) 
model_lma_2 <- lmer(lma ~ type + (1 | code) + (1 | age) +  (1 | ever_dec) + (1 | canopy_pos), data = nns)
model_lma_3 <- lmer(lma ~ type + (1 | code) + (1 | age) +  (1 | ever_dec) + (1 | canopy_pos) + (1 | dbh), data = nns)
model_lma_4 <- lmer(lma ~ type + (1 | code) +  (1 | ever_dec) + (1 | canopy_pos), data = nns) #this one is best (accoridng to the AIC)
AIC(null_lma, model_lma, model_lma_1, model_lma_2, model_lma_3, model_lma_4)
#models 2 and 4 fall within 2 AIC scores; so virtually identical fit to the data
#looks like adding DBH does not add anything to the data, so I will not use it

#so for model_lma_2; there is still 12.56 residual std that is not explained by any of these random effects

model_chl_1 <- lmer(avg_chl ~ type + (1 | code) + (1 | age) +  (1 | ever_dec), data = trees)
model_chl_2 <- lmer(avg_chl ~ type + (1 | code) + (1 | age) +  (1 | ever_dec) + (1 | canopy_pos), data = trees)
model_chl_1 <- lmer(avg_chl ~ type + (1 | code) + (1 | age) +  (1 | ever_dec) + (1 | canopy_pos) + (1 | dbh), data = trees)

summary(glm(lma ~ type, data = subset_trees))



#Step 4 - NMDS
#NMDS- in progress ----
#LMA
lma <- nns %>% filter(type, lma)

lma_matrix <- lma_matrix %>%
  mutate(Invasive = ifelse(is.na(Invasive), mean(Invasive, na.rm = TRUE), Invasive),
         Native = ifelse(is.na(Native), mean(Native, na.rm = TRUE), Native),
         Naturalised = ifelse(is.na(Naturalised), mean(Naturalised, na.rm = TRUE), Naturalised))
nmds_result <- metaMDS(lma_matrix, distance = "bray") #why bray??
plot(nmds_result)

#PCA ----
pca_data <- trees %>% 
  select(lma, ldcm, A, E, g, chl)

pca <- princomp(pca_data, cor = TRUE, scores = TRUE)
pca$loadings #pca1 explains loads -> add the scree plot and see that the comp 1 explains a lot (plot(pca))
#lma and chl are the most important variables in the pca1; followied by g, A, and E (but the latter in the opp direction)
#add the pca1 and pca2 to the original dataset to plot 
#can go back to the models and use the pca as a resposne variable
#then decide how to name the groups (i.e. pca1 = morphological and chemical; pca2 = morhological and physiological)

biplot <- biplot(pca)

#workflow
#dimension reduction (pca) -> modelling -> post-hoc tests
#double check that the traits are normally distributed for the PCA (if not, then transform)
