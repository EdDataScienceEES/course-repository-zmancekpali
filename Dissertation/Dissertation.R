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
library(ggrepel)
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

(trees_counts <- trees %>%
    group_by(type) %>%
    summarise(unique_species = n_distinct(code)))
#1 invasive, 22 naturalised, 9 native, 4 alien


nns <- trees %>% 
  filter(type %in% c('Native', 'Naturalised', "Invasive")) %>% #excluding the alien group for initial analysis
  mutate(canopy_pos = recode(canopy_pos, 
                             "L" = "Lower",
                             "U" = "Upper")) %>%  #recode canopy positions from abbreviations
  arrange(type = factor(type, levels = c('Native', 'Naturalised', 'Invasive'))) %>%  #rearranges the categories in this order
  filter(A >= 0) #removed negative A values (they were dead leaves)

(nns_counts <- nns %>%
  group_by(type) %>%
  summarise(unique_species = n_distinct(code)))
#1 invasive, 22 naturalised, 9 native

traits.palette <- c("#CD6090", "#698B69", "#EEC900")    #defining 3 colours
traits.palette2 <- c("#CD6090", "#698B69", "#EEC900", "#5EA8D9", "#245C82", "#4A3E87", "#5A5DC7")

cn_trees <- read.csv("cn_analysis.csv")
cn_trees <- cn_trees %>% 
  mutate(canopy_pos = recode(canopy_pos, 
                             "L" = "Lower",
                             "U" = "Upper")) %>%  #recode canopy positions from abbreviations
  mutate(code_two = recode(code_two,
                           "CB" = "C. bullatus",
                           "QC" = "Q. cerris",
                           "QI" = "Q. ilex",
                           "RPS" = "R. pseudoacacia semperflorens")) %>% #recode alien species names
  arrange(code_two = factor(type, levels = c('Native', 'Naturalised', 'Invasive', 
                                             'C. bullatus', 'Q. cerris', 'Q. ilex', 
                                             'R. pseudoacacia semperflorens'))) %>% #rearranges the categories in this order
  mutate(c_n = C/N)

cn_trees$age <- as.numeric(cn_trees$age)

cn_nns <- cn_trees %>% 
  filter(type %in% c('Native', 'Naturalised', "Invasive")) %>% #excluding the alien group for initial analysis
  mutate(canopy_pos = recode(canopy_pos, 
                             "L" = "Lower",
                             "U" = "Upper")) %>%  #recode canopy positions from abbreviations
  arrange(type = factor(type, levels = c('Native', 'Naturalised', 'Invasive'))) %>%   #rearranges the categories in this order
  mutate(c_n = C/N)
  
#Exploration
head(nns)
str(nns)
head(cn_nns)

#Step 1: see whether NN and I species differ in their traits + respective box plots + post-hoc tests if significant
#LMA ----
lma_mod <- lm(lma ~ type, data = nns)
autoplot(lma_mod)
shapiro.test(resid(lma_mod)) #residuals not distributed normally
bartlett.test(lma ~ type, data = nns) #heteroscedascity

#Attempt mathematical transformation first to meet ANOVA assumptions:
lma_boxcox <- boxcox(lma ~ 1, data = nns) #the λ is the highest point on the curve
(lma_lambda <- lma_boxcox$x[which.max(lma_boxcox$y)]) #λ = -0.3434343
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


#Evapotransiration rate (??) ----
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
(tukey_e <- TukeyHSD(aov_e))
#no significant differences


#GH20 ----
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
bartlett.test(transformed_g ~ type, data = nns) #homoscedascity           

#Transformation did not work, moving on to non-parametric alternative:
kruskal.test(g ~ type, data = nns) #0.03313; significant

(g_boxplot <- ggplot(nns, 
                     aes(x = factor(type, levels = c('Native', 'Naturalised', 'Invasive')), #reorders the types 
                         y = g, fill = type)) + 
    geom_boxplot() + #creates the boxplot
    stat_boxplot(geom ='errorbar', width = 0.3) + #adds the whisker ends
    scale_fill_manual(values = traits.palette) + 
    labs(x = "\n Invasion status", 
         y = expression(atop(paste("Stomatal conductance rate (", mu, "mol CO"[2] ~ "m"^-2*~"s"^-1, ")")))) +
    theme_classic() + 
    theme(axis.text = element_text(size = 10), 
          axis.title = element_text(size = 11), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

ggsave("g_boxplot.jpg", g_boxplot, path = "Plots", units = "cm", width = 20, height = 15) 

#Dunn post-hoc test
dunn_g <- dunn.test(nns$g, nns$type, method = "bonferroni") #invasive differ significantly from natives and naturalised yay
#naturalised and natives show no significant difference


#%C ----
c_mod <- lm(C ~ type, data = cn_nns)
autoplot(c_mod)
shapiro.test(resid(c_mod)) #residuals not distributed normally
bartlett.test(C ~ type, data = cn_nns) #homoscedascity

#Attempt mathematical transformation first to meet ANOVA assumptions:
c_boxcox <- boxcox(C ~ 1, data = cn_nns) #the λ is the highest point on the curve
(c_lambda <- c_boxcox$x[which.max(c_boxcox$y)]) #λ = -0.7878788
cn_nns <- cn_nns %>% mutate(transformed_c = (C ^ (c_lambda - 1)) / c_lambda) #Box-Cox transformation applied in a new column

c_mod_trans <- lm(transformed_c ~ type, data = cn_nns)
autoplot(c_mod_trans)
shapiro.test(resid(c_mod_trans)) #residuals not distributed normally
bartlett.test(transformed_c ~ type, data = cn_nns) #homoscedascity

#Transformation did not work, moving on to non-parametric alternative:
(c_kw <- kruskal.test(C ~ type, data = cn_nns)) #p-value = 0.008; significant

(c_boxplot <- ggplot(cn_nns, 
                       aes(x = factor(type, levels = c('Native', 'Naturalised', 'Invasive')), #reorders the types 
                           y = C, fill = type)) + 
    geom_boxplot() + #creates the boxplot
    stat_boxplot(geom ='errorbar', width = 0.3) + #adds the whisker ends
    scale_fill_manual(values = traits.palette) + 
    labs(x = "\n Invasion status", 
         y = expression(atop("% Carbon by dry weight"))) + 
    theme_classic() + 
    theme(axis.text = element_text(size = 10), 
          axis.title = element_text(size = 11), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

ggsave("c_boxplot.jpg", c_boxplot, path = "Plots", units = "cm", width = 20, height = 15) 

#Dunn post-hoc test
dunn_c <- dunn.test(cn_nns$C, cn_nns$type, method = "bonferroni") #invasives don't differ significantly from natives

#%N ----
n_mod <- lm(N ~ type, data = cn_nns)
autoplot(n_mod)
shapiro.test(resid(n_mod)) #residuals not distributed normally
bartlett.test(N ~ type, data = cn_nns) #heteroscedascity

#Attempt mathematical transformation first to meet ANOVA assumptions:
n_boxcox <- boxcox(N ~ 1, data = cn_nns) #the λ is the highest point on the curve
(n_lambda <- n_boxcox$x[which.max(n_boxcox$y)]) #λ = 0.3838384
cn_nns <- cn_nns %>% mutate(transformed_n = (N ^ (n_lambda - 1)) / n_lambda) #Box-Cox transformation applied in a new column

n_mod_trans <- lm(transformed_n ~ type, data = cn_nns)
autoplot(n_mod_trans)
shapiro.test(resid(n_mod_trans)) #residuals not distributed normally
bartlett.test(transformed_n ~ type, data = cn_nns) #heteroscedascity

#Transformation did not work, moving on to non-parametric alternative:
(n_kw <- kruskal.test(N ~ type, data = cn_nns)) #p-value = 0.01013; significant

(n_boxplot <- ggplot(cn_nns, 
                     aes(x = factor(type, levels = c('Native', 'Naturalised', 'Invasive')), #reorders the types 
                         y = N, fill = type)) + 
    geom_boxplot() + #creates the boxplot
    stat_boxplot(geom ='errorbar', width = 0.3) + #adds the whisker ends
    scale_fill_manual(values = traits.palette) + 
    labs(x = "\n Invasion status", 
         y = expression(atop("% Nitrogen by dry weight"))) + 
    theme_classic() + 
    theme(axis.text = element_text(size = 10), 
          axis.title = element_text(size = 11), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

ggsave("n_boxplot.jpg", c_boxplot, path = "Plots", units = "cm", width = 20, height = 15) 

#Dunn post-hoc test
dunn_n <- dunn.test(cn_nns$N, cn_nns$type, method = "bonferroni") #invasives differ significantly from natives

#C:N ----
cn_mod <- lm(c_n ~ type, data = cn_nns)
autoplot(cn_mod)
shapiro.test(resid(cn_mod)) #residuals not distributed normally
bartlett.test(c_n ~ type, data = cn_nns) #homoscedascity

#Attempt mathematical transformation first to meet ANOVA assumptions:
cn_boxcox <- boxcox(c_n ~ 1, data = cn_nns) #the λ is the highest point on the curve
(cn_lambda <- cn_boxcox$x[which.max(cn_boxcox$y)]) #λ = -0.666667
cn_nns <- cn_nns %>% mutate(transformed_cn = (c_n ^ (cn_lambda - 1)) / cn_lambda) #Box-Cox transformation applied in a new column

cn_mod_trans <- lm(transformed_cn ~ type, data = cn_nns)
autoplot(cn_mod_trans)
shapiro.test(resid(cn_mod_trans)) #residuals not distributed normally
bartlett.test(transformed_cn ~ type, data = cn_nns) #heteroscedascity

#Transformation did not work, moving on to non-parametric alternative:
(cn_kw <- kruskal.test(c_n ~ type, data = cn_nns)) #p-value = 0.004006; significant

(cn_boxplot <- ggplot(cn_nns, 
                     aes(x = factor(type, levels = c('Native', 'Naturalised', 'Invasive')), #reorders the types 
                         y = c_n, fill = type)) + 
    geom_boxplot() + #creates the boxplot
    stat_boxplot(geom ='errorbar', width = 0.3) + #adds the whisker ends
    scale_fill_manual(values = traits.palette) + 
    labs(x = "\n Invasion status", 
         y = expression(atop("C:N"))) + 
    theme_classic() + 
    theme(axis.text = element_text(size = 10), 
          axis.title = element_text(size = 11), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

ggsave("cn_boxplot.jpg", cn_boxplot, path = "Plots", units = "cm", width = 20, height = 15) 

#Dunn post-hoc test
dunn_cn <- dunn.test(cn_nns$c_n, cn_nns$type, method = "bonferroni") #invasives differ significantly from natives





#Step 2: compare alien species ----
#LMA ----
lma_mod2 <- lm(lma ~ code_two, data = trees)
autoplot(lma_mod2)
shapiro.test(resid(lma_mod2)) #residuals not distributed normally
bartlett.test(lma ~ code_two, data = trees) #heteroscedascity

#Attempt mathematical transformation first to meet ANOVA assumptions:
lma_boxcox2 <- boxcox(lma ~ 1, data = trees) #the λ is the highest point on the curve
(lma_lambda2 <- lma_boxcox2$x[which.max(lma_boxcox2$y)]) #λ = -0.1818182
trees <- trees %>% mutate(transformed_lma2 = (lma ^ (lma_lambda2 - 1)) / lma_lambda2) #Box-Cox transformation applied in a new column

lma_mod_trans2 <- lm(transformed_lma2 ~ type, data = trees)
autoplot(lma_mod_trans2)
shapiro.test(resid(lma_mod_trans2)) #residuals not distributed normally
bartlett.test(transformed_lma2 ~ type, data = trees) #heteroscedascity

#Transformation did not work, moving on to non-parametric alternative:
(lma_kw2 <- kruskal.test(lma ~ type, data = trees)) #p-value = 2.575e-08; significant

(lma_boxplot2 <- ggplot(trees, 
                       aes(x = factor(code_two, levels = 
                                        c('Native', 'Naturalised', 'Invasive', 
                                          'C. bullatus', 'Q. cerris', "Q. ilex", 
                                          'R. pseudoacacia semperflorens')), #reorders the types 
                           y = lma, fill = code_two)) + 
    geom_boxplot() + #creates the boxplot
    stat_boxplot(geom ='errorbar', width = 0.3) + #adds the whisker ends
    scale_fill_manual(values = c("Invasive" = "#CD6090", "Native" = "#698B69",
                                 "Naturalised" = "#EEC900", "C. bullatus" = "steelblue3",
                                 "Q. cerris" = "steelblue3", "Q. ilex" = "steelblue3",
                                 "R. pseudoacacia semperflorens" = "steelblue3")) + #colours each boxplot this particular colour
    labs(x = "\n Invasion status", 
         y = expression(atop("LMA (g/cm"^2*")"))) + 
    theme_classic() + 
    theme(axis.text.x = element_text(face = c("plain", "plain", "plain", "italic", "italic", "italic", "italic")),  # Italicize selected names
          axis.text = element_text(size = 10), 
          axis.title = element_text(size = 11), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = "cm")) +
    guides(fill = FALSE))

ggsave("lma_boxplot2.jpg", lma_boxplot2, path = "Plots", units = "cm", width = 30, height = 15) 


#Dunn post-hoc test
dunn_lma_2 <- dunn.test(trees$lma, trees$code_two, method = "bonferroni") 
#C. bullatus differs significantly from native species; others do not

#Average chlorophyll ----
chl_mod2 <- lm(chl ~ code_two, data = trees)
autoplot(chl_mod2)
shapiro.test(resid(chl_mod2)) #residuals not distributed normally
bartlett.test(chl ~ code_two, data = trees) #heteroscedascity

#Attempt mathematical transformation first to meet ANOVA assumptions:
chl_boxcox2 <- boxcox(chl ~ 1, data = trees) #the λ is the highest point on the curve
(chl_lambda2 <- chl_boxcox2$x[which.max(chl_boxcox2$y)]) #λ = -0.1010101
trees <- trees %>% mutate(transformed_chl2 = (chl ^ (chl_lambda2 - 1)) / chl_lambda2) #Box-Cox transformation applied in a new column

chl_mod_trans2 <- lm(transformed_chl2 ~ type, data = trees)
autoplot(chl_mod_trans2)
shapiro.test(resid(chl_mod_trans2)) #residuals not distributed normally
bartlett.test(transformed_chl2 ~ type, data = trees) #heteroscedascity

#Transformation did not work, moving on to non-parametric alternative:
(chl_kw2 <- kruskal.test(chl ~ type, data = trees)) #p-value = 6.305e-05; significant


(chl_boxplot2 <- ggplot(trees, 
                        aes(x = factor(code_two, levels = 
                                         c('Native', 'Naturalised', 'Invasive', 
                                           'C. bullatus', 'Q. cerris', "Q. ilex", 
                                           'R. pseudoacacia semperflorens')), #reorders the types 
                            y = chl, fill = code_two)) + 
    geom_boxplot() + #creates the boxplot
    stat_boxplot(geom ='errorbar', width = 0.3) + #adds the whisker ends
    scale_fill_manual(values = c("Invasive" = "#CD6090", "Native" = "#698B69",
                                 "Naturalised" = "#EEC900", "C. bullatus" = "steelblue3",
                                 "Q. cerris" = "steelblue3", "Q. ilex" = "steelblue3",
                                 "R. pseudoacacia semperflorens" = "steelblue3")) + #colours each boxplot this particular colour
    labs(x = "\n Invasion status", 
         y = expression(atop("Average chlorophyll (SPAD)"))) + 
    theme_classic() + 
    theme(axis.text.x = element_text(face = c("plain", "plain", "plain", "italic", "italic", "italic", "italic")),  # Italicize selected names
          axis.text = element_text(size = 10), 
          axis.title = element_text(size = 11), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = "cm")) +
    guides(fill = FALSE))

ggsave("chl_boxplot2.jpg", chl_boxplot2, path = "Plots", units = "cm", width = 30, height = 15) 


#Dunn post-hoc test
dunn_chl_2 <- dunn.test(trees$chl, trees$code_two, method = "bonferroni") #invasives differ significantly from natives yay
#Q. cerris, C. bullatus, and RPS differ significantly from natives and naturalised
#RPS differs significantly from other alien species too (significantly different from all other categories??)

#LDCM ----
ldcm_mod2 <- lm(ldcm ~ code_two, data = trees)
autoplot(ldcm_mod2)
shapiro.test(resid(ldcm_mod2)) #residuals distributed normally
bartlett.test(ldcm ~ code_two, data = trees) #homoscedascity
anova(ldcm_mod2)
#significant, p = 1.989e-07

(ldcm_kw2 <- kruskal.test(ldcm ~ type, data = trees)) #p-value = 0.001723; significant


(ldcm_boxplot2 <- ggplot(trees, 
                        aes(x = factor(code_two, levels = 
                                         c('Native', 'Naturalised', 'Invasive', 
                                           'C. bullatus', 'Q. cerris', "Q. ilex", 
                                           'R. pseudoacacia semperflorens')), #reorders the types 
                            y = ldcm, fill = code_two)) + 
    geom_boxplot() + #creates the boxplot
    stat_boxplot(geom ='errorbar', width = 0.3) + #adds the whisker ends
    scale_fill_manual(values = c("Invasive" = "#CD6090", "Native" = "#698B69",
                                 "Naturalised" = "#EEC900", "C. bullatus" = "steelblue3",
                                 "Q. cerris" = "steelblue3", "Q. ilex" = "steelblue3",
                                 "R. pseudoacacia semperflorens" = "steelblue3")) + #colours each boxplot this particular colour
    labs(x = "\n Invasion status", 
         y = expression(atop(paste("Leaf dry matter concentration (g" ~ "g"^-1~")")))) +
    theme_classic() + 
    theme(axis.text.x = element_text(face = c("plain", "plain", "plain", "italic", "italic", "italic", "italic")),  # Italicize selected names
          axis.text = element_text(size = 10), 
          axis.title = element_text(size = 11), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = "cm")) +
    guides(fill = FALSE))

ggsave("ldcm_boxplot2.jpg", a_boxplot2, path = "Plots", units = "cm", width = 30, height = 15) 


#Dunn post-hoc test
dunn_ldcm_2 <- dunn.test(trees$ldcm, trees$code_two, method = "bonferroni") #invasives differ significantly from natives yay
#none differ significantly from the natives
#Q. ilex differs significantly from naturalised and invasive species, and RPS


#Assimilation rate ----
a_mod2 <- lm(A ~ code_two, data = trees)
autoplot(a_mod2)
shapiro.test(resid(a_mod2)) #residuals distributed normally
bartlett.test(A ~ code_two, data = trees) #heteroscedascity

#Attempt mathematical transformation first to meet ANOVA assumptions:
a_boxcox2 <- boxcox(A ~ 1, data = trees) #the λ is the highest point on the curve
(a_lambda2 <- a_boxcox2$x[which.max(a_boxcox2$y)]) #λ = 0.7878788
trees <- trees %>% mutate(transformed_a2 = (A ^ (a_lambda2 - 1)) / a_lambda2) #Box-Cox transformation applied in a new column

a_mod_trans2 <- lm(transformed_a2 ~ type, data = trees)
autoplot(a_mod_trans2)
shapiro.test(resid(a_mod_trans2)) #residuals not distributed normally
bartlett.test(transformed_a2 ~ type, data = trees) #heteroscedascity

#Transformation did not work, moving on to non-parametric alternative:
(a_kw2 <- kruskal.test(A ~ type, data = trees)) #p-value = 0.001882; significant


(a_boxplot2 <- ggplot(trees, 
                      aes(x = factor(code_two, levels = 
                                       c('Native', 'Naturalised', 'Invasive', 
                                         'C. bullatus', 'Q. cerris', "Q. ilex", 
                                         'R. pseudoacacia semperflorens')), #reorders the types 
                          y = A, fill = code_two)) + 
    geom_boxplot() + #creates the boxplot
    stat_boxplot(geom ='errorbar', width = 0.3) + #adds the whisker ends
    scale_fill_manual(values = c("Invasive" = "#CD6090", "Native" = "#698B69",
                                 "Naturalised" = "#EEC900", "C. bullatus" = "steelblue3",
                                 "Q. cerris" = "steelblue3", "Q. ilex" = "steelblue3",
                                 "R. pseudoacacia semperflorens" = "steelblue3")) + #colours each boxplot this particular colour
    labs(x = "\n Invasion status", 
         y = expression(atop(paste("Assimilation rate (", mu, "mol CO"[2]~"m"^-2*~"s"^-1, ")")))) +
    theme_classic() + 
    theme(axis.text.x = element_text(face = c("plain", "plain", "plain", "italic", "italic", "italic", "italic")),  # Italicize selected names
          axis.text = element_text(size = 10), 
          axis.title = element_text(size = 11), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = "cm")) +
    guides(fill = FALSE))

ggsave("a_boxplot2.jpg", a_boxplot2, path = "Plots", units = "cm", width = 30, height = 15) 


#Dunn post-hoc test
dunn_a_2 <- dunn.test(trees$A, trees$code_two, method = "bonferroni") #invasives differ significantly from natives yay
#none differ significantly from the natives
#Q. ilex differs significantly from invasives


#Evapotranspiration (??) rate ----
e_mod2 <- lm(E ~ code_two, data = trees)
autoplot(e_mod2)
shapiro.test(resid(e_mod2)) #residuals distributed normally
bartlett.test(E ~ code_two, data = trees) #homoscedascity

anova(e_mod2) #NS; p-value = 0.9352

(e_boxplot2 <- ggplot(trees, 
                      aes(x = factor(code_two, levels = 
                                       c('Native', 'Naturalised', 'Invasive', 
                                         'C. bullatus', 'Q. cerris', "Q. ilex", 
                                         'R. pseudoacacia semperflorens')), #reorders the types 
                          y = E, fill = code_two)) + 
    geom_boxplot() + #creates the boxplot
    stat_boxplot(geom ='errorbar', width = 0.3) + #adds the whisker ends
    scale_fill_manual(values = c("Invasive" = "#CD6090", "Native" = "#698B69",
                                 "Naturalised" = "#EEC900", "C. bullatus" = "steelblue3",
                                 "Q. cerris" = "steelblue3", "Q. ilex" = "steelblue3",
                                 "R. pseudoacacia semperflorens" = "steelblue3")) + #colours each boxplot this particular colour
    labs(x = "\n Invasion status", 
         y = expression(atop(paste("Evapotranspiration rate (", mu, "mol CO"[2]~"m"^-2*~"s"^-1, ")")))) +
    theme_classic() + 
    theme(axis.text.x = element_text(face = c("plain", "plain", "plain", "italic", "italic", "italic", "italic")),  # Italicize selected names
          axis.text = element_text(size = 10), 
          axis.title = element_text(size = 11), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = "cm")) +
    guides(fill = FALSE))

ggsave("e_boxplot2.jpg", e_boxplot2, path = "Plots", units = "cm", width = 30, height = 15) 






#GH2O ----
g_mod2 <- lm(g ~ code_two, data = trees)
autoplot(g_mod2)
shapiro.test(resid(g_mod2)) #residuals not distributed normally
bartlett.test(g ~ code_two, data = trees) #homoscedascity

#Attempt mathematical transformation first to meet ANOVA assumptions:
g_boxcox2 <- boxcox(g ~ 1, data = trees) #the λ is the highest point on the curve
(g_lambda2 <- g_boxcox2$x[which.max(g_boxcox2$y)]) #λ = -0.7474747
trees <- trees %>% mutate(transformed_g2 = (g ^ (g_lambda2 - 1)) / g_lambda2) #Box-Cox transformation applied in a new column

g_mod_trans2 <- lm(transformed_g2 ~ type, data = trees)
autoplot(g_mod_trans2)
shapiro.test(resid(g_mod_trans2)) #residuals not distributed normally
bartlett.test(transformed_g2 ~ type, data = trees) #homoscedascity

#Transformation did not work, moving on to non-parametric alternative:
(g_kw2 <- kruskal.test(g ~ type, data = trees)) #p-value = 0.04557; significant


(g_boxplot2 <- ggplot(trees, 
                      aes(x = factor(code_two, levels = 
                                       c('Native', 'Naturalised', 'Invasive', 
                                         'C. bullatus', 'Q. cerris', "Q. ilex", 
                                         'R. pseudoacacia semperflorens')), #reorders the types 
                          y = g, fill = code_two)) + 
    geom_boxplot() + #creates the boxplot
    stat_boxplot(geom ='errorbar', width = 0.3) + #adds the whisker ends
    scale_fill_manual(values = c("Invasive" = "#CD6090", "Native" = "#698B69",
                                 "Naturalised" = "#EEC900", "C. bullatus" = "steelblue3",
                                 "Q. cerris" = "steelblue3", "Q. ilex" = "steelblue3",
                                 "R. pseudoacacia semperflorens" = "steelblue3")) + #colours each boxplot this particular colour
    labs(x = "\n Invasion status", 
         y = expression(atop(paste("Stomatal conductance rate (", mu, "mol CO"[2]~"m"^-2*~"s"^-1, ")")))) +
    theme_classic() + 
    theme(axis.text.x = element_text(face = c("plain", "plain", "plain", "italic", "italic", "italic", "italic")),  # Italicize selected names
          axis.text = element_text(size = 10), 
          axis.title = element_text(size = 11), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = "cm")) +
    guides(fill = FALSE))

ggsave("g_boxplot2.jpg", g_boxplot2, path = "Plots", units = "cm", width = 30, height = 15) 


#Dunn post-hoc test
dunn_g_2 <- dunn.test(trees$g, trees$code_two, method = "bonferroni") #invasives differ significantly from natives yay
#C. bullatus differs significantly from native and naturalised species






#C:N ----
cn_mod2 <- lm(c_n ~ code_two, data = cn_trees)
autoplot(cn_mod2)
shapiro.test(resid(cn_mod2)) #residuals not distributed normally
bartlett.test(c_n ~ code_two, data = cn_trees) #homoscedascity

#Attempt mathematical transformation first to meet ANOVA assumptions:
cn_boxcox2 <- boxcox(c_n ~ 1, data = cn_trees) #the λ is the highest point on the curve
(cn_lambda2 <- cn_boxcox2$x[which.max(cn_boxcox2$y)]) #λ = 0.5454545
cn_trees <- cn_trees %>% mutate(transformed_cn2 = (c_n ^ (cn_lambda2 - 1)) / cn_lambda2) #Box-Cox transformation applied in a new column

cn_mod_trans2 <- lm(transformed_cn2 ~ code_two, data = cn_trees)
autoplot(cn_mod_trans2)
shapiro.test(resid(cn_mod_trans2)) #residuals distributed normally
bartlett.test(transformed_cn2 ~ code_two, data = cn_trees) #homoscedascity

#Transformation did not work, moving on to non-parametric alternative:
(cn_kw2 <- kruskal.test(c_n ~ code_two, data = cn_trees)) #p-value = 0.0003939; significant

(cn_boxplot2 <- ggplot(cn_trees, 
                      aes(x = factor(code_two, levels = c('Native', 'Naturalised', 'Invasive', 
                                                          'C. bullatus', 'Q. cerris', "Q. ilex", 
                                                          'R. pseudoacacia semperflorens')), #reorders the types 
                          y = c_n, fill = code_two)) + 
    geom_boxplot() + #creates the boxplot
    stat_boxplot(geom ='errorbar', width = 0.3) + #adds the whisker ends
    scale_fill_manual(values = c("Invasive" = "#CD6090", "Native" = "#698B69",
                                   "Naturalised" = "#EEC900", "C. bullatus" = "steelblue3",
                                   "Q. cerris" = "steelblue3", "Q. ilex" = "steelblue3",
                                   "R. pseudoacacia semperflorens" = "steelblue3")) + 
    labs(x = "\n Invasion status", 
         y = expression(atop("C:N"))) + 
    theme_classic() + 
    theme(axis.text.x = element_text(face = c("plain", "plain", "plain", "italic", "italic", "italic", "italic")),  # Italicize selected names
          axis.text = element_text(size = 10), 
          axis.title = element_text(size = 11), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = "cm")) +
    guides(fill = FALSE))

ggsave("cn_boxplot2.jpg", cn_boxplot2, path = "Plots", units = "cm", width = 20, height = 15) 

#Dunn post-hoc test
dunn_cn <- dunn.test(cn_trees$c_n, cn_trees$code_two, method = "bonferroni") #invasives differ significantly from natives





#Step 3 - Mixed effect models?? ----
#LMA LMER ----
null_lma <- lm(lma ~ 1, data = nns)
model_lma <- lmer(lma ~ type + (1 | ever_dec), data = nns)
model_lma_1 <- lmer(lma ~ type + (1 | code) + (1 | age) +  (1 | ever_dec), data = nns) 
model_lma_2 <- lmer(lma ~ type + (1 | code) + (1 | age) +  (1 | ever_dec) + (1 | canopy_pos), data = nns)
model_lma_3 <- lmer(lma ~ type + (1 | code) + (1 | age) +  (1 | ever_dec) + (1 | canopy_pos) + (1 | dbh), data = nns)
AIC(null_lma, model_lma, model_lma_1, model_lma_2, model_lma_3)
#models 2 and 3 fall within 2 AIC scores; so virtually identical fit to the data
#using model 2 (simplest of the two)
#so for model_lma_2; there is still 9.272 residual std that is not explained by any of these random effects

#Chl LMER ----
null_chl <- lm(chl ~ 1, data = nns)
model_chl_1 <- lmer(chl ~ type + (1 | code) + (1 | age) +  (1 | ever_dec), data = nns)
model_chl_2 <- lmer(chl ~ type + (1 | code) + (1 | age) +  (1 | ever_dec) + (1 | canopy_pos), data = nns)
model_chl_3 <- lmer(chl ~ type + (1 | code) + (1 | age) +  (1 | ever_dec) + (1 | canopy_pos) + (1 | dbh), data = nns)
AIC(null_chl, model_chl_1, model_chl_2, model_chl_3)
#Models 1 and 2 fall within two AIC scores, so i'll use 1 (simplest)
#3.611 residual std that isn't explained by the rest of the variables

#A LMER ----
null_a <- lm(A ~ 1, data = nns)
model_a_1 <- lmer(A ~ type + (1 | code) + (1 | age) +  (1 | ever_dec), data = nns)
model_a_2 <- lmer(A ~ type + (1 | code) + (1 | age) +  (1 | ever_dec) + (1 | canopy_pos), data = nns)
model_a_3 <- lmer(A ~ type + (1 | code) + (1 | age) +  (1 | ever_dec) + (1 | canopy_pos) + (1 | dbh), data = nns)
AIC(null_a, model_a_1, model_a_2, model_a_3)
#models 2 and 3 are the same; will use 2 (simplest of the two)
#1.689972 residual unexplained variance

#LDMC LMER ----
null_ldmc <- lm(ldcm ~ 1, data = nns)
model_ldmc_1 <- lmer(ldcm ~ type + (1 | code) + (1 | age) +  (1 | ever_dec), data = nns)
model_ldmc_2 <- lmer(ldcm ~ type + (1 | code) + (1 | age) +  (1 | ever_dec) + (1 | canopy_pos), data = nns)
model_ldmc_3 <- lmer(ldcm ~ type + (1 | code) + (1 | age) +  (1 | ever_dec) + (1 | canopy_pos) + (1 | dbh), data = nns)
AIC(null_ldmc, model_ldmc_1, model_ldmc_2, model_ldmc_3)
#models 2 and 3 are virtually identical, so will use 2 (simplest)
#0.06139 residual unexplained variance

#E LMER ----
null_e <- lm(E ~ 1, data = nns)
model_e_1 <- lmer(E ~ type + (1 | code) + (1 | age) +  (1 | ever_dec), data = nns)
model_e_2 <- lmer(E ~ type + (1 | code) + (1 | age) +  (1 | ever_dec) + (1 | canopy_pos), data = nns)
model_e_3 <- lmer(E ~ type + (1 | code) + (1 | age) +  (1 | ever_dec) + (1 | canopy_pos) + (1 | dbh), data = nns)
AIC(null_e, model_e_1, model_e_2, model_e_3)
#models 1, 2, and 3 are the same; will use 1 (simplest of the three)
#1.2057 residual unexplained variance

#GH2O LMER ----
null_g <- lm(g ~ 1, data = nns)
model_g_1 <- lmer(g ~ type + (1 | code) + (1 | age) +  (1 | ever_dec), data = nns)
model_g_2 <- lmer(g ~ type + (1 | code) + (1 | age) +  (1 | ever_dec) + (1 | canopy_pos), data = nns)
model_g_3 <- lmer(g ~ type + (1 | code) + (1 | age) +  (1 | ever_dec) + (1 | canopy_pos) + (1 | dbh), data = nns)
AIC(null_g, model_g_1, model_g_2, model_g_3)
#models 1 and 2 are the same; will use 1 (simplest of the two)
#11.922205 residual unexplained variance


#C:N LMER ----
null_cn <- lm(c_n ~ 1, data = cn_nns)
model_cn_1 <- lmer(c_n ~ type + (1 | code) + (1 | age) +  (1 | ever_dec), data = cn_nns)
model_cn_2 <- lmer(c_n ~ type + (1 | code) + (1 | age) +  (1 | ever_dec) + (1 | canopy_pos), data = cn_nns)
model_cn_3 <- lmer(c_n ~ type + (1 | code) + (1 | age) +  (1 | ever_dec) + (1 | canopy_pos) + (1 | dbh), data = cn_nns)
AIC(null_cn, model_cn_1, model_cn_2, model_cn_3)
#all models are virtually the same; will use 1 (simplest of the three)
#2.039e+00 residual unexplained variance





#NMDS----
physiological <- nns %>% select(A, E, g)
phys_alien <- trees %>% select(A, E, g)
morphological <- nns %>% select(lma, ldcm)
morph_alien <- trees %>% select(lma, ldcm)
chemical <- cn_nns %>% select(C, N, c_n)
chem_alien <- cn_trees %>% select(C, N, c_n)
#Morphological NMDS (nns) ----
numeric_cols_morph <- colnames(morphological)[sapply(morphological, is.numeric)] 
numeric_data_morph <- morphological[, numeric_cols_morph]
numeric_data_morph <- numeric_data_morph %>% select(lma, ldcm)

nmds_morph <- metaMDS(morphological, distance = "euclidean")
nmds_coords_morph <- as.data.frame(scores(nmds_morph, "sites"))
nmds_coords_morph$type <- nns$type

hull.data <- data.frame()
for (i in unique(nmds_coords_morph$type)) {
  temp <- nmds_coords_morph[nmds_coords_morph$type == i, ][chull(nmds_coords_morph[nmds_coords_morph$type == i, c("NMDS1", "NMDS2")]), ]
  hull.data <- rbind(hull.data, temp)
}

(nmds_morph <- ggplot() +
    geom_polygon(data = hull.data[hull.data$type != "Invasive", ], aes(x = NMDS1, y = NMDS2, group = type, fill = type), alpha = 0.5) + #add polygons for non-invasive types
    geom_polygon(data = hull.data[hull.data$type == "Invasive", ], aes(x = NMDS1, y = NMDS2, group = type, fill = type), alpha = 0.8) + #add polygons for invasive type
    geom_point(data = nmds_coords_morph, aes(x = NMDS1, y = NMDS2, color = type), size = 3) + # Add points
    scale_color_manual(values = c("Native" = "#698B69", "Invasive" = "#CD6090", "Naturalised" = "#EEC900")) +
    scale_fill_manual(values = c("Native" = "#698B69", "Invasive" = "#CD6090", "Naturalised" = "#EEC900")) +
    theme_classic() +
    theme(legend.position = c(0.9, 0.9), legend.direction = "vertical", legend.title = element_blank()) +
    labs(title = "NMDS Plot of Morphological Leaf Traits by Invasion Type"))
ggsave("morphological_nmds.jpg", nmds_morph, path = "Plots", units = "cm", 
       width = 20, height = 20) 

diss_matrix_morph <- vegdist(morphological, method = "bray")
anosim(diss_matrix_morph, nns$type, permutations = 9999) 
#significant, the three types are significantly different in their morphological traits (p = 0.0151);
#however, the R value is close to 0 (0.06575), indicating a slight but significant difference between the groups

#Morphological NMDS (alien) ----
numeric_cols_morph_alien <- colnames(morph_alien)[sapply(morph_alien, is.numeric)] 
numeric_data_morph_alien <- morph_alien[, numeric_cols_morph_alien]
numeric_data_morph_alien <- numeric_data_morph_alien %>% select(lma, ldcm)

nmds_morph_alien <- metaMDS(morph_alien, distance = "euclidean")
nmds_coords_morph_alien <- as.data.frame(scores(nmds_morph_alien, "sites"))
nmds_coords_morph_alien$code_two <- trees$code_two

hull.data <- data.frame()
for (i in unique(nmds_coords_morph_alien$code_two)) {
  temp <- nmds_coords_morph_alien[nmds_coords_morph_alien$code_two == i, ][chull(nmds_coords_morph_alien[nmds_coords_morph_alien$code_two == i, c("NMDS1", "NMDS2")]), ]
  hull.data <- rbind(hull.data, temp)
}

(nmds_morph_alien <- ggplot() +
    geom_polygon(data = hull.data[hull.data$code_two != "Invasive", ], aes(x = NMDS1, y = NMDS2, group = code_two, fill = code_two), alpha = 0.5) + #add polygons for non-invasive types
    geom_polygon(data = hull.data[hull.data$code_two == "Invasive", ], aes(x = NMDS1, y = NMDS2, group = code_two, fill = code_two), alpha = 0.8) + #add polygons for invasive type
    geom_point(data = nmds_coords_morph_alien, aes(x = NMDS1, y = NMDS2, color = code_two, shape = code_two), size = 3) + # Add points
    scale_color_manual(values = c("Native" = "#698B69", "Invasive" = "#CD6090", 
                                  "Naturalised" = "#EEC900", "C. bullatus" = "#5EA8D9", 
                                  "Q. cerris" = "#245C82", "Q. ilex" = "#4A3E87", 
                                  "R. pseudoacacia semperflorens" = "#5A5DC7")) +
    scale_fill_manual(values = c("Native" = "#698B69", "Invasive" = "#CD6090", 
                                 "Naturalised" = "#EEC900", "C. bullatus" = "#5EA8D9", 
                                 "Q. cerris" = "#245C82", "Q. ilex" = "#4A3E87", 
                                 "R. pseudoacacia semperflorens" = "#5A5DC7")) +
    theme_classic() +
    theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank()) +
    labs(title = "NMDS Plot of Morphological Leaf Traits by Invasion Type"))
ggsave("morphological_nmds.jpg", nmds_morph, path = "Plots", units = "cm", 
       width = 20, height = 20) 

diss_matrix_morph <- vegdist(morphological, method = "bray")
anosim(diss_matrix_morph, nns$type, permutations = 9999) 
#Physiological NMDS (nns) ----
numeric_cols_phys <- colnames(physiological)[sapply(physiological, is.numeric)] 
numeric_data_phys <- physiological[, numeric_cols_phys]
numeric_data_phys <- numeric_data_phys %>% select(A, E, g)

nmds_phys <- metaMDS(physiological, distance = "euclidean")
nmds_coords_phys <- as.data.frame(scores(nmds_phys, "sites"))
nmds_coords_phys$type <- nns$type

hull.data <- data.frame()
for (i in unique(nmds_coords_phys$type)) {
  temp <- nmds_coords_phys[nmds_coords_phys$type == i, ][chull(nmds_coords_phys[nmds_coords_phys$type == i, c("NMDS1", "NMDS2")]), ]
  hull.data <- rbind(hull.data, temp)
}

(nmds_phys <- ggplot() +
    geom_polygon(data = hull.data[hull.data$type != "Invasive", ], aes(x = NMDS1, y = NMDS2, group = type, fill = type), alpha = 0.5) + #add polygons for non-invasive types
    geom_polygon(data = hull.data[hull.data$type == "Invasive", ], aes(x = NMDS1, y = NMDS2, group = type, fill = type), alpha = 0.8) + #add polygons for invasive type
    geom_point(data = nmds_coords_phys, aes(x = NMDS1, y = NMDS2, color = type), size = 3) + # Add points
    scale_color_manual(values = c("Native" = "#698B69", "Invasive" = "#CD6090", "Naturalised" = "#EEC900")) +
    scale_fill_manual(values = c("Native" = "#698B69", "Invasive" = "#CD6090", "Naturalised" = "#EEC900")) +
    theme_classic() +
    theme(legend.position = c(0.9, 0.9), legend.direction = "vertical", legend.title = element_blank()) +
    labs(title = "NMDS Plot of Physiological Leaf Traits by Invasion Type"))
ggsave("physiological_nmds.jpg", nmds_phys, path = "Plots", units = "cm", 
       width = 20, height = 20) 

diss_matrix_phys <- vegdist(physiological, method = "bray")
anosim(diss_matrix_phys, nns$type, permutations = 9999) 
#not significant, the three types are not significantly different in their physiological traits (p = 0.2227);
#the R is also close to 0 (0.01599), so this relationship is insignificant and a weak difference between the groups


#Chemical NMDS (nns) ----
numeric_cols_chem <- colnames(chemical)[sapply(chemical, is.numeric)] 
numeric_data_chem <- chemical[, numeric_cols_chem]
numeric_data_chem <- numeric_data_chem %>% select(C, N)

nmds_chem <- metaMDS(chemical, distance = "euclidean")
nmds_coords_chem <- as.data.frame(scores(nmds_chem, "sites"))
nmds_coords_chem$type <- cn_nns$type

hull.data <- data.frame()
for (i in unique(nmds_coords_chem$type)) {
  temp <- nmds_coords_chem[nmds_coords_chem$type == i, ][chull(nmds_coords_chem[nmds_coords_chem$type == i, c("NMDS1", "NMDS2")]), ]
  hull.data <- rbind(hull.data, temp)
}

(nmds_chem <- ggplot() +
    geom_polygon(data = hull.data[hull.data$type != "Invasive", ], aes(x = NMDS1, y = NMDS2, group = type, fill = type), alpha = 0.5) + #add polygons for non-invasive types
    geom_polygon(data = hull.data[hull.data$type == "Invasive", ], aes(x = NMDS1, y = NMDS2, group = type, fill = type), alpha = 0.8) + #add polygons for invasive type
    geom_point(data = nmds_coords_chem, aes(x = NMDS1, y = NMDS2, color = type), size = 3) + # Add points
    scale_color_manual(values = c("Native" = "#698B69", "Invasive" = "#CD6090", "Naturalised" = "#EEC900")) +
    scale_fill_manual(values = c("Native" = "#698B69", "Invasive" = "#CD6090", "Naturalised" = "#EEC900")) +
    theme_classic() +
    theme(legend.position = c(0.9, 0.9), legend.direction = "vertical", legend.title = element_blank()) +
    labs(title = "NMDS Plot of Chemical Leaf Traits by Invasion Type"))
ggsave("chemical_nmds.jpg", nmds_chem, path = "Plots", units = "cm", 
       width = 20, height = 20) 

diss_matrix_chem <- vegdist(chemical, method = "bray")
anosim(diss_matrix_chem, cn_nns$type, permutations = 9999) 
#significant, the three types are significantly different in their chemical traits (p = 0.0214);
#however, the R is close to 0 (0.09888), so this relationship is not very strong but still significant







#PCA in progress (badly and sadly) ---- 
#Morpholgical PCA ----
morphological$type <- factor(nns$type)
morphological <- mutate_if(morphological, is.character, as.numeric)
pca_morph <- prcomp(morphological, scale. = TRUE)
(morph_pca <- autoplot(pca_morph, data = morphological, color = 'type', alpha = 0.7, 
                       loadings = TRUE, size = 2, shape = 'type') +
  scale_color_manual(values = c("Native" = "#698B69", "Invasive" = "#CD6090", "Naturalised" = "#EEC900")) +
  theme_classic() +
  theme(legend.position = c(0.9, 0.9), legend.direction = "vertical", legend.title = element_blank()))
#bottom arrow is LDMC, top is LMA
summary(pca_morph)
#both axes explain 100% of the variance in the data; and PC1 explains more (55.09%)
#this means PC1 captures more variance (traits associated with PC1 contribute to more variability of the data)
(loadings <- pca_morph$rotation)
#both lma and ldmc have a strong negative correlation with PC1 (-0.7071068)
#lma has a positive correlation with PC2 (0.7071068); ldmc has a negative one (-0.7071068)
#These interpretations suggest that PC1 captures a trade-off between lma and ldcm, while PC2 captures variation primarily driven by lma alone.

#chat gpt's interpretation: 
#For our analysis, we identified two principal components, PC1 and PC2, which together explain a significant proportion of the variation in the morphological traits of invasive, native, and naturalized species.
#PC1: This principal component primarily represents a trade-off between leaf mass per area (LMA) and leaf dry matter content (LDCM). Both variables are strongly negatively correlated with PC1, suggesting that species with higher LMA tend to have lower LDCM, and vice versa. This finding indicates a functional trade-off in leaf traits related to resource acquisition and conservation strategies.
#PC2: PC2 primarily captures variation driven by LMA. The positive correlation between LMA and PC2 suggests that species with higher LMA values tend to exhibit higher scores on PC2. This could reflect differences in plant strategies related to resource allocation and structural investments in leaf tissues.

#Physiological PCA
physiological$type <- factor(nns$type)
physiological <- mutate_if(physiological, is.character, as.numeric)
pca_phys <- prcomp(physiological, scale. = TRUE)
(phys_pca <- autoplot(pca_phys, data = physiological, color = 'type', alpha = 0.7, 
                       loadings = TRUE, size = 2, shape = 'type') +
    scale_color_manual(values = c("Native" = "#698B69", "Invasive" = "#CD6090", "Naturalised" = "#EEC900")) +
    theme_classic() +
    theme(legend.position = c(0.9, 0.9), legend.direction = "vertical", legend.title = element_blank()))
#bottom arrow is LDMC, top is LMA
summary(pca_morph)
#both axes explain 100% of the variance in the data; and PC1 explains more (55.09%)
#this means PC1 captures more variance (traits associated with PC1 contribute to more variability of the data)
(loadings <- pca_morph$rotation)
#both lma and ldmc have a strong negative correlation with PC1 (-0.7071068)
#lma has a positive correlation with PC2 (0.7071068); ldmc has a negative one (-0.7071068)
#These interpretations suggest that PC1 captures a trade-off between lma and ldcm, while PC2 captures variation primarily driven by lma alone.

#chat gpt's interpretation: 
#For our analysis, we identified two principal components, PC1 and PC2, which together explain a significant proportion of the variation in the morphological traits of invasive, native, and naturalized species.
#PC1: This principal component primarily represents a trade-off between leaf mass per area (LMA) and leaf dry matter content (LDCM). Both variables are strongly negatively correlated with PC1, suggesting that species with higher LMA tend to have lower LDCM, and vice versa. This finding indicates a functional trade-off in leaf traits related to resource acquisition and conservation strategies.
#PC2: PC2 primarily captures variation driven by LMA. The positive correlation between LMA and PC2 suggests that species with higher LMA values tend to exhibit higher scores on PC2. This could reflect differences in plant strategies related to resource allocation and structural investments in leaf tissues.


#----
pca_data <- nns %>% 
  select(lma, ldcm, A, E, g, chl)

pca <- princomp(pca_data, cor = TRUE, scores = TRUE)
pca$loadings #pca1 explains loads -> add the scree plot and see that the comp 1 explains a lot (plot(pca))
#lma and chl are the most important variables in the pca1; followed by g, E, and A (but the latter in the opp direction)
#add the pca1 and pca2 to the original dataset to plot 
#can go back to the models and use the pca as a response variable
#then decide how to name the groups (i.e. pca1 = morphological and chemical; pca2 = morphological and physiological)

biplot <- biplot(pca)

#workflow
#dimension reduction (pca) -> modelling -> post-hoc tests
#double check that the traits are normally distributed for the PCA (if not, then transform)
hist(nns$lma)
shapiro.test(nns$lma) #non-normal
hist(nns$ldcm)
shapiro.test(nns$ldcm) #normal
hist(nns$A)
shapiro.test(nns$A) #non-normal
hist(nns$E)
shapiro.test(nns$E) #normal
hist(nns$g)
shapiro.test(nns$g) #non-normal
hist(nns$chl)
shapiro.test(nns$chl) #non-normal



