# *Orcinus orca* Population Trends in North American Marine Habitats since 1970
By Zoja Manček Páli, for the ZSL and WWF
November 20th, 2023

![pic](https://cdn.theatlantic.com/thumbor/0AeFF0OVHRBqCuNxOUMIplLQc50=/0x214:4114x2528/1952x1098/media/img/mt/2019/04/shutterstock_554899423_1/original.jpg)
Image credit: Tory Kallman / Shutterstock

_____________
## Introduction and background:
Due to the increasing speed of the effects of climate change and human pressures on habitats, it is increasingly important to understand how various species are reacting to these changes over time, with particular focus on endangered and protected species. The orca (or killer whale; *Orcinus orca*) is a protected marine mammal species listed as critically endangered by the IUCN (Reeves *et al*., 2017). 

Understanding how *O. orca* responds to these external stressors is crucial for marine conservation efforts as this species is often considered a sentinel for the health of marine ecosystems: changes in their abundance may signal broader ecological disruptions (Ford, 2009). The focus of this study is thus to evaluate the changes in population of *O. orca* over time in North American marine habitats.

Please note that this document only mentions the models that were shown to fit the data best - to see all of the models, please see [the script](https://github.com/EdDataScienceEES/challenge3-zmancekpali/blob/master/Challenge/script/Script.R). 

_____________
## The trends:

Our study found that *O. orca* abundances show variation over time, as well as within different habitats - see Figure 1 (left and right, respectfully). 

![fig 1](https://github.com/EdDataScienceEES/challenge3-zmancekpali/blob/master/Challenge/Plots/trends_grid.png)

**Figure 1** shows the trends of the *O. orcinus* abundance data over time from 1970 until 2014. The points represent the raw data, the lines the regression lines fit to the data. Left shows the combined data for North America, where we see fluctuations in the abundance and an overall decrease over the 44 years; right shows the same trends with the points colour-coded to represent each sampling site within North America (n = 212). 


While we saw significant variation within each site (Figure 2, left), we did not see significant variation between the two countries (Figure 2, right).  
![fig 2](https://github.com/EdDataScienceEES/challenge3-zmancekpali/blob/master/Challenge/Plots/site_grid.png)

**Figure 2** shows the trends of the *O. orcinus* abundance data between the different sites and countries. The box plots show the distribution of the data, with the box plots showing the minimum, first quartile, median, third quartile, and maximum abundance values for each site/country. 

The fluctuations and overall decrease over 44 years signify potential ecological disruptions.

_____________

## Statistical modelling: 
To understand the relationship between O. orca abundance and time, we initially employed a generalized linear mixed-effects model (GLMM, model1). However, the model assumptions were not met (Figure 4). To address this, we explored a transformed model (model3) and a generalized additive model (GAM, gam4). The GAM, designed to capture non-linear relationships, better adhered to the assumptions. Finally, however, we employed the more felxible Bayesian framework. For the breakdown of each of these models, see below.

---
To understand the relationship between *O. orca* abundance and time, we modelled the relationship using a generalised linear mixed effect model (hereafter GLMM) as below: 
```
model1 <- glmer(abundance ~ year + country + (1|location) + (1|year), family = poisson, data = orca)
```
where the population of *O. orca* is significantly impacted by time (year) and and country within which their populations are found (country). We also included two random effects (the variation between the 13 locations in the US and Canada (location), and the within-year variation (year); model1 in the script). The reason for including the inter-year and location variability as random effects is to capture any of the variability that a simpler model (that exlcudes these two variables) could not. The ```family = poisson``` command informs the model of the distibution of the data (in our case, a poisson distribution - see Figure 3).

![fig 3](https://github.com/EdDataScienceEES/challenge3-zmancekpali/blob/master/Challenge/Plots/orca_hist.png)

**Figure 3** shows the distribution of the *O. orcinus* abundance data, resembling a Poisson distribution. 

However, after testing the GLMM assumptions on this model (which must be met to be able to correctly interpret the results of the model) - they were not met - see Figure 4.

![fig 4](https://github.com/EdDataScienceEES/challenge3-zmancekpali/blob/master/Challenge/Plots/assumptions_checks_grid.jpg)

**Figure 4** shows the diagnostic plots of model1. The figure shows neither of the three tested assumptions were met: the relationship is not linear (left), and the variances are not equal (middle) or normally-distributed (right).

To account for these assumptions not being met, we transformed the year variable via a square-root mathematical transformation (model3 in the script):

```
model3 <- glmer(abundance ~ sqrt(year) + country + (1|location) + (1|year), family = poisson, data = orca)
```

This model showed improvents in the ability to meet the assumptions - see Figure 3. 

![fig5](https://github.com/EdDataScienceEES/challenge3-zmancekpali/blob/master/Challenge/Plots/assumptions_checks_sqrt_grid.jpg) 

**Figure 5** shows the diagnostic plots of model3. The figure shows a more linear relationship between the response and predictor variables (left), however, the variances are not equal (middle) or normally-distributed (right).

Mathematically, the distribution of the residuals could also be said to be normal, depending on the definition of statistical significance (i.e. p-value > 0.05 vs p-value > 0.01). For the sake of thoroughness, I will discuss here the results of model3, whose predictions can be seen in Figure 6.

![fig6](https://github.com/EdDataScienceEES/challenge3-zmancekpali/blob/master/Challenge/Plots/prediction_plot.png)

**Figure 6** shows the prediction plot for model3. The figure shows a scatter plot of predicted vs. observed *O. orca* abundances, and a line where observed values = predicted values. 

For the results of the model, please see [Model 3 results](https://github.com/EdDataScienceEES/challenge3-zmancekpali/blob/master/Challenge/Tables/model3_summary.txt).

---
In order to also design a model where the model assumptions weren't violated, I decided to use a generalised additive model (hereafter GAM), as such models are more receptive to non-linear relationships. See below for the code: 

```
gam4 <- gam(Abundance ~ s(Year, k = 40) + s(Country.list, bs = "fs") + s(Location.of.population, bs = "re") + s(Year, bs = "re", k = 40), data = orca, family = poisson)

```

This model appeared not to violate the assumptions of GAM models (see Figure 7; residuals appear normally distributed if the significance level is set at 0.01), and could thus be used to interpret the relationships between *O. orca* abundance over time in North America. 

![fig 7](https://github.com/EdDataScienceEES/challenge3-zmancekpali/blob/master/Challenge/Plots/resid_gam4.png)

**Figure 7** shows the distribution of the gam4 residuals. 


![fig 8](https://github.com/EdDataScienceEES/challenge3-zmancekpali/blob/master/Challenge/Plots/gam_check_4.png)

**Figure 8** shows the predictions for gam4. The figure shows a scatter plot of predicted vs. observed *O. orca* abundances, and a line where observed values = predicted values. 

For the full results of the gam4 model, please see [GAM4 results](https://github.com/EdDataScienceEES/challenge3-zmancekpali/blob/master/Challenge/Tables/gam4_summary.txt). 

_____
Alternatively, we could have also used a Bayesian framework (see below):

```
orca2_mbrms <- brms::brm(Abundance ~ I(Year - 1970) + (1|Year) +
                           (1|Location.of.population),
                         data = orca, family = poisson(), chains = 3,
                         iter = 3000, warmup = 1000)
```

To determine the fit of the model, we look at the diagnostic plots, which show it fits the data well (see Figures 9, 10, and 11). For the full results of the bayesian model see [Bayesian model results](https://github.com/EdDataScienceEES/challenge3-zmancekpali/blob/master/Challenge/Tables/bayes_orca_summary.txt).

![fig 9](https://github.com/EdDataScienceEES/challenge3-zmancekpali/blob/master/Challenge/Plots/bayes_plot.png)

**Figure 9** shows the model evaluation plots for orca2_mbrms. The figure shows that the model converges. 

![fig 10](https://github.com/EdDataScienceEES/challenge3-zmancekpali/blob/master/Challenge/Plots/pp_plot.png)

**Figure 10** shows the pp model evaluation plots for orca2_mbrms. The similarity between the two curves confirms the model fits the data well. 


![fig 11](https://github.com/EdDataScienceEES/challenge3-zmancekpali/blob/master/Challenge/Plots/bayes_grid.jpg)

**Figure 11** shows the model evaluation plots for orca2_mbrms. The figure shows a scatter plot of *O. orca* abundance values with the model lines fit through; shown with the associated credibility intervals. The left plot shows the overall trend, the right plot shows the trend for each population location. 

Overall, we can conclude that *O. orca* abundance has increased slightly between 1970 and 2014. 

_____________
## Results:
Overall, from both the results of the [GLMM](https://github.com/EdDataScienceEES/challenge3-zmancekpali/blob/master/Challenge/Tables/model3_summary.txt) and the [GAM](https://github.com/EdDataScienceEES/challenge3-zmancekpali/blob/master/Challenge/Tables/gam4_summary.txt), we can see that orca abundance has increased over the 44 years between 1970 and 2014. Both models show that there is a significant effect of year on *O. orca* abundance, and neither model found the effect of country to be singificant on orca abundance. The GAM model also shows no significant inter-year variation. The GAM model explains more than 93% of the deviation in the data, and has an R^2 value of 95% (very high for ecological contexts). All models were compared to related null-models to reduce bias. 

Comparing these two frameworks, I would only draw conclusions from the GAM as it met the assumptions and was more receptive to the type of data at hand. We can say that the model shows the relationship between *O. orca* abundance and year is not linear. The model also shows a singificant variability in *O. orca* abundances between the different North American sites. As the relationship is non-linear, there are few conclusions we can draw about the overall trend in North American *O. orca* populations, however, the GAM suggest an increase in *O. orca* since 1970. 

However, overall for the entire study, I will draw my final conclusions from the results of the Bayesian framework as they allow for the incorporation of prior knowledge, and are more flexible with small sample sizes (which can have a strong effect on frequentist linear models). The Bayesian model thus tells us a similar story: the *O. orca* log-abundance increases by 0.03 for every one-unit increase in years, suggesting a consistent increase in *O. orca* populations since 1970 (i.e. approximately 1.03 orca individuals per year in North America). The model also confirms the findings of the GAM, suggesting that different locations show differing *O. orca* abundances. 

For future studies, it would also be valuable to look at the global trends of *O. orca* abundance, as they are present all over the globe in every marine habitat. 



_____________
## References: 
Auguie, B. (2017). _gridExtra: Miscellaneous Functions for "Grid" Graphics_. R package version 2.3, https://CRAN.R-project.org/package=gridExtra.

Bates, D., Maechler, M., Bolker, B., Walker, S. (2015). Fitting Linear Mixed-Effects Models Using lme4. Journal of Statistical Software, 67(1), 1-48. doi:10.18637/jss.v067.i01.

Bürkner, P. C. (2017). brms: An R Package for Bayesian Multilevel Models Using Stan. Journal of Statistical Software, 80(1), 1-28. doi:10.18637/jss.v080.i01
  
Ford, J. K. B. (2009). K - Killer Whale: Orcinus orca. ScienceDirect. https://www.sciencedirect.com/science/article/pii/B9780123735539001504.

Fox, J., Weisberg, S. (2019). _An R Companion to Applied Regression_, Third edition. Sage, Thousand Oaks CA. https://socialsciences.mcmaster.ca/jfox/Books/Companion/.

Kallman, T. (n.d.). Killer Whale (Orcinus Orca). Shutterstock. https://www.shutterstock.com/image-photo/killer-whale-orcinus-orca-554899423?consentChanged=true.

Kay, M. (2023). _tidybayes: Tidy Data and Geoms for Bayesian Models_. doi:10.5281/zenodo.1308151, R package version 3.0.6, http://mjskay.github.io/tidybayes/.

Lüdecke, D. (2018). “ggeffects: Tidy Data Frames of Marginal Effects from Regression Models.” _Journal of Open Source Software_, *3*(26), 772. https://doi.org/10.21105/joss.00772.

Lüdecke, D. (2023). _sjPlot: Data Visualization for Statistics in Social Science_. R package version 2.8.15, https://CRAN.R-project.org/package=sjPlot.

Reeves, R., Ford, J., and Pitman, R. (2017). IUCN Red List of Threatened Species: Orcinus orca. IUCN Red List of Threatened Species. https://www.iucnredlist.org/species/15421/50368125#threats.

Sarkar D (2008). _Lattice: Multivariate Data Visualization with R_. Springer, New York. ISBN 978-0-387-75968-5, http://lmdvr.r-forge.r-project.org.

Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth Edition. Springer, New York. ISBN 0-387-95457-0

Wickham, H., Averick, M., Bryan, J., Chang, W., McGowan, L. D., François, R., Grolemund, G., Hayes, A., Henry, L., Hester, J., Kuhn, M., Pedersen, T. L., Miller, E., Bache, S. M., Müller, K., Ooms, J., Robinson, D., Seidel, D. P., Spinu, V., Takahashi, K., Vaughan, D., Wilke, C., Woo, K., Yutani, H. (2019). “Welcome to the tidyverse.” _Journal of Open Source Software_, *4*(43), 1686. https://doi.org/10.21105/joss.01686.

Wood, S.N. (2011) Fast stable restricted maximum likelihood and marginal likelihood estimation of semiparametric generalized linear models. Journal of the Royal Statistical Society (B) 73(1):3-36

WWF and ZSL (2023). The Living Planet Index. Livingplanetindex.org. https://www.livingplanetindex.org. 

Zeileis, A., Hothorn, T. (2002). Diagnostic Checking in Regression Relationships. R News 2(3), 7-10. https://CRAN.R-project.org/doc/Rnews/


