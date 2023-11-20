### **Pre-registration**

Name: Zoja Manček Páli

Date: November 7th, 2023

**1. What is the data source?  Do you have permission to use the data?**

The data source is the Living Planet Index ([LPI](https://www.livingplanetindex.org)) by the WWF International and the Zoological Society of London (ZSL). The data is open source and can be used for conservation purposes, scientific analyses, or research, as per their [data agreement](https://livingplanetindex.org/documents/data_agreement.pdf). The data is used in accordance with this agreement and the [repository licence](https://github.com/EdDataScienceEES/challenge3-zmancekpali/blob/master/LICENSE) for scientific analysis without any reproduction for direct financial gain or further distribution, and will be cited appropriately. 


**2. What is the aim of this study?**

Due to the increasing speed of the effects of climate change and increasing human pressures on habitats, it is increasingly important to understand how various species are reacting to these changes over time, with particular focus on endangered and protected species. The orca (or killer whale; *Orcinus orca*) is a protected marine mammal species listed as critically endangered by the [IUCN](https://www.iucnredlist.org/species/15421/50368125#threats). The focus of this study is to evaluate the changes in population of *O. orca* over time in North America.

Understanding how *O. orca* responds to these external stressors is crucial for marine conservation efforts as this species is often considered a sentinel for the health of marine ecosystems: changes in their abundance may signal broader ecological disruptions (Ford, 2009).


**3. What's the main question being asked or hypothesis being tested in this study?**

Research question 1: How does the abundance of *O. orca* species vary over time in North American marine habitats?

- Hypothesis: We expect the abundance of *O. orca* to decrease over time due to the immense number of external stressors (including water pollution, the fishing industry, disturbances via tourism, and whaling, among others; Reeves *et al*., 2017) which may impact their natural habitat and prey availability. 

- Null hypothesis: There is no change in *O. orca* species abundances over time. 

This study focuses on *O. orca* in the US and Canada (hereafter North America), as they both provide a high number of continuous observations of *O. orca* over time within this dataset. We also want to investigate whether these two countries within the same region show different patterns in *O. orca* populations:


Research question 2: How does the abundance of *O. orca* vary over time within Canada and the US marine habitats?

- Hypothesis: We expect the *O. orca* abundances to show similar negative relationship over time in Canada and the US. 

- Null hypothesis: There is no difference in *O. orca* populations in Canada and the US.


**4. Describe the key independent and dependent variable(s).**

- Independent: Time.
- Dependent: Abundance of *O. orca* (number of individuals).
- Additional factor: Country within North America and site within there. 


**5. What are the spatial and temporal structures to the data (number of sites, duration in years, etc.)?**

The temporal structure: the dataset spans over 45 years (between 1970 and 2014; inclusive), with varying observations for each year. The dataset contains some missing values for each year in this time period, which is another reason as to why I selected the US and Canada - as they both contain the majority of the continuous abundance datapoints from the LPI dataset for *O. orca*. The missing data will not be included in the analysis. 

The spatial structure: There are 13 population locations within Canada and the US where these *O. orca* populations have been sampled/observed. 


**6. What is the overall sample size?**

The sample sizes for RQ1 and RQ2 are 45 and 13, respectively. When combined for analysis of both, the combined sample size is 585, which is calculated by summing the sample sizes for RQ1 (45) and RQ2 (13), as we aim to analyze both questions collectively.


**7. Specify exactly which analyses you will conduct to examine the main question/hypothesis.**

First, I will visualise the data using a time-series plot (a line plot of year vs abundance) and a histogram of the data distribution (as the independent variable is *O. orca* abundance, I expect the distribution to be Poisson). The appropriate model for a Poisson-distributed data is a model which can accommodate for the Poisson distribution of the data. To test my RQs, I will thus use a generalised linear mixed-effect model that incorporates both the time and the country as the independent variables and the population site and the time as the random effects: 
        ```glmer(Abundance ~ Year + Country + (1 | Year) + (1 | Site), family = poisson, data = dataset) ```

The reason I decided to include year as a random effect (and a fixed effect), is to allow the model to account for the temporal structure of the data - adding the year as a random effect into the model can account for variation within each year, not just across the years. I included Site as a random effect because I am expecting some variation in the data to come from the different sites (e.g. the habitat in the California Current region may vary from the Prince William Sound in Alaska or the Pribilof Islands in the Bering sea) - *O. orca* are mostly known to be present in coastal and temperate waters (Ford, 2009), meaning their abundance in the different regions may account for some of the variation within the data that isn't explained by the year. I included Country and as a fixed effect as they it has fewer than 5 levels (only two countries) but may still explain the variation in *O. orca* population abundances. 

I will compare this models to a null model, a model where the year is only included as a fixed effect, and a model with Country and Site are nested (if I find that is appropriate). I will compare the models via the AIC (Akaike Information Criterion) first, followed by the LRT (Likelihood Ratio Test) if necessary, to determine which one explains the trends within the data the best while taking into account the trade-off between complexity and model fit. 

I'd also like to compare these models to the Bayesian alternative and interpret them.

Regarding data visualisation, I would like to display at least one plot (time vs abundance; separated for each country); with a map of the populations as well if time allows. I will also include the model evaluation plots and values and the justification for whichever model I select for my report alongside. 


**8. Is there any other study information you would like to pre-register?**
I made the conscious choice to not filter out any of the values based on their units - when looking at the sampling units in the original dataset, they all refer to the general concept of abundance, but are named differently (e.g. "Number observed", "Estimated abundance", "Total population", "No. of Individuals", "Estimated density", etc.) - for the purposes of this study, I am just calling them all "Abundance of *O. orca*". 

**Reference list:**

Ford, J.K.B. (2009). K - Killer Whale: Orcinus orca. ScienceDirect. Available at: https://www.sciencedirect.com/science/article/pii/B9780123735539001504.

Reeves, R., Ford, J. and Pitman, R. (2017). IUCN Red List of Threatened Species: Orcinus orca. IUCN Red List of Threatened Species. Available at: https://www.iucnredlist.org/species/15421/50368125#threats.

WWF and ZSL (2023). The Living Planet Index. Livingplanetindex.org. Available at: https://www.livingplanetindex.org/#:~:text=The%20Living%20Planet%20Index%20(LPI.
