##%#########################################################################%##
#                                                                             #
#                          Data science W2 (28.9.2023)                        #
#                                                                             #
##%#########################################################################%##

#WD
setwd("~/") #erases previously set WDs
setwd("Personal repo - zmancekpali/Weekly DS content/Week 2") #sets a new one
getwd() #check that it's worked


#Data visualization and manipulation ----
#packages
library(cowplot)
library(ggpubr)
library(grid)
library(jpeg) 
library(magick) 
library(rmarkdown) 
library(tidyverse) #includes ggplot and dplyr 

#data
nobel <- read.csv("nobel_prize_data.csv")
path <- "elmo.jpeg" #elmo path
elmo = image_read(path) #R reads elmo


#data exploration
dim(nobel) #dimensions of the dataset
str(nobel) #class for the whole dataset
head(nobel) #fist few lines of the dataset
tail(nobel) #last few lines of the dataset
summary(nobel)

nobel <- nobel %>% 
  mutate(pop_clean = gsub(",", "", population_2018)) %>% #replace the commas in the numbers
  mutate(pop_clean = gsub("[6]","", pop_clean, fixed=T)) %>%  #'fixed = T' gives an exact match; delete the "[6]'
  mutate(pop_clean = as.numeric(pop_clean)) %>% #changed from character to numeric
  mutate(per_capita = nobel_prizes/pop_clean * 1000000) %>% 
  mutate(continent = case_when(
    continent == "Central America" ~ "North America",
    continent == "Middle East" ~ "Asia",
    continent == "Australasia" ~ "Oceania", 
    TRUE ~ continent
  )) #this moves some of the countries into another continent (i.e. bc we had some grouped into Australasia, Middle East, ... that are not really continents)

nobel %>% filter(continent == "Africa") #filter only data for Africa

#plots
boxplot(nobel_prizes ~ continent, data = nobel)
plot(nobel_prizes ~ pop_clean, data = nobel)

#pretty graph w elmo
(p <- ggplot(nobel, aes(x=continent, y = log(per_capita), fill = continent)) +
  geom_boxplot() +
  scale_y_continuous(breaks = log(c(1,5,10,20)), #change the ticks on the y-axis
                     labels = c(1,5,10,20)) + #labels the y-axis
  theme_classic() +
  guides(fill = "none") +  #removes the legend
  labs(y="Nobel prizes per 1,000,000 people (log-scale)", x = "Continent") +
  annotate("text", x = 3, y = log(26), parse = TRUE,
           label = expression("WINNERS" ~ "!!"), color = "red") +
  annotate("text", x = 1, y = log(1), parse = TRUE,
           label = "LOSERS", color = "red"))
grid.raster(elmo, just = "top", gp = gpar(alpha = 0.5)) #this adds the elmo on top of the image

ggsave("plot2 - Helen.png", p, units = "cm", width = 20, height = 15) #saves the plot at set dimensions to your WD


#Functions----
#Simple functions
example.fn <- function(x, y){
  x + y} #this is the most basic function format
example.fn(x = 1, y = 2) #the code to run the function

nobel_per_million <- function(x) {
  x/1000000}
nobel_per_million(x = nobel$nobel_prizes)

nobel_per_capita <- function(x, y) {
  x/y}
nobel_per_capita(x = nobel$nobel_prizes, y = nobel$pop_clean)

#For loops
for(i in list){
} #this is the most basic loop


#Markdown ----
#File -> New file -> Markdown file (html)
#Look into how the graphs appear in Markdown - can/t figure it out 