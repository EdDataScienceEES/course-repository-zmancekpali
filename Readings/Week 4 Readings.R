##%#########################################################################%##
#                                                                             #
#                          Data science W4 (12.10.2023)                       #
#                                  Readings                                   #
#                                                                             #
##%#########################################################################%##


#R for Data Science Chapter 5 Data transformation (http://r4ds.had.co.nz/transform.html)
#Libraries
library(nycflights13)
library(tidyverse)

#Data
flights

#Wrangling
jan1 <- filter(flights, month == 1, day == 1)
(dec25 <- filter(flights, month == 12, day == 25)) #this saves the code and prints the results

filter(flights, month == 11 | month == 12) #November and December
nov_dec <- filter(flights, month %in% c(11, 12)) #this does the same as above

filter(flights, !(arr_delay > 120 | dep_delay > 120)) #flights not delayed by more than 2 hours

#Exercises
#Flights that had an arrival delay of two or more hours
two_hour_delay <- filter(flights, (arr_delay > 120)) 

#Flights that flew to Houston (IAH or HOU)
houston <- filter(flights, dest == c("IAH", "HOU"))

#Flights that were operated by United, American, or Delta
operated <- filter(flights, carrier %in% c("UA", "AA", "DL"))

#Flights that departed in the summer
summer <- filter(flights, month %in% c("6", "7", "8"))

#Flights that arrived more than two hours late, but didn’t leave late
delayed <- filter(flights, !(arr_delay > 120 | dep_delay == 0)) #flights not delayed by more than 2 hours

#Flights that were delayed by at least an hour, but made up over 30 minutes in flight
quick_flyers <- filter(flights, !(dep_delay >= 60 | (air_time - 30) > 0))

#Flights that departed between midnight and 6am (inclusive)
late_night_flights <- filter(flights, )

#R for Data Science Part II Wrangle Chapters 9 - 16 (http://r4ds.had.co.nz/wrangle-intro.html)

