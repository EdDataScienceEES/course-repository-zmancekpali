##%#########################################################################%##
#                                                                             #
#                          Data science W3 (5.10.2023)                        #
#                                  Readings                                   #
#                                                                             #
##%#########################################################################%##

#R for Data Science Chapter 5 Data transformation (http://r4ds.had.co.nz/transform.html)
#Libraries
library(nycflights13)
library(tidyverse)

#Data
flights

#Filter() ----
jan1 <- filter(flights, month == 1, day == 1)
(dec25 <- filter(flights, month == 12, day == 25)) #this saves the code and prints the results

filter(flights, month == 11 | month == 12) #November and December
nov_dec <- filter(flights, month %in% c(11, 12)) #this does the same as above

filter(flights, !(arr_delay > 120 | dep_delay > 120)) #flights not delayed by more than 2 hours

#Filter() exercises
two_hour_delay <- filter(flights, (arr_delay > 120)) #Flights that had an arrival delay of two or more hours

houston <- filter(flights, dest == c("IAH", "HOU")) #Flights that flew to Houston (IAH or HOU)

operated <- filter(flights, carrier %in% c("UA", "AA", "DL")) #Flights that were operated by United, American, or Delta

summer <- filter(flights, month %in% c("6", "7", "8")) #Flights that departed in the summer

delayed <- filter(flights, !(arr_delay > 120 | dep_delay == 0)) #Flights that arrived more than two hours late, but didn’t leave late

quick_flyers <- filter(flights, !(dep_delay >= 60 | (air_time - 30) > 0)) #Flights that were delayed by at least an hour, but made up over 30 minutes in flight

late_night_flights <- filter(flights, ) #Flights that departed between midnight and 6am (inclusive)


#Arrange() ----
arrange(flights, year, day, month) #changes the order of the columns
arrange(flights, desc(dep_delay)) #re-orders a column in descending order

#Arrange() exercises
df <- tibble(x = c(5, 2, NA))
df_sorted <- df %>%
  arrange(desc(is.na(x))) #arranges the NA values first (normally it arranges them last)

arrange(flights, desc(dep_delay)) #arranges from largest departure delay to smallest
arrange(flights, dep_time) #arranges from earliest departure time to latest
flight_time <- arrange(flights, air_time) #arranges from shortest air time to longest
longest <- arrange(flights, desc(distance)) #arranges from largest distance to lowest

#Select() ----
select(flights, year, month, day) #selects only these columns from the flights data set
select(flights, year:day) #selects only the columns between year and day (inclusive)
select(flights, -(year:day)) #selects all columns except those between year and day (inclusive)
select(flights, time_hour, air_time, everything()) #selects the named columns and presents them first; the everything() will keep the other columsn after the selected few

renamed <- rename(flights, tail_num = tailnum) #renames the tailnum column into tail_num

#Select() exercises
select(flights, dep_time, dep_delay, arr_time, arr_delay)
select(flights, dep_time:arr_delay, -c(sched_dep_time, sched_arr_time))
select(flights, c(4, 6, 7, 9))
select(flights, matches("^(dep_time|dep_delay|arr_time|arr_delay)$")) #all four of these do the same

select(flights, dep_time, dep_delay, dep_time) #nothing happens if you include a column name twice

vars <- c("year", "month", "day", "dep_delay", "arr_delay")
selected_columns <- select(flights, any_of(vars)) #selects the columns from flights that are the same as the vars

select(flights, contains("TIME")) #selects for columns with 'time' regardless of case
select(flights, matches("TIME", ignore.case = FALSE)) #takes case into account

#Mutate() ----

