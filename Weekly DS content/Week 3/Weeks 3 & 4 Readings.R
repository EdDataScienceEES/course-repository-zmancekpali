##%#########################################################################%##
#                                                                             #
#                          Data science W3 & 4 (5.10.2023)                    #
#                                  Readings                                   #
#                                                                             #
##%#########################################################################%##

#R for Data Science Chapter 5 Data Transformation (http://r4ds.had.co.nz/transform.html)
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
flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time)
mutate(flights_sml,
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60,
       gains_per_hous = gain / hours) #creates these new columns into the flights_sml data set

transmute(flights,
          gain = dep_delay - arr_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours) #this does the same as the above except it only shows these new colums







#Summarise() ----
summarise(flights, delay = mean(dep_delay, na.rm = TRUE)) #gives the mean delay value
by_day <- group_by(flights, year, month, day) #group the flights by year, month, and day
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE)) #give them mean for each year, month, and day

by_dest <- group_by(flights, dest) #group by destination
delay <- summarise(by_dest,
                   count = n(), #count the number of flights to that destination
                   dist = mean(distance, na.rm = TRUE), #give the mean distance 
                   delay = mean(arr_delay, na.rm = TRUE)) #give the mean arrival delay
delay <- filter(delay, count > 20, dest != "HNL") #filter out flights that occur more than >20 but not to HNL

#Useful summarise values
flights_summarised <- summarise(flights,
          mean_delay = mean(dep_delay, na.rm = TRUE), #mean
          sd_delay = sd(dep_delay, na.rm = TRUE), #standard deviation
          min_delay = min(dep_delay, na.rm = TRUE), #minimum
          max_delay = max(dep_delay, na.rm = TRUE), #maximum
          carriers = n_distinct(carrier), #gives unique values
          first = first(dep_delay), #first non-NA value
          last = last(dep_delay)) #last value
 
#Piping ----
not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n())

ggplot(data = delays, mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)  #flights vs average delay

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(n_early = sum(dep_time < 500)) #how many flights left before 5am

daily <- group_by(flights, year, month, day) #each new grouping narrows down the dataset further
(per_day   <- summarise(daily, flights = n()))
(per_month <- summarise(per_day, flights = sum(flights)))
(per_year  <- summarise(per_month, flights = sum(flights)))

daily %>% 
  ungroup() %>%           
  summarise(flights = n())



#R for Data Science Chapters 9 to 16 Data Wrangling (https://r4ds.had.co.nz/wrangle-intro.html)
#Libraries
library(lubridate)
library(nycflights13)
library(readr)
library(readxl)
library(tidyverse)

#Tibbles and tribbles ----
#Tibbles only show the first 10 rows
as_tibble(iris)

tribble(
  ~x, ~y, ~z,
  #--|--|---
  "a", 2, 3.6,
  "b", 1, 8.5) #this lets you specify the columns

tibble(
  a = lubridate::now() + runif(1e3) * 86400, #today's date + a random number of seconds (between 0 and 86400)
  b = lubridate::today() + runif(1e3) * 30, #today's date + a random number of days (between 0 and 30)
  c = 1:1e3, #sequence of integers from 1 to 1000
  d = runif(1e3), #generates 1000 random values between 0 and 1
  e = sample(letters, 1e3, replace = TRUE)) #generates 1000 random letters (from a to z)

nycflights13::flights %>% 
  print(n = 10, width = Inf) #n = 10 and width = inf will show 10 rows for all the columns


#Data import ----
read.csv() #for csv files; base R
read_csv() #from the readr package
read.excel() #from the readxly package
load() #for R data

#Parse
parse_number("$100")
parse_number("20%")
parse_number("It cost $123.45") #all three of these will return numbers

parse_datetime("2010-10-01T2010")
parse_datetime("20101010") #if no time is given, it will be at midnight
parse_date("01/02/15", "%m/%d/%y") #month, day, year
parse_date("01/02/15", "%d/%m/%y") #day, month, year
parse_date("01/02/15", "%y/%m/%d") #year, month, day
parse_date("1 janvier 2015", "%d %B %Y", locale = locale("fr")) #gives date from the french text

fruit <- c("apple", "banana")
parse_factor(c("apple", "banana", "banana"), levels = fruit) #makes the apple and banana as factors

x1 <- "El Ni\xf1o was particularly bad this year"
x2 <- "\x82\xb1\x82\xf1\x82\xc9\x82\xbf\x82\xcd"
parse_character(x1, locale = locale(encoding = "Latin1")) #writes out the characters in Latin alphabet
parse_character(x2, locale = locale(encoding = "Shift-JIS")) #writes out the text in Japanese alphabet

guess_parser("2010-10-01") #date
guess_parser("15:01") #time
guess_parser(c("TRUE", "FALSE")) #logical
guess_parser(c("1", "5", "9")) #double
guess_parser(c("12,352,561")) #number
str(parse_guess("2010-10-10")) #date + format: 2010 - 10 - 10

#Tidying data ----
#Variables are columns, observations are columns, variables is each row:column 

table1 %>% 
  mutate(rate = cases / population * 10000) #calculates rate per 10,000 people

table1 %>% 
  count(year, wt = cases) #cases per year

tidy4a <- table4a %>% pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "cases") #change to longer 
tidy4b <- table4b %>% pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "population") #this type is more useful for what I will do
left_join(tidy4a, tidy4b) #joins them both

table2 %>% pivot_wider(names_from = type, values_from = count) #changes to a wider dataset

table3 %>% separate(rate, into = c("cases", "population")) #separate the rate into cases and total population

table3 %>% separate(year, into = c("century", "year"), sep = 2) #separate two digits into each column

table5 %>% unite(new, century, year) #joins century and year with a _ as a separator
table5 %>% unite(new, century, year, sep = "") #same as above but no _

#Missing values
stocks <- tibble(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(   1,    2,    3,    4,    2,    3,    4),
  return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66))

stocks %>% pivot_wider(names_from = year, values_from = return)
stocks %>% pivot_wider(names_from = year, values_from = return) %>% 
           pivot_longer(cols = c(`2015`, `2016`), 
                        names_to = "year", 
                        values_to = "return", 
                        values_drop_na = TRUE) #excludes the NAs
stocks %>% complete(year, qtr)

treatment <- tribble(
  ~ person,           ~ treatment, ~response,
  "Derrick Whitmore", 1,           7,
  NA,                 2,           10,
  NA,                 3,           9,
  "Katherine Burke",  1,           4)
treatment %>% fill(person) #this will replace the NAs with the most recent non-NA

#I used this for the coding club week 5 to find what species are the same in both datasets:
common_countries <- intersect(unique(ruff$Country.list), unique(razorbill$Country.list))
#Obviously, adjust the data and column names as necessary

#Factors ----
gss_cat
gss_cat %>%
  count(race)

relig_summary <- gss_cat %>%
  group_by(relig) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n())

ggplot(relig_summary, aes(tvhours, relig)) + 
  geom_point()

ggplot(relig_summary, aes(tvhours, fct_reorder(relig, tvhours))) +
  geom_point() #reordered factors

relig_summary %>%
  mutate(relig = fct_reorder(relig, tvhours)) %>%
  ggplot(aes(tvhours, relig)) +
  geom_point() #same as above but recodes within the data sets (mutate()) - useful if you have more variables

by_age <- gss_cat %>%
  filter(!is.na(age)) %>%
  count(age, marital) %>%
  group_by(age) %>%
  mutate(prop = n / sum(n))

ggplot(by_age, aes(age, prop, colour = marital)) +
  geom_line(na.rm = TRUE)

ggplot(by_age, aes(age, prop, colour = fct_reorder2(marital, age, prop))) +
  geom_line() +
  labs(colour = "marital")

#Dates and times ----
today() #gives today's date
now() #gives the date and time currently

ymd("2017-01-31")
mdy("January 31st, 2017")
dmy("31-Jan-2017")
ymd(20170131) #all of these will transform these strings or numbers into the date (y,m,d format)

ymd_hms("2017-01-31 20:11:59") #gives date and time (y,m,d and h,m,s format)
mdy_hm("01/31/2017 08:01") #gives date and time (y,m,d and h,m, format)

flights %>% select(year, month, day, hour, minute) %>% 
  mutate(departure = make_datetime(year, month, day, hour, minute))

make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
} #function to divide the time values by 100

flights_dt <- flights %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))

h_age <- today() - ymd(19791014) #so cool wow
as.duration(h_age)
