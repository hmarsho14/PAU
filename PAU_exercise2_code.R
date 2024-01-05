#=========================================================================
# PAU Data-related Exercises - Exercise 2
# Hannah Marsho
#=========================================================================

# install.packages("dplyr") <- run, if necessary
# install.packages("lubridate") <- run, if necessary
library(dplyr)
library(lubridate)
library(stats)

setwd("~/Documents/GitHub/PAU")

#=========================================================================
# Import, refine, and export CSV file
#=========================================================================

investigation_data_raw <- read.csv('car_ped_stops.csv') # import 2019 Vehicle & Pedestrian Investigations CSV file 
                                                        # from https://opendataphilly.org/datasets/vehicle-pedestrian-investigations/

investigation_data_raw$datetimeoccur <- ymd_hms(investigation_data_raw$datetimeoccur) # reformat datetimeoccur variable
                                                    
refined <- investigation_data_raw %>% # refine data set
                                  filter(stoptype == 'vehicle') %>% # for vehicle stops
                                  filter(between(datetimeoccur, as.Date("2019-01-01"), as.Date("2019-03-31"))) %>% # from January 1, 2019 to March 31, 2019
                                  filter(districtoccur == 12 | districtoccur == 24) # in districts 12 and 24

write.csv(refined, "~/Documents/GitHub/PAU/refined_data.csv", row.names = FALSE) # export refined dataset

#=========================================================================
# Question 1
#=========================================================================

# What is the aggregate number of vehicle stops conducted by the PPD?

num_stops <- nrow(refined) # 13654

#=========================================================================
# Question 2
#=========================================================================

# What is the breakdown of aggregate vehicle stops by race/ethnicity by percentage and count? 

refined_race_ethn <- refined %>% group_by(race) %>% summarise(count = n()) %>% mutate(percentage = round(count / num_stops * 100, 2)) # create table

write.csv(refined_race_ethn, "~/Documents/GitHub/PAU/refined_race_eth.csv", row.names = FALSE) # export table

#=========================================================================
# Question 3
#=========================================================================

# From the aggregate data, in which month did the PPD conduct the most vehicle stops? 

refined_month <- refined %>% group_by(month = floor_date(datetimeoccur, "month")) %>% summarise(count = n()) %>% replace(1, c('January', 'February', 'March')) # create table

write.csv(refined_month, "~/Documents/GitHub/PAU/refined_month.csv", row.names = FALSE) # export table

#=========================================================================
# Question 4
#=========================================================================

# From the aggregate data, on which day of the week were vehicles most likely to be stopped?

refined_weekday <- refined %>% group_by(weekday) %>% summarise(count = n()) # create table

write.csv(refined_weekday, "~/Documents/GitHub/PAU/refined_weekday.csv", row.names = FALSE) # export table

#=========================================================================
# Question 5
#=========================================================================

# Were more motorists stopped in District 12 or District 24? 

refined_district <- refined %>% group_by(districtoccur) %>% summarise(count = n()) # create table

write.csv(refined_district, "~/Documents/GitHub/PAU/refined_district.csv", row.names = FALSE) # export table

#=========================================================================
# Question 6
#=========================================================================

# What is the breakdown of vehicle stops by race/ethnicity in District 12 compared to District 24?

refined_race_district <- table(refined$race, refined$districtoccur) # create two-way table

write.csv(refined_race_district, "~/Documents/GitHub/PAU/refined_race_district.csv", row.names = TRUE) # export table

#=========================================================================
# Question 7
#=========================================================================

# Calculate the frisk rate in District 12 for each racial/ethnic identity.

refined_frisked <- refined %>% filter(districtoccur == 12) %>% group_by(race) %>% summarise(frisk_rate = round(sum(vehicle_frisked) / n() * 100, 2)) # create table

write.csv(refined_frisked, "~/Documents/GitHub/PAU/refined_frisked.csv", row.names = TRUE) # export table

#=========================================================================
# Question 8
#=========================================================================

# Are there any observable differences in vehicle stops in District 12 versus District 24? 
# What might account for such differences, and how would you go about testing your hypotheses?

chisq.test()

