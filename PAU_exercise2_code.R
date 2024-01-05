#=========================================================================
# PAU Data-related Exercises - Exercise 2
# Hannah Marsho
#=========================================================================

# install.packages("dplyr") <- run, if necessary
# install.packages("lubridate") <- run, if necessary
# install.packages("ggplot2") <- run, if necessary
library(dplyr)
library(lubridate)
library(stats)
library(ggplot2)

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

refined_frisked_12 <- refined %>% filter(districtoccur == 12) %>% group_by(race) %>% summarise(frisk_rate = round(sum(vehicle_frisked) / n() * 100, 2)) # create table

write.csv(refined_frisked_12, "~/Documents/GitHub/PAU/refined_frisked_12.csv", row.names = TRUE) # export table

#=========================================================================
# Question 8
#=========================================================================

# Are there any observable differences in vehicle stops in District 12 versus District 24? 
# What might account for such differences, and how would you go about testing your hypotheses?

refined_frisked_24 <- refined %>% filter(districtoccur == 24) %>% group_by(race) %>% summarise(frisk_rate = round(sum(vehicle_frisked) / n() * 100, 2)) # create table
write.csv(refined_frisked_24, "~/Documents/GitHub/PAU/refined_frisked_24.csv", row.names = TRUE) # export table
refined_frisked_district <- merge(refined_frisked_12, refined_frisked_24, by = 'race') %>% setNames(c('race', '12', '24')) # compare racial frisk rates across Districts 12 & 24

refined <- refined %>% mutate(month = floor_date(datetimeoccur, "month")) 
district_month <- table(refined$districtoccur, refined$month)
write.csv(district_month, "~/Documents/GitHub/PAU/district_month.csv", row.names = TRUE) # export table
print(chisq.test(district_month)) # p-value = 0.0003325

district_weekday <- table(refined$districtoccur, refined$weekday)
write.csv(district_weekday, "~/Documents/GitHub/PAU/district_weekday.csv", row.names = TRUE) # export table
print(chisq.test(district_weekday)) # p-value < 2.2e-16

district_gender <- table(refined$districtoccur, refined$gender)
write.csv(district_gender, "~/Documents/GitHub/PAU/district_gender.csv", row.names = TRUE) # export table
print(chisq.test(district_gender)) # p-value = 0.1665

district_race <- table(refined$districtoccur, refined$race)
write.csv(district_race, "~/Documents/GitHub/PAU/district_race.csv", row.names = TRUE) # export table
print(chisq.test(district_race)) # p-value < 2.2e-16

district_searched <- table(refined$districtoccur,refined$vehicle_searched)
write.csv(district_searched, "~/Documents/GitHub/PAU/district_searched.csv", row.names = TRUE) # export table
print(chisq.test(district_searched)) # p-value < 2.2e-16

district_frisked <- table(refined$districtoccur,refined$vehicle_frisked)
write.csv(district_frisked, "~/Documents/GitHub/PAU/district_frisked.csv", row.names = TRUE) # export table
print(chisq.test(district_frisked)) # p-value = 4.614e-07

district_contraband <- table(refined$districtoccur,refined$vehicle_contraband) 
write.csv(district_contraband, "~/Documents/GitHub/PAU/district_contraband.csv", row.names = TRUE) # export table
print(chisq.test(district_contraband)) # p-value = 6.218e-12

#=========================================================================
# Question 9
#=========================================================================

# After you have completed answering these questions, please produce graph(s) that 
# you find interesting. These could be vehicle stops over time, a breakdown of 
# vehicle stops by race/ethnicity, etc.

png("breakdown_race.png", width = 15, height = 10, units = "in", res = 100)
breakdown_race <- pie(refined_race_ethn$count, labels = c("American Indian", "Asian", "Black - Latino", "Black - Non-Latino", "Unknown", "White - Latino", "White - Non-Latino"), main  = "Vehicle Stops by Race/Ethnicity")
dev.off()

refined_gender <- refined %>% filter(gender == "Male" | gender == "Female") %>% group_by(gender) %>% summarise(count = n()) # create table
png("breakdown_gender.png", width = 15, height = 10, units = "in", res = 100)
breakdown_gender <- pie(refined_gender$count, labels = c("Female", "Male"), main  = "Vehicle Stops by Gender")
dev.off()

refined_age <- refined %>% filter(age != 'NA') %>% mutate(age_group = case_when(age <= 18 ~ 'Minor (<=18)',
                                                        age < 30 ~ '19-29',
                                                        age < 40 ~ '30s',
                                                        age < 50 ~ '40s',
                                                        age < 60 ~ '50s and Early 60s',
                                                        age < 120 ~ 'Senior (>=65)'))
refined_age_chart <- refined_age %>% group_by(age_group) %>% summarise(count = n()) # create table
png("breakdown_age.png", width = 15, height = 10, units = "in", res = 100)
breakdown_age <- pie(refined_age_chart$count, labels = c('19-29', '30s', '40s', '50s and Early 60s', 'Minor (<=18)', 'Senior (>=65)'), main  = "Vehicle Stops by Age Group")
dev.off()

refined_ts <- refined %>% mutate(date = substr(datetimeoccur, 1, 10)) %>% group_by(date) %>% summarise(count = n())
breakdown_ts <- ggplot(refined_ts, aes(x = date, y = count, group = 1)) +
    geom_point() +
    geom_line() +
    ylim(0, 300) +
    theme_minimal() +
    theme(axis.text.x=element_blank()) +
    labs(title = "Vehicle Stops Over Time",
          x ="Date", y = "Stops") +
    theme(plot.title = element_text(hjust = 0.5))
png("breakdown_ts.png")
print(breakdown_ts)
dev.off()
