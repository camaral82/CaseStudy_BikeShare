# 10/08/2021
# Google data Analytics Capstone

#Bike-Share Case Study

setwd("~/0. Google Data Analytics Certificate/8. Capstone")

library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)

#### LOADING
tripdata_raw <- read.csv("~/0. Google Data Analytics Certificate/8. Capstone/Trip_Data/202007-202106-divvy-tripdata.txt")
head(tripdata_raw)
dim(tripdata_raw)
glimpse(tripdata_raw)


#create tripdata_1 as a sample of tripdata_raw with 10% of its size 
'Random Sampling
Population Size: 4.460.151
Confidence Level: 99%
Margim of Error: 2%
Sample Size: 4.143'

sample_size = round(dim(tripdata_raw)[1]*0.05,0) #sample size of 5% = 223.008

set.seed(100)
row_number = sample(1:nrow(tripdata_raw), sample_size)
tripdata_1 = tripdata_raw[row_number,]




### PROCESS
# member_casual and rideable_type to Factor
tripdata_1$member_casual <- as.factor(tripdata_1$member_casual)
tripdata_1$rideable_type <- as.factor(tripdata_1$rideable_type)
str(tripdata_1)

summary(tripdata_1)

#start_at and ended_at as date time
tripdata_1$started_at <- ymd_hms(tripdata_1$started_at)
tripdata_1$ended_at <- ymd_hms(tripdata_1$ended_at)
str(tripdata_1)


#2. Create ride_length and day_of_week
#Calculate the length of each ride by subtracting the column "started_at" from the column "ended_at"
#and format as HH:MM:SS
'RIDE_LENGTH'
tripdata_1 <- tripdata_1 %>% mutate(ride_length = ended_at - started_at)

'DAY_OF_WEEK'
days <- c("domenica", "lunedì", "martedì", "mercoledì", "giovedì", "venerdì", "sabato")
tripdata_1 <- tripdata_1 %>% 
  mutate(day_of_week = as.factor(weekdays(as.Date(started_at))))

tripdata_1$day_of_week = factor(tripdata_1$day_of_week, 
                                   levels = days,
                                   order=TRUE)

summary(tripdata_1$day_of_week)
str(tripdata_1$day_of_week)


tripdata_1$ride_length_min <- as.numeric(round((tripdata_1$ride_length)/60))

str(tripdata_1)
colnames(tripdata_1)

tripdata_1 <- tripdata_1 %>% select(-c(ride_id, start_lat, start_lng, 
                                       end_lat, end_lng,
                                       start_station_name, start_station_id,
                                       end_station_name, end_station_id))


"DATA CLEANING"
summary(tripdata_1)
head(tripdata_1)

# NA VALUES - no
sum(is.na(tripdata_1$ride_length))

#NEGATIVE VALUES - yes
sum(tripdata_1$ride_length_min < 0)

#get rid of ride_length's  negative values
tripdata_1 <- tripdata_1 %>% filter(ride_length_min >= 0)

#removing outliers
'Outlier: An outlier in a distribution is a number that is more than 1.5 times 
the length of the box away from either the lower or upper quartiles.
Speci???cally, if a number is less than Q1 - 1.5×IQR or
greater than Q3 + 1.5×IQR, then it is an outlier.'
summary(tripdata_1$ride_length_min)
quantile(tripdata_1$ride_length_min)
IQR = IQR(tripdata_1$ride_length_min)
Q1 = 8
Q3 = 25


upper_outlier = Q3 + 1.5*IQR
lower_outlier = Q1 - 1.5*IQR

sum(tripdata_1$ride_length_min > upper_outlier)

tripdata_1 <- tripdata_1 %>% filter(ride_length_min < upper_outlier)
tripdata_1 <- tripdata_1 %>% filter(ride_length_min > lower_outlier)

hist(tripdata_1$ride_length_min)


######## ANALYZE ########
table(tripdata_1$rideable_type)
table(tripdata_1$member_casual)
table(tripdata_1$day_of_week)

x <- prop.table(table(tripdata_1$member_casual, tripdata_1$day_of_week))*100
x[1, 7]


prop.table(table(tripdata_1$member_casual, tripdata_1$rideable_type))*100

"Average Ride Length Minutes for Member Casual"
tripdata_1 %>% group_by(member_casual) %>% 
  summarize(average = mean(ride_length_min),
            mediana = median(ride_length_min))


### DATA VISUALIZATION
#bar graph of member_casual
ggplot(data=tripdata_1) + geom_bar(mapping = aes(x = member_casual, fill = member_casual))


#bar graph of rideable type
ggplot(data=tripdata_1) + 
  geom_bar(mapping = aes(x = rideable_type, fill = rideable_type))+
  facet_wrap(~member_casual)


ggplot(data=tripdata_1) + 
  geom_bar(mapping = aes(x = day_of_week, fill = day_of_week))+
  facet_wrap(~member_casual)
















