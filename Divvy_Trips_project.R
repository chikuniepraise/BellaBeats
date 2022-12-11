library(readr)
library(skimr)
library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)

#loading the dataset for the year, 2020

Divvy_Trips_Q1 <- read_csv("Divvy_Trips_2020_Q1.csv")
Divvy_Trips2 <- read_csv("202004-divvy-tripdata.csv")
Divvy_Trips3 <- read_csv("202005-divvy-tripdata.csv")
Divvy_Trips4 <- read_csv("202006-divvy-tripdata.csv")


## Cleaning the dataset
#Checking for null values in the dataset
is.null(Divvy_Trips_Q1)
is.null(Divvy_Trips2)
is.null(Divvy_Trips3)
is.null(Divvy_Trips4)



#checking for duplicate data
sum(duplicated(Divvy_Trips_Q1))
sum(duplicated(Divvy_Trips2))
sum(duplicated(Divvy_Trips3))
sum(duplicated(Divvy_Trips4))
#the Start_station_id and end_station_id are in decimal instead of integer

#merging all the dataset into one
Divvy_Trips <- rbind(Divvy_Trips_Q1, Divvy_Trips2, Divvy_Trips3, Divvy_Trips4)

View(Divvy_Trips)
str(Divvy_Trips)


#Checking for null values in the dataset
colSums(is.na(Divvy_Trips))

#replacing null values to N/A
Divvy_Trips$start_station_name[Divvy_Trips$start_station_name ==""]<- "N/A"

Divvy_Trips$end_station_name[Divvy_Trips$end_station_name ==""]<- "N/A"

# droping the columns: latitude , longitude , start station Id , end Station Id.
Divvy_Trips = subset(Divvy_Trips, select = -c(start_lat, start_lng, end_lat, end_lng, start_station_id , end_station_id))

#Checking for how many distict values in the column member_casual
n_distinct(Divvy_Trips$member_casual) #..this means that there are two distinct variables, member and casual
n_distinct(Divvy_Trips$ride_id) #...there are 2919113 distinct rider_ids
n_distinct(Divvy_Trips$rideable_type) # there are three distinct ride types: electric, classic and docked bike


#checking for how many causual riders and annual members on the dataset
table(Divvy_Trips['member_casual'])
Divvy_Trips%>%
  count(member_casual)


#checking for how many rideable types on the dataset
table(Divvy_Trips['rideable_type'])
Divvy_Trips%>%
  count(rideable_type)

#checking for how many members use reach rideable types on the dataset
table(Divvy_Trips['rideable_type', 'member_casual'])
Divvy_Trips%>%
  count(rideable_type, member_casual)

skim(Divvy_Trips)
#this shows that more member riders use classic bikes, docked bikes and electric bikes than casual riders.

Divvy_Trips$start_station_id <- as.integer(Divvy_Trips$start_station_id)
Divvy_Trips$end_station_id <- as.integer(Divvy_Trips$end_station_id)
skim_without_charts(Divvy_Trips)



table(Divvy_Trips$member_casual, useNA = "ifany")   #this shows that there is no null values.


#transforming the dataset
# separating the column 'started at' and 'ended at'.


Divvy_Trips$rideDate<-as.Date(Divvy_Trips$started_at)

Divvy_Trips$started_at<-as_datetime(Divvy_Trips$started_at)
Divvy_Trips$ended_at<-as_datetime(Divvy_Trips$ended_at)

Divvy_Trips$month<-format(as.Date(Divvy_Trips$rideDate),"%B")
Divvy_Trips$day <-format(as.Date(Divvy_Trips$rideDate),"%d")
Divvy_Trips$year<-format(as.Date(Divvy_Trips$rideDate),"%Y")
Divvy_Trips$day_of_week<-format(as.Date(Divvy_Trips$rideDate),"%A")

colnames(Divvy_Trips)
View(Divvy_Trips)


#length of the ride
#Divvy_Trips$length_of_ride <- difftime(Divvy_Trips$ended_at,Divvy_Trips$started_at)
Divvy_Trips <- Divvy_Trips%>%
  mutate(length_of_ride=ended_at - started_at)
head(Divvy_Trips)

#checking for hours and minutes used to complete the ride
Divvy_Trips$hour_minutes_of_ride <- hms::as_hms(Divvy_Trips$length_of_ride)
View(Divvy_Trips)

Divvy_Trips$length_of_ride <- as.numeric(Divvy_Trips$length_of_ride)
str(Divvy_Trips$length_of_ride)


##filtering the length of ride less than 0 seconds

biketrip <- filter(Divvy_Trips,length_of_ride>0)

##average, minimum and maximum length of ride

biketrip%>%
  summarise(min_length=min(length_of_ride),max_length=max(length_of_ride),average_length=mean(length_of_ride))
View(biketrip)

##length of Ride by member_type

aggregate(length_of_ride~member_casual, data= biketrip,mean)
aggregate(length_of_ride~member_casual, data= biketrip,median)
aggregate(length_of_ride~member_casual, data= biketrip,max)
#this means that casual riders have more ride lengths than member riders.

#sorting the data by weekday
biketrip$day_of_week<-ordered(biketrip$day_of_week,levels=c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
biketrip%>%count(day_of_week,member_casual)
#sorting the data by month
biketrip$month<-ordered(biketrip$month,levels=c('January', 'February', 'March', 'April', 'May', 'June','July', 'August', 'September', 'October', 'November', 'December'))
biketrip%>%count(month,member_casual)
biketrip%>%count(member_casual, rideable_type)

#to make sure that rstudio doesn't change the values when plotting the axis
#biketrip$length_of_ride <- factor(biketrip$length_of_ride, levels = rev(unique(biketrip$length_of_ride)))

##average, max and min length of ride for 6 months
mean_r_length <-as.numeric(mean(biketrip$length_of_ride))/60
cat("The average ride length over 6 months is;", mean_r_length, "minutes")



min_r_length <-as.numeric(min(biketrip$length_of_ride))/60
cat("The minimum ride length over 6 months is;", min_r_length, "minutes")

max_r_length <- as.numeric(max(biketrip$length_of_ride))/3600
cat("The maximum ride length over 6 months is;", max_r_length, "hours")
```
#visualization
#loading packages
library(corrplot)
library(ggplot2)


biketrip%>%
  group_by(member_casual,day_of_week)%>%
  summarise(total_ride_duration=mean(length_of_ride))%>%
  ggplot(mapping=aes(x=member_casual,y=total_ride_duration,fill=day_of_week)) +
  geom_bar(position="Dodge",stat = "identity") +
  facet_wrap(~day_of_week) +
  labs(title="Average ride length by day of the week")

biketrip%>%
  group_by(member_casual,month)%>%
  group_by(total_ride_duration=mean(length_of_ride))%>%
  ggplot(mapping=aes(x=member_casual,y=total_ride_duration,fill=month)) +
  geom_bar(position="Dodge",stat = "identity") +
  facet_wrap(~month) +
  labs(title="Average ride length of the month")

biketrip%>%
  group_by(member_casual,year)%>%
  summarise(Ridenumbers=n())%>%
  ggplot(mapping=aes(x=year,y=Ridenumbers,fill=member_casual)) +
  geom_bar(position="Dodge",stat = "identity") +
  labs(title="Average ride length by year")


str(biketrip$length_of_ride)
par(mfrow=c(1,1))


boxplot(length_of_ride ~ member_casual,
        data = biketrip,
        main = "distribution of length by week",
        xlab = "casual_member",
        ylab = "length of ride",
        col = c("orange", "yellow"))



boxplot(biketrip$month ~ biketrip$member_casual,
        data = biketrip,
        main = "Month Vs Riders",
        xlab = "Member Riders and Casual Riders",
        ylab = "Month",
        col = c("pink", "pink1"))
 

