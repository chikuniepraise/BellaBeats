#BellaBeat Wellness Technology
## Preparing the data
library('tidyverse')
library('tidyr')
library('dplyr')

dailyActivity <- read.csv('dailyActivity_merged.csv')
dailyCalories <- read.csv('dailyCalories_merged.csv')
dailyIntensities <- read.csv('dailyIntensities_merged.csv')
heartrate <- read.csv('heartrate_seconds_merged.csv')
dailySteps <- read.csv('dailySteps_merged.csv')
sleepDay <- read.csv('sleepDay_merged.csv')
weight <- read.csv('weightLogInfo_merged.csv')


#Different Time Frame
hourly_calories <- read.csv("hourlyCalories_merged.csv")
hourly_intensities <- read.csv("hourlyIntensities_merged.csv")
hourly_steps <- read.csv("hourlySteps_merged.csv")
daily_sleep <- read.csv("sleepDay_merged.csv")


# Preview the dataset using the structure function

str(dailyActivity)
str(dailyIntensities)
str(dailySteps)
str(dailyCalories)
str(sleepDay)

str(hourly_calories)
str(hourly_intensities)
str(daily_sleep)
str(hourly_steps)

#Varaibles Contained in Daily_Step, Daily_Intensities, Daily_Calories is present in Daily Activities
# To Avoid duplicate data We are dropping the 3 Dataframes 
rm(dailyIntensities,dailyCalories,dailySteps)


# cleaning the data
#Checking For NA's Value #checking for nulls
#is.null() or colSums(is.na())
colSums(is.na(dailyActivity))
colSums(is.na(daily_sleep))
colSums(is.na(hourly_calories))
colSums(is.na(hourly_steps))
colSums(is.na(sleepDay))
colSums(is.na(weight))
colSums(is.na(heartrate))

#### Fat in weight Contains 65 Na's/Null Values 
#### The Information aboutbFat isnt Neccesary as most of the value are NA's 
#### 2 Values are only present out of 67values
#### So We remove it from the column

rm(weight) #  Removing Table Weight ----Successful 


### The dates are in character 'chr', instead of a date, to change this to a date format, we install and load the lubridate package. The lubridate package helps change characters to date formats.

#install.packages('lubridate')
library('lubridate')

#Working on the Dates
dailyActivity$ActivityDate<-as.Date(dailyActivity$ActivityDate
                                    , format = "%m/%d/%y")
sleepDay$SleepDay<-as.Date(sleepDay$SleepDay
                           , format = "%m/%d/%y")
sleepDay$SleepDay<-as.Date(sleepDay$SleepDay
                           , format = "%m/%d/%y")
daily_sleep$SleepDay<-as.Date(sleepDay$SleepDay
                              , format = "%m/%d/%y")
str(daily_sleep)


#### Checking for duplicate ids

unique(dailyActivity$Id)
n_distinct(dailyActivity$Id)

unique(daily_sleep$Id)
n_distinct(daily_sleep$Id)

unique(hourly_calories$Id)
n_distinct(hourly_calories$Id)

unique(hourly_intensities$Id)
n_distinct(hourly_intensities$Id)

unique(hourly_steps$Id)
n_distinct(hourly_steps$Id)


#removing duplicate data.

sum(duplicated(dailyActivity))
sum(duplicated(daily_sleep))
sum(duplicated(hourly_calories))
sum(duplicated(hourly_intensities))
sum(duplicated(hourly_steps))

#Removing the duplicated row in daily_sleep
sleep_daily<- daily_sleep[!duplicated(daily_sleep), ]
sum(duplicated(sleep_daily))
str(sleep_daily)

#converting time frame from character to date 
hourly_calories$ActivityHour <- mdy_hms(hourly_calories$ActivityHour)
hourly_intensities$ActivityHour <-mdy_hms(hourly_intensities$ActivityHour)
hourly_steps$ActivityHour <-mdy_hms(hourly_steps$ActivityHour)

# Analyzing the Dataset.
#renaming dataset
sleep_daily<- sleep_daily %>%
  rename(ActivityDate = SleepDay)
str(dailyActivity)
str(sleep_daily)

## Merging the datasets "dailyActivity" and "sleep_daily" ensures that the dataset is organized.

# the merged data would be named 'sleep_n_daily_activity'.
sleep_n_daily_activity <- merge(dailyActivity, sleep_daily, by=c("Id", "ActivityDate"))
str(sleep_n_daily_activity)




#plot visuals. 
library("ggplot2")
library("corrplot")

# Plotting the relationship between Total steps and calories burned.
ggplot(sleep_n_daily_activity) +
  geom_point(mapping = aes(x=TotalSteps, y=Calories, color = ActivityDate)) +
  labs(title = "Relationship between TotalSteps and Calories")

## Analyzing the correlation between Calories burned and Daily Activity Level.
#View the data set

glimpse(dailyActivity)
View(dailyActivity)

#Plot a density graph showing the relationship in density of calories burned by users.

ggplot(data = dailyActivity, mapping = aes(x=Calories, fill = VeryActiveMinutes, color = "VeryActiveMinutes" )) +
  geom_density(bw = 2)


#Plot the correlation graph

corrplot(corr = cor(dailyActivity[11:15]), order = "AOE",
         method = "circle",
         type = "upper", tl.pos = "lt")

corrplot(corr = cor(dailyActivity[11:15]), add= TRUE, order = "AOE",
         method = "pie", diag = FALSE,
         type = "lower", tl.pos = "n", cl.pos = "n")
