
install.packages('tidyverse')
install.packages('ggplot2')
install.packages('dplyr')
install.packages('lubridate')
install.packages('tidyr')
install.packages('corrplot')
install.packages('janitor')

library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(corrplot)
library(janitor)


daily_activity <- read.csv('C:\\Users\\Owner\\OneDrive - Sify Technologies Limited\\Desktop\\Dataset\\Fitabase Data 4.12.16-5.12.16\\dailyActivity_merged.csv')
View(daily_activity)

daily_sleep = read.csv('C:\\Users\\Owner\\OneDrive - Sify Technologies Limited\\Desktop\\Dataset\\Fitabase Data 4.12.16-5.12.16\\sleepDay_merged.csv')

n_distinct(daily_activity$Id)
n_distinct(daily_sleep$Id)

sum(is.na(daily_activity))
sum(is.na(daily_sleep))

sum(duplicated(daily_activity))
sum(duplicated(daily_sleep))

str(daily_activity)
glimpse(daily_activity)
glimpse(daily_sleep)


daily_activity$Date <- mdy(daily_activity$ActivityDate)
daily_sleep$Date <- mdy_hms(daily_sleep$SleepDay)

daily_activity <- daily_activity %>% mutate(week_days = wday(Date, label = T))

daily_activity = daily_activity %>%
   mutate(Active_distance = case_when(TotalSteps < 5000 ~ 'Sedentary',
                                   TotalSteps < 7499 ~ 'Less Active',
                                   TotalSteps < 9999 ~ 'Moderately Active',
                                   TotalSteps <12500 ~ 'Active',
                                   TotalSteps >= 12500 ~ 'Highly Active'))

daily_sleep = daily_sleep %>%
  mutate(sleep_time = case_when(TotalMinutesAsleep < 420 ~ 'Not Enough',
                                TotalMinutesAsleep < 540 ~  'Good Sleep',
                                TotalMinutesAsleep > 540 ~ 'Over Sleep'))

daily_sleep <- daily_sleep %>% select(Id,Date,TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>% 
  mutate(time_in_bed = (TotalTimeInBed-TotalMinutesAsleep))

final_data = merge(daily_activity, daily_sleep, by = c('Id', 'Date'))

data_corr = cor(final_data[, c(4, 5, 12, 13, 14, 15, 16,20)])
corrplot(data_corr, method = 'color')
corrplot(data_corr, method = 'number')


ggplot(daily_activity,aes(x=week_days,y=SedentaryMinutes, fill = week_days))+
  geom_boxplot() + labs(title = 'Sedentary Level Distribution', x='Day of the week',y = 'Sedentary (mins)')

daily_activity <-daily_activity %>% mutate(Total_activity_mins = VeryActiveMinutes+FairlyActiveMinutes+LightlyActiveMinutes )

ggplot(daily_activity, aes(x = week_days, y = Total_activity_mins, fill = week_days)) +
  geom_boxplot()+ labs(title = 'Active Minutes', x = 'Days of the Week', y = 'Total Active Minutes')

ggplot(daily_activity, aes(x = week_days, y = TotalSteps, fill = week_days)) +
  geom_boxplot()+ labs(title = 'Active Distance', x = 'Days of the Week', y = 'Total Active Steps')

ggplot(final_data, aes(x = week_days, y = TotalMinutesAsleep, fill = week_days)) +
  geom_boxplot()+ labs(title = 'Sleep Distribution for Week', x = 'Days of the Week', y = 'Total Sleep Time')

grouptime <- final_data %>% 
  group_by(Date) %>%
  summarise(mean_total = mean(TotalDistance))

ggplot(data = grouptime, aes(x=Date, y=mean_total))+geom_line(stat = "identity", color="red")+ theme(axis.text.x = element_text(angle=90))+labs(title="Average Total Distance VS Date")  


ggplot(daily_activity, aes(x = TotalSteps, y = Calories))+
  geom_point()+
  geom_smooth(method = lm, formula = y~x)+
  labs(title='Daily Calories vs Total Steps')

ggplot(daily_activity, aes(x = VeryActiveMinutes, y = Calories))+
  geom_point()+
  geom_smooth(method = lm, formula = y~x)+
  labs(title='Daily Calories vs Very Active Minutes')

ggplot(final_data, aes(x = VeryActiveMinutes, y = TotalMinutesAsleep))+
  geom_point()+
  geom_smooth(method = lm, formula = y~x)+
  labs(title='Very Active Minutes vs Total Sleep Time')

ggplot(final_data, aes(x=sleep_time,y=TotalMinutesAsleep, fill= sleep_time))+ geom_boxplot()+
  labs(title= "Sleeping Behaviour", x= "Sleep Type", y= "Total Minutes Asleep",legends= "sleep category" )+ 
  scale_fill_discrete(name="Sleep Category", labels= c("Good Sleep", "Not Enough", "Over Sleep"))

ggplot(final_data, aes(x=Active_distance,y=TotalSteps, fill= Active_distance))+ geom_boxplot()+
  labs(title= "Active Behaviour", x= "Active Type", y= "Total Steps",legends= "Active category" )+ 
  scale_fill_discrete(name="Active Category", labels= c("Active", "Very Active", "Less Active", 'Moderately Active', 'Sedentary'))

ggplot(final_data, aes(x = SedentaryMinutes, y = TotalMinutesAsleep))+
  geom_point()+
  geom_smooth(method = lm, formula = y~x)+
  labs(title='Sedentary Minutes vs Total Sleep Time')

ggplot(final_data, aes(x = Total_activity_mins, y = TotalMinutesAsleep))+
  geom_point()+
  geom_smooth(method = lm, formula = y~x)+
  labs(title='Total Active Minutes vs Total Sleep Time')

ggplot(final_data, aes(x = TotalDistance, y = VeryActiveMinutes))+
  geom_point()+
  geom_smooth(method = lm, formula = y~x)+
  labs(title='Very Active Minutes vs Total Distance')

ggplot(final_data, aes(x = FairlyActiveMinutes, y = TotalDistance))+
  geom_point()+
  geom_smooth(method = lm, formula = y~x)+
  labs(title='Fairly Active Minutes vs Total Distance')

