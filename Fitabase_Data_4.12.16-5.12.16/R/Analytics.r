library(odbc)
library(DBI)
library(dplyr)
library(tidyverse)
library(httpgd)
library(lubridate)

#Connect to Local database through DSN
db_conn <- dbConnect(odbc(), "LocalDSN")

dbListTables(db_conn)

#Assign a database table to a variable
daily_activity <- dbReadTable(db_conn, "dailyActivity_merged")
weightlog <- dbReadTable(db_conn, "weightLogInfo_merged")
hourlyintensities <- dbReadTable(db_conn, "hourlyIntensities_merged")
hourlycalories <- dbReadTable(db_conn, "hourlyCalories_merged")


#Plot calories burned by day with a graph for each user
ggplot(data = daily_activity) +
  aes(x = ActivityDate, y = Calories, colour = factor(Id)) +
  geom_line() +
  facet_wrap(~Id)

#Here we assign a modified version of the table to a variable, after that we
# create a column that shows the week of the year for each date,
# and another that shows which day of the week it is.
Id1503960366Cal <- daily_activity %>%
  filter(Id == 1503960366) %>%
  group_by(ActivityDate) %>%
  select(Id, ActivityDate, Calories)

Id1503960366Cal$week_num <- strftime(Id1503960366Cal$ActivityDate, format = "%V")

Id1503960366Cal$weekday <- wday(Id1503960366Cal$ActivityDate, week_start=1)


#Plot calories burned for each day, comparing week to week.

ggplot(data = Id1503960366Cal, aes(x = weekday, y = Calories, colour = week_num)) +
  geom_line() +
  coord_cartesian(xlim = c(1, 7)) +
  scale_x_discrete(labels = c("Mon.", "Tues.", "Wed.", "Thurs.", "Fri.", "Sat.", "Sun."), breaks=c(1:7)) +
  facet_wrap(~week_num, scales = "free_x", ncol = 1)

#Plot average calories burned for each weekday.

ggplot(data = Id1503960366Cal, aes(x = factor(weekday), y = Calories)) +
  scale_x_discrete(labels = c("Mon.", "Tues.", "Wed.", "Thurs.", "Fri.", "Sat.", "Sun."), breaks=c(1:7)) +
  scale_y_continuous(limits = c(0,4000)) +
  geom_bar(fill = "lightblue", colour = "black", stat = "summary", fun = "mean") +
  ggtitle("Avg Calories spent per day for user 1503960366") +
  theme(plot.title = element_text(size = 16, lineheight = .9)) +
  xlab("Weekday")

#Plot average calories burned for each weekday with a different id.

Id1644430081Cal <- daily_activity %>%
  filter(Id == 1644430081) %>%
  group_by(ActivityDate) %>%
  select(Id, ActivityDate, Calories)

Id1644430081Cal$week_num <- strftime(Id1644430081Cal$ActivityDate, format = "%V")

Id1644430081Cal$weekday <- wday(Id1644430081Cal$ActivityDate, week_start=1)

ggplot(data = Id1644430081Cal, aes(x = factor(weekday), y = Calories)) +
  scale_x_discrete(labels = c("Mon.", "Tues.", "Wed.", "Thurs.", "Fri.", "Sat.", "Sun."), breaks=c(1:7)) +
  scale_y_continuous(limits = c(0,4000)) +
  geom_bar(fill = "lightblue", colour = "black", stat = "summary", fun = "mean") +
  ggtitle("Avg Calories spent per day for user 1644430081") +
  theme(plot.title = element_text(size = 16, lineheight = .9)) +
  xlab("Weekday")



hourlyCalxIntensities <- merge(x = hourlycalories, y = hourlyintensities, by = c("Id", "ActivityHour"))

Id1644430081CalxInt <- hourlyCalxIntensities %>%
  filter(Id >= 600000000)

ggplot(data = hourlyCalxIntensities, aes(x = TotalIntensity, y = Calories, colour = factor(Id))) +
  geom_point(colour = "grey92") +
  geom_smooth(method = lm, se = FALSE, fullrange = FALSE) +
  ggtitle("Calories spent - Activity Intensity per hour") +
  labs(colour = "User's Id") + 
  xlab("Total Intensity") +
  ylab("Calories spent") 
  

Id1644430081Intens <- hourlyintensities %>%
  filter(Id == 1644430081) %>%
  group_by(ActivityHour) %>%
  select(Id, ActivityHour, TotalIntensity) %>%
  mutate(Date = as.Date(ActivityHour))

Id1644430081Intens$week_num <- strftime(Id1644430081Intens$Date, format = "%V")

Id1644430081Intens$weekday <- wday(Id1644430081Intens$Date, week_start=1)

Id1644430081Intens <- Id1644430081Intens %>%
  filter(week_num == 16)


daysoftheweek <- list("1" = "Mon.", "2" = "Tues.", "3" = "Wed.", "4" = "Thurs.", "5" = "Fri.", "6" = "Sat.", "7" = "Sun.")


days_labeller <- function(variable, value){return(daysoftheweek[value])}

ggplot(data = Id1644430081Intens, aes(x = hour(ActivityHour), y = TotalIntensity)) +
  geom_col(fill = "lightblue", colour = "black") +
  ggtitle("User 1644430081's intensity each hour for week 16 of the year 2016") +
  xlab("Hour") +
  ylab("Intensity") +
  facet_wrap(~weekday, scales = "free_x", labeller=days_labeller) +
  scale_x_continuous(labels = c(0:24), breaks=c(0:24)) +
  theme(axis.text.x = element_text(size = rel(0.7)))

