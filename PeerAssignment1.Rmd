Peer Assignment 1 for Reproducible Research
===========================================

###Loading and Preprocessing data 
```{r}
activitydata <- read.csv("activity.csv")
```

###What is the mean total number of steps taken per day?
```{r}
stepsperday <- aggregate(steps~date,activitydata,sum)
```
#####Histogram of steps taken per day
``` {r}
library(ggplot2)
ggplot(stepsperday,aes(x = steps)) + 
  geom_histogram(fill = "red", binwidth = 1000) +
  labs(title = "Histogram of Steps taken per day" , x = "Number of Steps per Day",
       y = "Number of times in a day(Count)") + theme_bw()
```

#####Mean and Median of total number of steps taken per day
```{r}
mean(stepsperday$steps)
median(stepsperday$steps)
```

###Average daily activity pattern

#####Time series plot of 5-minute interval(x-axis) and the average number of steps taken, averaged across all days(y-axis)
```{r}
stepsatintervals <- aggregate(steps~interval,activitydata,mean)
library(ggplot2)
ggplot(stepsatintervals,aes(x = interval , y = steps)) + 
  geom_line(color = "red") +
  labs(title = "Average daily Activity Pattern" , x = "Interval",
       y = "Number of steps") + theme_bw()
```

#####Maximum number of steps in 5 minute interval
```{r}
stepsatintervals$interval[max(stepsatintervals$steps)]
```

###Inputting missing values
#####Number of NA values
```{r}
Nas <- which(is.na(activitydata$steps))
length(Nas)
```
#####Filling NA values
```{r}
activitydata$steps[which(is.na(activitydata$steps))] <- mean(activitydata$steps,na.rm = T)
```
#####Create a new dataset without NA values
```{r}
activitydata2 <- activitydata
```
#####Histogram of total number of steps taken each day
```{r}
stepsperday2 <- aggregate(steps~date,activitydata,sum)
library(ggplot2)
ggplot(stepsperday2,aes(x = steps)) + 
  geom_histogram(fill = "blue", binwidth = 1000) +
  labs(title = "Histogram of Steps taken per day" , x = "Number of Steps per Day",
       y = "Number of times in a day(Count)") + theme_bw()
```

#####Mean and Median of number of steps taken per day
```{r}
mean(stepsperday2$steps)
median(stepsperday2$steps)
```

###Are there differences in activity patterns between weekdays and weekends?
###Create a new factor-weekday(0 if weekday and 1 if weekend)
```{r}
activitydata2$date <- as.Date(activitydata2$date)
activitydata2$day <- weekdays(activitydata2$date)
activitydata2$weekend <- 0
activitydata2$weekend[activitydata2$day %in% c('Saturday','Sunday')] <- 1
activitydata2$weekend <- as.factor(activitydata2$weekend)
```
###Make a panel plot of 5-min interval(x-axis) and average number of steps taken averaged across all weekdays and weekends(y-axis)
```{r}
ggplot(activitydata2,aes(x=interval,y=steps)) +
  geom_line(color = "orange") +
  facet_wrap(~weekend , ncol = 1 , nrow = 2) +
  labs(x = "Interval" ,y = "Number of steps") +
  theme_bw()
```