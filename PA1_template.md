---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
Load the necessary packages:

``` r
library(zoo)
library(formatdown)
library(dplyr)
```

First we have to load the data.

``` r
if(!file.exists('activity.csv')){
    unzip('activity.zip')
}
data <- read.csv("activity.csv")
```

Preprocessing the data involves formatting the dates as dates rather than characters.

``` r
data$date <- as.Date(data$date)
```

## What is mean total number of steps taken per day?
First we'll aggregate the data by date.

``` r
daily_steps <- aggregate(data$steps, by=list(data$date), FUN=sum, na.rm=TRUE)
colnames(daily_steps) <- c("Date", "steps")
```

Now we'll make a histogram of the daily steps.

``` r
hist(daily_steps$steps, xlab="Steps", main="Daily Steps", breaks=10)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

Next we'll calculate the mean number of steps taken each day.

``` r
mean_steps <- mean(daily_steps$steps)
median_steps <- median(daily_steps$steps)
```

The mean number of steps per day is 9354.2, and the median is 10395.

## What is the average daily activity pattern?
First we'll aggregate the data by interval.

``` r
steps_by_interval <- aggregate(data$steps, by=list(data$interval), FUN=mean, na.rm=TRUE)
colnames(steps_by_interval) <- c("Interval", "Average steps")
```

Next we'll make a time series plot of the interval data.

``` r
plot(x=steps_by_interval$Interval, y=steps_by_interval$`Average steps`, type="l", lwd=2,
     xlab="Interval", ylab="Average Steps", main="Average Steps Throughout the Day")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

## Imputing missing values
To count the number of missing values, run the following:

``` r
missing <- sum(is.na(data$steps))
```

There are 2304 missing values in the data.

Next we'll fill in the missing values by replacing them with the mean for that 5-minute interval.

``` r
data2 <- data
data2$steps <- na.aggregate(data$steps, by=data$interval, FUN=mean, na.rm=FALSE)
daily_steps2 <- aggregate(data2$steps, by=list(data2$date), FUN=sum, na.rm=TRUE)
colnames(daily_steps2) <- c("Date", "steps")
```

Now we'll make a histogram of the total steps taken each day in the new data.

``` r
hist(daily_steps2$steps, xlab="Steps", main="Daily Steps", breaks=10)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)

Finally we'll calculate the mean and median of the daily steps.

``` r
mean_steps2 <- mean(daily_steps2$steps)
median_steps2 <- median(daily_steps2$steps)
```

The mean number of steps per day is $10766.2$, and the median is $10766.2$.

## Are there differences in activity patterns between weekdays and weekends?
First we'll add the weekday to the data and aggregate it by interval.

``` r
data2$weekday <- weekdays(data2$date)

weekday_data <- data2 %>%
  filter(weekday %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))

weekend_data <- data2 %>%
  filter(weekday %in% c("Saturday", "Sunday"))

steps_by_interval_weekday <- aggregate(weekday_data$steps, by=list(weekday_data$interval), FUN=mean, na.rm=TRUE)
colnames(steps_by_interval_weekday) <- c("Interval", "Average steps")

steps_by_interval_weekend <- aggregate(weekend_data$steps, by=list(weekend_data$interval), FUN=mean, na.rm=TRUE)
colnames(steps_by_interval_weekend) <- c("Interval", "Average steps")
```

Next we'll make a time series plot for the interval data on weekdays and weekends.

``` r
parameter <- par(mfrow=c(2, 1))

plot(x=steps_by_interval_weekday$Interval, y=steps_by_interval_weekday$`Average steps`, type="l", lwd=1.5,
     xlab="Interval", ylab="Average Steps", main="Weekday", col="blue")

plot(x=steps_by_interval_weekend$Interval, y=steps_by_interval_weekend$`Average steps`, type="l", lwd=1.5,
     xlab="Interval", ylab="Average Steps", main="Weekend", col="blue")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png)
