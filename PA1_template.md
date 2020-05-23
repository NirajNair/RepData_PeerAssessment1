---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data

```r
library(ggplot2)
unzip('activity.zip')
data <- read.csv('activity.csv')
```


## What is mean total number of steps taken per day?

#### 1. The total number of steps taken per day. (Dates with steps value as 'NA' were excluded)

```r
agg <- aggregate(steps ~ date, data = data, sum)
head(agg, 10)
```

```
##          date steps
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
```

#### 2. Histogram of the total number of steps taken each day.

```r
steps <- ggplot(aes(steps), data = agg)
steps + geom_histogram(binwidth = 1000) +
    labs(x = "Steps") +
    labs(title = "Total Steps taken by a human.")
```

![](PA1_template_files/figure-html/totalStepsHist-1.png)<!-- -->

#### 3. Mean number of steps are

```r
mean(agg$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

#### 4. Median number of steps are

```r
median(agg$steps, na.rm = TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

#### 1. Time series plot of  interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
aggStepsInt <- aggregate(steps ~ interval, data = data, mean,na.rm = T)
stepsInterval <- ggplot(aes(x = interval, y = steps), data = aggStepsInt)
stepsInterval + geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

#### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
aggStepsInt[which.max(aggStepsInt$steps),1]
```

```
## [1] 835
```


## Imputing missing values

#### 1. Calculate and report the total number of missing values in the dataset.

```r
sum(!complete.cases(data))
```

```
## [1] 2304
```

#### 2. Using the mean for that 5-minute interval for filling in all of the missing values in the dataset

```r
stepsAvg <- aggregate(steps ~ interval, data = data, mean)
completeSteps <- numeric()
for (i in 1:nrow(data)) {
    observation <- data[i,]
    if(is.na(observation$steps)){
        steps <- subset(stepsAvg, interval == observation$interval)$steps
    }else{
        steps <- observation$steps
    }
    completeSteps <- c(completeSteps, steps)
}
```

#### 3. New dataset that is equal to the original dataset but with the missing data filled in.

```r
newData <- data
newData$steps <- completeSteps
```

#### 4. Histogram of the total number of steps taken each day

```r
completeAgg <- aggregate(steps ~ date, data = newData, sum)
noSteps <- ggplot(aes(steps), data = completeAgg)
noSteps + geom_histogram() +
    labs(x = "Steps") +
    labs(title = "Total Steps taken by a human.")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

#### 5. Mean number of steps are

```r
mean(completeAgg$steps)
```

```
## [1] 10766.19
```

#### 6. Median number of steps are

```r
median(completeAgg$steps)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

#### 1. Creating a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend.

```r
days <- newData$date
days <- as.POSIXct(days, format = "%Y-%m-%d")
days <- weekdays(days)
dayNames = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
for(i in 1:length(days)){
    if(days[[i]] %in% dayNames){
        days[[i]] <- "weekdays"
    }else
        days[[i]] <- "weekends"
}
newData$days <- days
head(newData)
```

```
##       steps       date interval     days
## 1 1.7169811 2012-10-01        0 weekdays
## 2 0.3396226 2012-10-01        5 weekdays
## 3 0.1320755 2012-10-01       10 weekdays
## 4 0.1509434 2012-10-01       15 weekdays
## 5 0.0754717 2012-10-01       20 weekdays
## 6 2.0943396 2012-10-01       25 weekdays
```

#### 2. Panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
library(lattice)
aggDays <- aggregate(steps ~ interval + days, data = newData, sum)
xyplot(aggDays$steps ~ aggDays$interval | aggDays$days, 
       main="Average Steps per Day by Interval",xlab="Interval",
       ylab="Steps",layout=c(1,2), type="l")
```

![](PA1_template_files/figure-html/dayPlot-1.png)<!-- -->
