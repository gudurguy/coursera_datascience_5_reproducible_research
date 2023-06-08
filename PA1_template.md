---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Data
The data for this assignment can be downloaded from the course web site:

* Dataset: Activity monitoring data [52K] (https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

The variables included in this dataset are:

* steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

* date: The date on which the measurement was taken in YYYY-MM-DD format

* interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Assignment
## Student Work Starts Here
## Loading and preprocessing the data


```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```


```r
#load needed libs
library(ggplot2)
# files and url variables
destFilename <- "Dataset.zip"
URL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
csvFile <- "activity.csv"
# if the zip file does not exists, then download
if (!file.exists(destFilename)){
  download.file(URL, destFilename, method="curl")
}
# if the csv file does not exist, then unzip the downloaded file in the previous step
if (!file.exists(csvFile)) {
  unzip(destFilename)
}
#read csv file
activity <- read.csv(csvFile)
```

### Check the data

```r
names(activity)
```

```
## [1] "steps"    "date"     "interval"
```

```r
dim(activity)
```

```
## [1] 17568     3
```

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

### Transform the data as needed


```r
activity$date <- as.Date(activity$date, '%Y-%m-%d')
activity$interval <- sprintf("%04d", activity$interval)
activity$interval <- format(strptime(activity$interval, format="%H%M"), format = "%H:%M")
```

## What is mean total number of steps taken per day?

```r
dailySteps <- tapply(activity$steps, activity$date, sum, na.rm=T)
# Histogram of the total number of steps taken each day
dailySteps %>% qplot(xlab='Steps per day', ylab='Frequency', binwidth=500)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
# Mean and median number of steps taken each day
dailyMeanSteps <- mean(dailySteps)
dailyMedianSteps <- median(dailySteps)
```
The Daily Mean: 9354.2295082, and the Daily Median: 10395

## What is the average daily activity pattern?


```r
averageSteps <- activity %>% filter(!is.na(steps)) %>% group_by(interval) %>%
        summarize(steps = mean(steps))
#Time series plot of the average number of steps taken
averageSteps %>% ggplot(aes(x=interval, y=steps, group=1)) + geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
#The 5-minute interval that, on average, contains the maximum number of steps
maxSteps <- which.max(averageSteps$steps)
maxStepsInterval <- averageSteps[which.max(averageSteps$steps), ][[1]]
```
A maximum number of steps: 104 are found at the interval: 08:35

## Imputing missing values

```r
# Show missing values
miss <- sapply(activity, is.na) %>% sum
```
Number of missing values: 2304


```r
# Code to describe and show a strategy for imputing missing data
# we will take the mean and fill the missing values with that
stepsComplete <- activity$steps
stepsComplete[is.na(stepsComplete)] <- round(mean(activity$steps, na.rm = T), digits=0)
stepsComplete <- as.numeric(stepsComplete)
activityComplete <- cbind.data.frame(stepsComplete, activity$date, activity$interval)
colnames(activityComplete) <- colnames(activity)
#Histogram of the total number of steps taken each day after missing values are imputed
daysteps_complete <- tapply(activityComplete$steps, activityComplete$date, sum)
daysteps_complete %>% qplot(xlab='Steps per day', ylab='Frequency', binwidth=500)
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

## Are there differences in activity patterns between weekdays and weekends?

```r
# create a new column weekdayornot and fill it with TRUE or False based on Week day of Not
activityComplete$date <- as.Date(activityComplete$date)
weekdays <-c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
    activityComplete$weekdayornot <- factor((weekdays(activityComplete$date) %in% weekdays),levels = c(FALSE, TRUE), labels = c('FALSE', 'TRUE'))
activityComplete$weekdayornot <- factor((weekdays(activityComplete$date) %in% weekdays), labels = c('FALSE', 'TRUE'))


# create Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
averageStepsComplete <- activityComplete %>% group_by(interval, weekdayornot) %>%
        summarise(steps = mean(steps))
averageStepsComplete %>% ggplot(aes(x=interval, y=steps, group=1)) +
        geom_line() +
        facet_grid(weekdayornot~.)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
