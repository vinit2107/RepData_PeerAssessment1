---
title: "Course Project 1 - Reproducible Research"
author: Vinit Deshbhratar
date: Novermber 16, 2019
output: 
        html_document:
                keep_md: yes
---

# Activity Monitoring

Importing the libraries

```r
library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)
```

1. **Code for reading in the dataset and/or processing the data**


```r
dataset  = read.csv('activity.csv')
```

In order to calculate steps taken each day, we need to group the data by day and add 
all the steps for each day.


```r
steps = dataset %>%
                group_by(date) %>%
                summarise(total_steps = sum(steps, na.rm = TRUE))
```

2. **Histogram of the total steps taken each day**


```r
ggplot(steps) +
        geom_histogram(aes(total_steps), bins = 30) +
        theme_bw() +
        ggtitle('Histogram of total steps taken each day') +
        xlab('Total Steps') +
        ylab('Frequency') +
        theme(plot.title = element_text(hjust = 0.5))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

3. **Mean and median number of steps taken each day**


```r
mean = mean(steps$total_steps, na.rm = TRUE)
median = median(steps$total_steps, na.rm = TRUE)

paste('Mean of the steps taken each day : ', round(mean, 2))
```

```
## [1] "Mean of the steps taken each day :  9354.23"
```

```r
paste('Median of the steps taken each day :', median)
```

```
## [1] "Median of the steps taken each day : 10395"
```

In order to make a time series plot, we need to combine the interval and date column to create a new field called date_time which stores the date in date and time format.
In order to achieve this, first we'll need to pad 0 to the interval so that each interval is of length 4. Then we need to insert ':' in the padded interval so that we can use it with the date so that we form a date time column.



4. **Time series plot of the number of steps taken each day**

The time series plot has to be plotted to determine the variation of the number of steps taken during each 5 minute interval of the day. TO plot this plot, we'll group the data on the basis if interval and average the number of steps taken during that interval. We also need the interval to be in 'hm' format, hence converting the datatype to represent the hm.


```r
steps = dataset %>%
                group_by(interval) %>%
                summarise(mean_steps = mean(steps, na.rm=TRUE))

steps$interval = hm(steps$interval)
```


```r
ggplot(steps) +
        geom_line(aes(interval, mean_steps)) +
        theme_bw() +
        ggtitle('Time-series plot of the number of steps taken each day') +
        xlab('Date') +
        ylab('Total number of steps taken') +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_x_time()
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

5. **The 5-minute interval that, on average, contains the maximum number of steps**


```r
paste('The maximum number of steps are taken at time :', steps[[steps$mean_steps == max(steps$mean_steps) ,1]])
```

```
## [1] "The maximum number of steps are taken at time : 8H 35M 0S"
```

6. **Code to describe and show a strategy for imputing missing data**

We'll identify the number of days where the steps are null.


```r
unique(dataset$date[is.na(dataset$steps)])
```

```
## [1] 2012-10-01 2012-10-08 2012-11-01 2012-11-04 2012-11-09 2012-11-10
## [7] 2012-11-14 2012-11-30
## 61 Levels: 2012-10-01 2012-10-02 2012-10-03 2012-10-04 ... 2012-11-30
```

There are 8 dates for which there are NA values in steps. As seen from the time series plot, we know that the average number of steps vary during the day, we'll replace the missing values in the table with the mean value of steps during that particular time.


```r
dataset = dataset %>%
                group_by(interval) %>%
                mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps))
```

7. **Histogram of the total number of steps taken each day after missing values are imputed**

Grouping the data on the basis of date and calculating the total number of steps taken each day.


```r
steps = dataset %>%
                group_by(date) %>%
                summarise(total_steps = sum(steps, na.rm = TRUE))
```


```r
ggplot(steps) +
        geom_histogram(aes(total_steps), bins = 30) +
        theme_bw() +
        ggtitle('Histogram after imputing the NA values') +
        xlab('Total Steps') +
        ylab('Frequency') +
        theme(plot.title = element_text(hjust = 0.5))
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

The histogram has changed when compared with the histogram plotted with non-imputed values. We can identify that the bar on 0 has reduced and the steps in the region of 12000 steps is more defined in the new histogram.

8. **Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends**

We'll add a column to the dataset to identify it as a weekday or a weekend


```r
dataset$week_day = weekdays(dataset$date_time)

weekdays = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
weekenddays = c('Saturday', 'Sunday')

dataset = dataset %>%
                group_by(week_day) %>%
                mutate(Weekday_weekend = ifelse(week_day %in% weekdays, 'Weekday', 'Weekend'))
```

After adding a column to identify the day as a weekday or a weekend, we'll group the data on the basis of weekday and interval to get the data to plot.


```r
steps = dataset %>%
                group_by(Weekday_weekend, interval) %>%
                summarise(mean_steps = mean(steps, na.rm = TRUE))

steps$interval = hm(steps$interval)
```


```r
ggplot(steps) +
        geom_line(aes(interval, mean_steps)) +
        scale_x_time() +
        facet_grid(.~Weekday_weekend) +
        theme_bw() +
        xlab('Interval') +
        ylab('Mean Steps') +
        ggtitle('Weekday and Weekend comparision of the time series plot for steps') +
        theme(plot.title = element_text(hjust = 0.5))
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

