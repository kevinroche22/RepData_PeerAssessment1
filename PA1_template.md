---
title: "Reproducible Research - Assignment One"
author: "Kevin Roche"
date: "11/08/2021"
output:
  prettydoc::html_pretty:
    theme: architect
    highlight: github
    keep_md: yes
---


# Assignment One
This R Markdown document answers the questions in assignment one of the reproducible research course of JHU's Data Science Specialization.

## Task One
Load and process the data.


```r
## Load data
activityData <- read.csv("activity.csv")

## Format date column as date class
activityData$date <- as.Date(activityData$date)
```

## Task Two
1. Calculate the total number of steps taken per day.
2. Make a histogram of the total number of steps taken each day.
3. Calculate and report the mean and median of the total number of steps taken per day.


```r
## Calculate total number of steps taken per day
totalDailySteps <- activityData %>% 
        group_by(date) %>% 
        summarise(steps = sum(steps))

## Plot total number of steps taken each day
totalDailySteps %>% ggplot(aes(x= date, y = steps)) +
        geom_histogram(stat = "identity") +
        geom_smooth(se = FALSE) + # shows trend over time
        theme_bw() +
        labs(x = "Date", y = "Steps") +
        ggtitle("Total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
## Calculate Mean
meanDailySteps <- totalDailySteps %>% 
        drop_na %>% # drop missing values
        summarise(steps = round(mean(steps), 2)) # round to two decimals
print(paste0("The mean number of daily steps is ", meanDailySteps)) 
```

```
## [1] "The mean number of daily steps is 10766.19"
```

```r
## Calculate Median
medianDailySteps <- totalDailySteps %>% 
        drop_na %>% 
        summarise(steps = median(steps))
print(paste0("The median number of daily steps is ", medianDailySteps)) 
```

```
## [1] "The median number of daily steps is 10765"
```
## Task Three
1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
## Calculate average steps taken per interval
meanStepsByInterval <- activityData %>% 
        drop_na %>% 
        group_by(interval) %>% 
        summarise(steps = mean(steps))

## Plot average number of steps taken per interval
meanStepsByInterval %>% ggplot(aes(x= interval, y = steps)) +
        geom_line() +
        theme_bw() +
        labs(x = "Interval", y = "Average Number of Steps") +
        ggtitle("Average number of steps taken per interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
## Determine which interval has highest number of average steps
print(meanStepsByInterval %>% slice(which.max(steps)))
```

```
## # A tibble: 1 x 2
##   interval steps
##      <int> <dbl>
## 1      835  206.
```
## Task Four
1. Calculate and report the total number of missing values in the dataset.
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
## Calculate NA's
mean(is.na(activityData))
```

```
## [1] 0.04371585
```

```r
## Impute missing values using MICE (Multivariate Imputations via Chained Equations)
imputedActivityData <- activityData %>% 
        mice::mice(m=5, maxit = 25) %>% 
        mice::complete(5)
```

```r
## Calculate total number of steps taken each day with imputed data
imputedTotalDailySteps <- imputedActivityData %>% 
        group_by(date) %>% 
        summarise(steps = sum(steps)) 

## Plot total number of steps taken each day with imputed data
imputedTotalDailySteps %>% 
        ggplot(aes(x= date, y = steps)) +
        geom_histogram(stat = "identity") +
        geom_smooth(se = FALSE) + # shows trend over time
        theme_bw() +
        labs(x = "Date", y = "Steps") +
        ggtitle("Total number of steps taken each day (NA's imputed)")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
## Calculate Mean
imputedMeanDailySteps <- imputedTotalDailySteps %>% 
        ungroup %>% 
        summarise(steps = round(mean(steps), 2)) # round to two decimals
print(paste0("The mean number of daily steps in the imputed data is ", imputedMeanDailySteps)) 
```

```
## [1] "The mean number of daily steps in the imputed data is 10443.64"
```

```r
## Calculate Median
imputedMedianDailySteps <- imputedTotalDailySteps %>% 
        ungroup %>% 
        summarise(steps = median(steps))
print(paste0("The median number of daily steps in the imputed data is ", imputedMedianDailySteps))
```

```
## [1] "The median number of daily steps in the imputed data is 10395"
```
The mice package assumes that missing data is missing at random, which is fairly plausible in this case. Mice uses predictive mean matching to impute missing numbers and is considered the gold standard for numerical imputation. Plus, you can code it in two lines - and I love efficiency.

## Task Five
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
## Create factor variable
imputedActivityData <- imputedActivityData %>% 
        mutate(dayType = case_when(weekdays(date) == "Monday" | weekdays(date) == "Tuesday" | weekdays(date) == "Wednesday" | weekdays(date) == "Thursday" | weekdays(date) == "Friday" ~ "weekday",
                                   weekdays(date) == "Saturday" | weekdays(date) == "Sunday" ~ "weekend"))
imputedActivityData$dayType <- as.factor(imputedActivityData$dayType)

## Calculate average steps taken by interval across day type
meanStepsByDayType <- imputedActivityData %>% 
        group_by(interval, dayType) %>% 
        summarise(steps = mean(steps))

## Plot average number of steps taken by interval across day type
meanStepsByDayType %>% ggplot(aes(x = interval, y = steps, colour = dayType)) +
        geom_line() +
        facet_wrap(. ~ dayType, dir = "v") +
        theme_bw() +
        labs(x = "Interval", y = "Average Number of Steps") +
        ggtitle("Average number of steps taken per interval across day type")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
