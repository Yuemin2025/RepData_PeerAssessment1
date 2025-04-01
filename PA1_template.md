---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

``` r
   # Load the data
   activity <- read.csv("activity.csv")

   # Convert date column to Date format
   activity$date <- as.Date(activity$date)

   # Display the structure of the dataset
   str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

``` r
   # Show the first few rows
   head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```


## What is mean total number of steps taken per day?

``` r
# Calculate total steps per day, ignoring NAs
daily_steps <- tapply(activity$steps, activity$date, sum, na.rm = TRUE)

# Create a histogram of total steps per day
hist(daily_steps, 
     breaks = 20, 
     main = "Histogram of Total Steps per Day", 
     xlab = "Total Steps per Day", 
     col = "blue", 
     border = "black")
```

![](PA1_template_files/figure-html/total_steps_per_day-1.png)<!-- -->

``` r
# Calculate mean and median
mean_steps <- mean(daily_steps)
median_steps <- median(daily_steps)

# Report the mean and median
cat("Mean total steps per day:", round(mean_steps, 2), "\n")
```

```
## Mean total steps per day: 9354.23
```

``` r
cat("Median total steps per day:", round(median_steps, 2), "\n")
```

```
## Median total steps per day: 10395
```

## What is the average daily activity pattern?


``` r
# Calculate the average steps per interval across all days
avg_steps_per_interval <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)

# Create a time series plot ( type = "l")
plot(x = as.numeric(names(avg_steps_per_interval)), 
     y = avg_steps_per_interval, 
     type = "l", 
     xlab = "5-Minute Interval", 
     ylab = "Average Steps (Across All Days)", 
     main = "Average Daily Activity Pattern", 
     col = "red")
```

![](PA1_template_files/figure-html/daily_activity_pattern-1.png)<!-- -->

``` r
# Find the interval with the maximum average steps
max_interval <- names(avg_steps_per_interval)[which.max(avg_steps_per_interval)]
max_steps <- max(avg_steps_per_interval)

# Report the result
cat("The 5-minute interval with the maximum average steps is:", max_interval, "\n")
```

```
## The 5-minute interval with the maximum average steps is: 835
```

``` r
cat("Average steps in that interval:", round(max_steps, 2), "\n")
```

```
## Average steps in that interval: 206.17
```

## Imputing missing values

``` r
# Calculate the total number of rows with NA in the steps 
total_NAs <- sum(is.na(activity$steps))

# Report the total number of missing values
cat("Total number of rows with missing values (NA) in steps:", total_NAs, "\n")
```

```
## Total number of rows with missing values (NA) in steps: 2304
```

``` r
# Calculate the mean steps per interval 
avg_steps_per_interval <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE) 

# Create a new dataset 
activity_imputed <- activity 

# Replace NA values with the mean for that interval 
for (i in 1:nrow(activity_imputed)) {
    if (is.na(activity_imputed$steps[i])) {
        interval <- activity_imputed$interval[i]
        activity_imputed$steps[i] <- avg_steps_per_interval[as.character(interval)]
    }
}

# Report the result(no NAs remain)
cat("Number of missing values after imputation:", sum(is.na(activity_imputed$steps)), "\n")
```

```
## Number of missing values after imputation: 0
```

``` r
# Calculate total steps per day with imputed data 
daily_steps_imputed <- tapply(activity_imputed$steps, activity_imputed$date, sum) 

# Create a histogram of total steps per day 
hist(daily_steps_imputed, 
     breaks = 20, 
     main = "Histogram of Total Steps per Day (Imputed Data)", 
     xlab = "Total Steps per Day", 
     col = "blue", 
     border = "black") 
```

![](PA1_template_files/figure-html/missing_values-1.png)<!-- -->

``` r
# Calculate mean and median 
mean_steps_imputed <- mean(daily_steps_imputed) 
median_steps_imputed <- median(daily_steps_imputed) 

# Report the mean and median 
cat("Mean total steps per day (imputed):", round(mean_steps_imputed, 2), "\n") 
```

```
## Mean total steps per day (imputed): 10766.19
```

``` r
cat("Median total steps per day (imputed):", round(median_steps_imputed, 2), "\n")
```

```
## Median total steps per day (imputed): 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

## Create a Factor Variable for Weekday/Weekend with imputed data

``` r
# Add a new column to classify dates as weekday or weekend 
activity_imputed$day_type <- ifelse(weekdays(activity_imputed$date) %in% c("Saturday", "Sunday"), "weekend", "weekday") 

# Convert day_type to a factor with two levels 
activity_imputed$day_type <- factor(activity_imputed$day_type, levels = c("weekday", "weekend")) 

# Calculate average steps per interval, separately for weekdays and weekends
avgsteps_by_interval_daytype <- tapply(activity_imputed$steps, list(activity_imputed$interval, activity_imputed$day_type), mean)

# Set up a 2-panel plot (one for weekdays, one for weekends)
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))

# Plot for weekdays
plot(x = as.numeric(rownames(avgsteps_by_interval_daytype)), 
     y = avgsteps_by_interval_daytype[, "weekday"], 
     type = "l", 
     xlab = "5-Minute Interval", 
     ylab = "Average Steps", 
     main = "Average Steps per Interval (Weekdays)", 
     col = "lightgreen")

# Plot for weekends
plot(x = as.numeric(rownames(avgsteps_by_interval_daytype)), 
     y = avgsteps_by_interval_daytype[, "weekend"], 
     type = "l", 
     xlab = "5-Minute Interval", 
     ylab = "Average Steps", 
     main = "Average Steps per Interval (Weekends)", 
     col = "red")
```

![](PA1_template_files/figure-html/weekday_weekend-1.png)<!-- -->

