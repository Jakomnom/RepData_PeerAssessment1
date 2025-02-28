---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Introduction

Modern activity monitoring devices, such as [Fitbit](http://www.fitbit.com), [Nike Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), or [Jawbone Up](https://jawbone.com/up), allow users to collect detailed data about their daily movements. These devices are part of the "quantified self" movement, where individuals track their activities to improve health, identify behavioral patterns, or satisfy technological curiosity. However, these datasets are often underutilized due to challenges in accessing raw data and applying statistical methods to interpret them.

This assignment analyzes data from a personal activity monitoring device. The dataset contains two months of step-count data (October and November 2012) collected at 5-minute intervals throughout the day.

---

## Data Description

The dataset can be downloaded from the course website:

- **Dataset**: [Activity Monitoring Data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

The dataset includes the following variables:

- **steps**: Number of steps taken in a 5-minute interval (missing values are coded as `NA`).
- **date**: Date of the measurement in `YYYY-MM-DD` format.
- **interval**: Identifier for the 5-minute interval when the measurement was taken.

The dataset is stored in a CSV file with 17,568 observations.

---

## Loading and Preprocessing the Data

Ensure the dataset is downloaded and loaded into the working environment. If the dataset is not already present, it will be downloaded and extracted automatically.


```r
# Check if the dataset exists; if not, download and extract it
if (!file.exists("activity.csv")) {
  temp <- tempfile()
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", temp)
  unzip(temp, exdir = ".")
  unlink(temp)
}

# Load the dataset
df <- read.csv("activity.csv", header = TRUE, sep = ",")
```

## What is mean total number of steps taken per day?
To determine this, aggregate all intervals by date and sum the steps and plot.  Also, check the daily mean and median steps across all dates.


```r
# Aggregate steps by date
totalStepsByDate <- aggregate(steps ~ date, df, sum)

# Plot histogram of total steps per day
hist(totalStepsByDate$steps, 
     main = "Total Steps Each Day", 
     col = "yellow", 
     xlab = "Number of Steps")
```

![](PA1_template2_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
# Calculate and print mean and median
dailyMean <- mean(totalStepsByDate$steps)
dailyMedian <- median(totalStepsByDate$steps)
print(paste("Daily Mean:", dailyMean))
```

```
## [1] "Daily Mean: 10766.1886792453"
```

```r
print(paste("Daily Median:", dailyMedian))
```

```
## [1] "Daily Median: 10765"
```

## What is the average daily activity pattern?
To determine the average daily activity pattern, aggregate the data by interval and compute the mean number of steps for each interval. The results are visualized using a time-series plot.

```r
# Aggregate steps by interval
stepsByInterval <- aggregate(steps ~ interval, df, mean)

# Plot average steps per interval
plot(stepsByInterval$interval, stepsByInterval$steps, 
     type = "l", 
     xlab = "Interval", 
     ylab = "Average Steps", 
     main = "Average Daily Activity Pattern")
```

![](PA1_template2_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
# Identify the interval with the maximum number of steps
max_interval <- stepsByInterval[which.max(stepsByInterval$steps), 1]
print(paste("Interval with Maximum Steps:", max_interval))
```

```
## [1] "Interval with Maximum Steps: 835"
```

## Imputing missing values
Missing values (NA) are imputed by replacing them with the average number of steps for the corresponding interval across all days. For the first day (all NA), values are set to zero to align with the low activity observed on the second day.

```r
# Count missing values
naCount <- sum(is.na(df$steps))
print(paste("Number of Missing Values:", naCount))
```

```
## [1] "Number of Missing Values: 2304"
```

```r
# Impute missing values
imputedData <- transform(df, 
                         steps = ifelse(is.na(steps), 
                                        stepsByInterval$steps[match(interval, stepsByInterval$interval)], 
                                        steps))

# Set the first day's steps to zero
imputedData[as.character(imputedData$date) == "2012-10-01", "steps"] <- 0

# Recalculate total steps by date
imputedStepsByDate <- aggregate(steps ~ date, imputedData, sum)

# Plot histograms of original and imputed data
hist(imputedStepsByDate$steps, 
     main = "Total Steps Each Day (Imputed vs Original)", 
     col = rgb(0, 1, 0), 
     xlab = "Number of Steps")
hist(totalStepsByDate$steps, 
     col = rgb(1, 1, 0), 
     add = TRUE)
legend("topright", c("Imputed", "Original"), 
       col = c("green", "yellow"), lwd = 8)
```

![](PA1_template2_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
# Calculate and print updated mean and median
imputedMean <- mean(imputedStepsByDate$steps)
imputedMedian <- median(imputedStepsByDate$steps)
print(paste("Imputed Daily Mean:", imputedMean))
```

```
## [1] "Imputed Daily Mean: 10589.6937828642"
```

```r
print(paste("Imputed Daily Median:", imputedMedian))
```

```
## [1] "Imputed Daily Median: 10766.1886792453"
```

## Are there differences in activity patterns between weekdays and weekends?
To compare activity patterns between weekdays and weekends, a new factor variable (dayFactor) is added to classify each date. The data is then aggregated and visualized using a lattice plot.

```r
library(lattice)

# Add weekday/weekend factor
imputedData$dayFactor <- as.factor(ifelse(weekdays(as.Date(imputedData$date)) %in% c("Saturday", "Sunday"), 
                                          "weekend", "weekday"))

# Aggregate steps by interval and day type
aggregateImputedData <- aggregate(steps ~ interval + dayFactor, imputedData, mean)

# Plot weekday vs weekend activity patterns
xyplot(steps ~ interval | dayFactor, 
       data = aggregateImputedData, 
       type = "l", 
       layout = c(1, 2), 
       main = "Interval Averages: Weekday vs Weekend", 
       xlab = "Interval", 
       ylab = "Average Steps")
```

![](PA1_template2_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

