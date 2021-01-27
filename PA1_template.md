---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


# Loading and preprocessing the data

### Load libraries

```r
library(dplyr)
```

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
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
library(ggplot2)
```

### Unzip and Load the data file  
### Read the file activity.csv into an R data frame  
### Preprocess the data to summarize the number of steps each day.

```r
zipfile <- "activity.zip"
exdir = "data"
unzip(zipfile, exdir=exdir)
infile <- "data/activity.csv"
df <- read.csv(infile)
df2 <- df %>% mutate (converted_date = ymd(date), na_steps = is.na(steps), ID = row_number())
df3 <- df2 %>% group_by( converted_date ) %>% summarize(daily_steps = sum(steps), missing_steps = sum(na_steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

# What is mean total number of steps taken per day?


### Show the mean number of daily steps

```r
original_mean <- mean(df3$daily_steps, na.rm = TRUE )
original_mean
```

```
## [1] 10766.19
```

### SHow the median for daily steps

```r
original_median <- median(df3$daily_steps, na.rm = TRUE )
original_median
```

```
## [1] 10765
```

### Show the number of steps each day in a bar chart

```r
ggplot(df3, mapping=aes(x=converted_date, y=daily_steps)) + 
  geom_bar(stat = "identity") +
  labs(title="Number of Steps Each Day")
```

```
## Warning: Removed 8 rows containing missing values (position_stack).
```

![](PA1_template_files/figure-html/bar_chart_steps_per_day-1.png)<!-- -->


### show a histogram of frequency values for daily steps

```r
hist(df3$daily_steps,breaks=10)
```

![](PA1_template_files/figure-html/histogram_of_daily_steps-1.png)<!-- -->


# What is the average daily activity pattern?



# Imputing missing values



# Are there differences in activity patterns between weekdays and weekends?
