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



# What is the average daily activity pattern?



# Imputing missing values



# Are there differences in activity patterns between weekdays and weekends?
