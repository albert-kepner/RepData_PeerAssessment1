## Script1.R

### Load libraries
library(dplyr)
library(lubridate)
library(ggplot2)

### Unzip and Load the data file
zipfile <- "activity.zip"
exdir = "data"
unzip(zipfile, exdir=exdir)

## Loading and preprocessing the data

### Read the file activity.csv into an R data frame.
infile <- "data/activity.csv"
df <- read.csv(infile)
str(df)



df2 <- df %>% mutate (converted_date = ymd(date), na_steps = is.na(steps), ID = row_number())

str(df2)
summary (df2)
glimpse( df2)

df3 <- df2 %>% group_by( converted_date ) %>% summarize(daily_steps = sum(steps), missing_steps = sum(na_steps))

df3

## What is mean total number of steps taken per day?

### Show the number of steps each day in a bar chart
ggplot(df3, mapping=aes(x=converted_date, y=daily_steps)) + geom_bar(stat = "identity")

### Show the mean number of daily steps
original_mean <- mean(df3$daily_steps, na.rm = TRUE )
original_mean

### SHow the median for daily steps
original_median <- median(df3$daily_steps, na.rm = TRUE )
original_median

### show a histogram of frequency values for daily steps
hist(df3$daily_steps,breaks=10)

## What is the average daily activity pattern?

steps_by_interval <- df2 %>% group_by ( interval ) %>% 
  summarize (sum_steps = sum(steps, na.rm=TRUE), 
             mean_steps = mean(steps, na.rm=TRUE),
             average_steps_integer = as.integer(round(mean_steps, 0)),
             count_steps = sum(!is.na(steps)))
steps_by_interval <- steps_by_interval %>% mutate (row_index = as.integer( (interval + 5) / 5) )
steps_by_interval

### Plot the timeline of average steps versus interval, averaged across all days
g <- ggplot(steps_by_interval, mapping=aes(x=interval, y=mean_steps)) + geom_line()
g <- g + xlab("interval -- time of day (HH:MM)")
g <- g + ylab("Average steps per interval across all days")
g <- g + ggtitle("Timeline of Average Steps per Interval")
g

### Determine the maximum average step value across intervals.

max_steps <- max( steps_by_interval$mean_steps)
max_steps

### Determine which time interval has the max_steps value.
max_steps_interval <- steps_by_interval[steps_by_interval$mean_steps == max_steps, ]$interval
max_steps_interval

## Apparently the largest number of average steps 206.1698 occurred at interval
## 835 or 8:35 a.m.



## Imputing Missing Values

### 1. Calculate and report the total number of missing values in the dataset
### (i.e. the total number of rows with `NA`s)

summary(df2)

### The summary function shows that 2304 values of steps are NA.

dates_with_na_steps <- df2 %>% filter(is.na(steps)) %>% 
  group_by(date) %>% summarize(count_of_nas =n())
dates_with_na_steps

### It turns out that there are 288 5-minute intervals each day,
### and we are missing all of the steps data on 8 full days.
### This accounts for all of the 2304 = 8 * 288 missing values.

## (REMOVE) Verify we get the same average steps per day from sum of steps_by_interval divided by days with data.
## original_mean
## sum(steps_by_interval$sum_steps)/53
## sum(steps_by_interval$mean_steps)
## sum(steps_by_interval$average_steps_integer)

## 2. Devise a strategy for filling in all of the missing values in the dataset. 
## The strategy does not need to be sophisticated. For example, 
## you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
## 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

### Create a function to replace a missing step value
### with the corresponding average for that 5 minute interval
### across all the days with data.
impute_step_value <- function( steps, interval ) {
  if(!is.na(steps)) {
    ## just return the same steps value where data is present.
    result <- steps
  } else {
    ## interval is an integer in 24 hour time format HH:MM
    ## We want to find an index based on the number of 5 minute intervals
    ## within the current day.
    index <- 1 + 12 * (interval %/% 100) + (interval %% 60) %/% 5
    ## for missing values return the average for that time of day
    ## (rounded to an intger).
    result <- steps_by_interval$average_steps_integer[index]
  }
  if(is.na(result)) {
    print(paste('interval: ', interval))
  }
  result
}
### create a function to call impute_step_value for
### each row of data.frame df2
impute_row <- function(row_index) {
  impute_step_value(df2$steps[row_index], df2$interval[row_index])
}

### Create a vector of the original step values merged with imputed step values.
steps_imputed <- sapply(1:nrow(df2), impute_row)

### Form a new data.frame with selected columns  and including the imputed step values.
imputed_steps_df <- df2 %>% select( date, interval, converted_date ) %>% cbind(steps_imputed)
imputed_steps_df
summary(imputed_steps_df)


### Group the data by date to sum total steps per day
daily_df <- imputed_steps_df %>% group_by( converted_date ) %>% summarize(daily_steps = sum(steps_imputed))

## 4. Make a histogram of the total number of steps taken each day and Calculate and 
## report the **mean** and **median** total number of steps taken per day. 
## Do these values differ from the estimates from the first part of the assignment? 
## What is the impact of imputing missing data on the estimates of the total daily number of steps?
  
hist(daily_df$daily_steps,breaks=10)


mean(daily_df$daily_steps)
original_mean
median(daily_df$daily_steps)
original_median



