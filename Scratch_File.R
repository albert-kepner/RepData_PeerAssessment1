library(dplyr)
library(lubridate)
library(ggplot2)

v1 <- 1:60
v2 <- 1:60


df1 <- cbind(v1, v2)
str(df1)

df1 <- data.frame(df1)
str(df1)

mysum <- function(x, y) { x + y}
myf <- function(d1) {
  if(weekday(d1) )
}

df1 %>% mutate( sumv1v2= mysum(v1,v2))

today <- date()

as.date(today)
d1 <- ymd('2021-01-01')

weekends <- function(day_of_week) {
  if("Saturday" == day_of_week || "Sunday" == day_of_week)  {
    weekend <- "weekend"
  } else {
    weekend <- "weekday"
  }
  weekend
}

d1<-as.Date(today())

d1<-ymd('2012-10-01')
d1
d2<-weekdays(d1)
d2

d3<-weekends(d2)

d3

weekends("Sunday")


d1<-ymd('2012-10-01')

v0 <- 1:60

d0 <- cbind(v0)

df1 <- data.frame(d0)

df1

names(df1)<-"ID"
df1

df2 <- df1 %>% mutate(date = ymd('2012-10-01') + ID)
df2

df3 <- df2 %>% mutate(day_of_week = weekdays(date))
df3

df4 <- df3 %>% mutate(weekend_flag = weekends(day_of_week))
df4

s1 <- sapply(df3$day_of_week, weekends)
str(s1)












