---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
#load packages I will need for assignment
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
## Warning: package 'lubridate' was built under R version 3.5.1
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.5.1
```

```r
library(Hmisc)
```

```
## Warning: package 'Hmisc' was built under R version 3.5.1
```

```
## Loading required package: lattice
```

```
## Loading required package: survival
```

```
## Loading required package: Formula
```

```
## 
## Attaching package: 'Hmisc'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     src, summarize
```

```
## The following objects are masked from 'package:base':
## 
##     format.pval, units
```

```r
#set working directory
setwd("J:/CPHI/Data Science Coursera/Reproducible research/week 2/RepData_PeerAssessment1/activity")
#read in data
activity<-read.csv("activity.csv", stringsAsFactors = FALSE)
#convert date to date class
activity$date<-date(activity$date)
#create variable for day of the week to identify weekends
activity$dayofweek<-wday(activity$date, label=TRUE, abbr=TRUE)
#Identify weekends
activity$weekend<-if_else(activity$dayofweek=="Sat"| activity$dayofweek=="Sun",1,0)

#create a dataset of the sum and mean steps by date

sumsteps<-activity %>% group_by(date) %>% summarise(sum_st=sum(steps),mean_st = mean(steps))

#create a dateset of the mean number of steps by interval 
meanintsteps<-activity %>% group_by(interval)%>%summarise(mean_st=mean(steps, na.rm=TRUE))
```

## What is mean total number of steps taken per day?


```r
#histogram of total steps per day
p<-ggplot(sumsteps, aes(x=sum_st) )+geom_histogram()
p
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->
  
  
  Mean number of steps per day is 1.0766189\times 10^{4}  
Medain number of steps per day is 10765 


## What is the average daily activity pattern?

```r
#create a timeseries graph
g<-ggplot(meanintsteps, aes(x=interval, y=mean_st))+geom_line()
g
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
  
  
  Interval with the most steps per day is:  

```r
#which interval has the max
meanintsteps[which.max(meanintsteps$mean_st),]
```

```
## # A tibble: 1 x 2
##   interval mean_st
##      <int>   <dbl>
## 1      835    206.
```


## Imputing missing values

```r
#total number of missing values
sum(is.na(activity))
```

```
## [1] 2304
```

```r
#save a new version of the dataset for imputation
activity2<-activity
#impute steps with the mean number of steps by interval
activity2<-activity2 %>% group_by(interval) %>% mutate(steps=ifelse(is.na(steps),mean(steps, na.rm = T),steps))
```

```
## Warning: package 'bindrcpp' was built under R version 3.5.1
```

```r
#check that NAs were imputed
sum(is.na(activity2))
```

```
## [1] 0
```

```r
#check if there is difference in the mean number of steps a day now
sumsteps2<-activity2 %>% group_by(date) %>% summarise(sum_st=sum(steps))
mean(sumsteps2$sum_st, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(sumsteps2$sum_st, na.rm=TRUE)
```

```
## [1] 10766.19
```
  
  
  The mean number of steps with the NAs in the dataset is 1.0766189\times 10^{4}  


The mean number of steps with the NAs imputed is 1.0766189\times 10^{4}


## Are there differences in activity patterns between weekdays and weekends?  
Yes there are:

```r
meanintsteps2<-activity %>% group_by(interval,weekend)%>%summarise(mean_st=mean(steps, na.rm=TRUE))
g<-ggplot(meanintsteps2, aes(x=interval, y=mean_st, colour=factor(weekend)))+geom_line()+scale_colour_discrete(name="Weekend", breaks=c(0,1),labels=c("No","Yes"))+ylab("Mean steps")
g                                                                                                                                                        
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
