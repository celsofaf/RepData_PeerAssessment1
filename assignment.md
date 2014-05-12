# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
I read the data from the compressed .zip file. I took a broad look on it and din't
think I needed to make changes (processing) on the data, at least initialy.

```r
data <- read.csv(unz("activity.zip", "activity.csv"))
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(data)
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

```r
tail(data)
```

```
##       steps       date interval
## 17563    NA 2012-11-30     2330
## 17564    NA 2012-11-30     2335
## 17565    NA 2012-11-30     2340
## 17566    NA 2012-11-30     2345
## 17567    NA 2012-11-30     2350
## 17568    NA 2012-11-30     2355
```

```r
summary(data$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##     0.0     0.0     0.0    37.4    12.0   806.0    2304
```


## What is mean total number of steps taken per day?
I will make a histogram of the steps and calculate mean and median, ignoring the
missing (NA) values. A histogram already ignores NA values. :-)

```r
daytotal <- unlist(lapply(split(data$steps, data$date), sum))
hist(daytotal, xlab = "Total steps per day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
mean(daytotal, na.rm = TRUE)
```

```
## [1] 10766
```

```r
median(daytotal, na.rm = TRUE)
```

```
## [1] 10765
```



## What is the average daily activity pattern?
Let's split the data and make a plot of the 5-min intervals averaged across all
days.

```r
intervalmeans <- lapply(split(data$steps, data$interval), mean, na.rm = TRUE)
intervalmeans <- unlist(intervalmeans)
plot(as.numeric(names(intervalmeans)), intervalmeans, type = "l", xlab = "5-min interval", 
    ylab = "Mean daily steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

Now, let's see which 5-min interval, on average across all the days in the dataset, contains the maximum number of steps.

```r
max.interval <- which(intervalmeans == max(intervalmeans))
names(max.interval)
```

```
## [1] "835"
```


## Imputing missing values
First, let's calculate the number of missing values for the steps in our data.

```r
length(data$steps[is.na(data$steps)])
```

```
## [1] 2304
```

Now, I was asked to devise a strategy for the missing values in the dataset. I chose to substitute the missing values by the mean of the corresponding 5-min interval, calculated over all days. A new dataset will be created, so I don't need to overwrite the original one.

```r
newdata <- data
for (i in 1:nrow(newdata)) {
    if (is.na(newdata$steps[i])) {
        int <- as.character(newdata$interval[i])
        newdata$steps[i] <- intervalmeans[int]
    }
}
```

I'm going to make a histogram of the total number of steps taken each day, using the new dataset, and calculate and report the mean and median total number of steps taken per day.

```r
daytotal <- unlist(lapply(split(newdata$steps, newdata$date), sum))
hist(daytotal, xlab = "Total steps per day")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

```r
print(paste("Mean total number of steps per day:", as.character(mean(daytotal))))
```

```
## [1] "Mean total number of steps per day: 10766.1886792453"
```

```r
print(paste("Median total number of steps per day:", median(daytotal)))
```

```
## [1] "Median total number of steps per day: 10766.1886792453"
```

The mean is the same as in the first part of the analysis (ignoring the NAs), as expected, since I simply forced the NAs to be the mean of each day... The median is now equal to the mean; before, it was only slightly different. The very large number of NAs, all of which were replaced by an avarage as I chose to do, is the cause of this data phenomenom.


## Are there differences in activity patterns between weekdays and weekends?
Firt, I will create a new factor variable in the dataset with two levels – “weekday” and “weekend” - indicating whether a given date is a weekday or weekend day. Then, I will make two plots, of the 5-min interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
wkday <- c()
candidates <- c("segunda", "terça", "quarta", "quinta", "sexta") # my system is in Portuguese
for (i in 1:nrow(newdata)) wkday <- c(wkday, weekdays(as.Date(newdata$date[i])) %in% 
    candidates)
newdata$wkday <- wkday
par(mfrow = c(2, 1))
newdata.wkday <- newdata[newdata$wkday == TRUE, ]
newdata.wkend <- newdata[newdata$wkday == FALSE, ]
wkdaymean <- unlist(lapply(split(newdata.wkday$steps, newdata.wkday$interval), 
    mean))
wkendmean <- unlist(lapply(split(newdata.wkend$steps, newdata.wkend$interval), 
    mean))
plot(as.numeric(names(wkdaymean)), wkdaymean, type = "l", xlab = "5-min interval, weekdays", 
    ylab = "Mean daily steps")
plot(as.numeric(names(wkendmean)), wkendmean, type = "l", xlab = "5-min interval, weekend", 
    ylab = "Mean daily steps")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

We can notice that, on weekends, the steps are more evenly distributed throughout the day as oposed to on weekdays. This is expected, because of the working hours, and the weekday peak at around 9:00 (and the smaller one around 18:00) can be attributed to commuting. And, clearly, on average, people wake up later on weekends...
