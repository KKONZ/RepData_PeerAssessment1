Activity Monitoring
===================

##Author: Karl Konz
Date: *Sunday, February 7, 2016*

Data 
----
The raw data for this report can be downloaded from this address <https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip>. Use the script below to download data.


## Loading and preprocessing the data


```r
MyData <- read.csv(file = "activity.csv", stringsAsFactors=FALSE, na.strings="NA")
```


```r
MyData$date <-strptime(MyData$date, "%Y-%m-%d")
```

## What is mean total number of steps taken per day?


```r
plot(MyData$date, MyData$steps, type = "h", main = " Total Number of Daily Steps", ylab = "Daily Number of Steps of Person", xlab = "October and November, 2012")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)


## What is the average daily activity pattern?


```r
avgDailyStps <- sapply(split(MyData$steps, MyData$interval), mean, na.rm=TRUE)
```


```r
 plot(avgDailyStps, type = "l", main = "Average Number of Steps", ylab = "Number of Steps", xlab = "5 min increments")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

## Imputing missing values


```r
trows <- nrow(MyData)
cAvgDailyStps <- rep(avgDailyStps, 61) 
nv <- vector()
leng <- 1:trows
for (i in leng) {
        ifelse (is.na(MyData$steps[i]), v <- cAvgDailyStps[i], v <- MyData$steps[i])
        nv <- c(nv, v)
}
```



```r
procdata <- MyData
procdata$steps <- nv
```

Here is mean total number of steps taken per day with the missing values imputted.


```r
plot(procdata$date, procdata$steps, type = "h", main = "Number Steps Taken Daily", ylab = "Number of steps", xlab = "October and November, 2012")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

```r
meanproc <- mean(procdata$steps)
medianproc <- median(procdata$steps)
```

## Are there differences in activity patterns between weekdays and weekends?


```r
library("timeDate")
time <- procdata$date
weekend <- sapply(as.Date(time), isWeekend)
weekend <- gsub("TRUE", "weekend", weekend)
weekend <- gsub("FALSE", "weekday", weekend)
procdata <- cbind(procdata, weekend)
```


```r
library(lattice)
subd <- subset(procdata, procdata$weekend == "weekday")
sube <- subset(procdata, procdata$weekend == "weekend")
weekday <- sapply(split(subd$steps, subd$interval), mean)
weekend <- sapply(split(sube$steps, sube$interval), mean)
Interval <- names(weekday)
Interval <- as.integer(Interval)
stpswd <- as.numeric(weekday)
tmd <- c("weekday")
tmd <- rep(tmd, 288)
wd <- data.frame(Interval, stpswd, tmd)
stpswe <- as.numeric(weekend)
tme <- c("weekend")
tme <- rep(tme, 288)
we <- data.frame(Interval, stpswe, tme)
colnames(wd) <- c("Interval", "steps", "time")
colnames(we) <- c("Interval", "steps", "time")
data <- rbind(wd, we)
xyplot(steps ~ Interval | time, data = data, type = "l", 
       layout = c(1, 2), ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)