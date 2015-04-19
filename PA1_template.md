# Reproducible Research: Peer Assessment 1

# Data Science / Course: Reproducible Research: Peer Assignment #1
### **To Load and Preprocess the Data**
process and transform the data into a format suitable for analysis by default echo=TRUE

```r
Echo=TRUE
data = read.csv("C:/Users/ALIENWARE/Desktop/Ray.Hw/activity.csv")
```
## **The Average Total Number of Steps Taken Each Day?**
Calculate total number of steps
Calculate and report the mean and median total number of steps taken per day


```r
library(ggplot2)
compute.steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(compute.steps, binwidth=1500, xlab="total number of steps taken each day",ylab="number",color="cyl")
```

![plot of chunk unnamed-chunk-2](PA1_template_files/figure-html/unnamed-chunk-2.png) 

```r
mean(compute.steps, na.rm=TRUE)
```

```
## [1] 9354
```

```r
median(compute.steps, na.rm=TRUE)
```

```
## [1] 10395
```
## **The Average Daily Activity Pattern?**
Make a time series plot which 5 minute interval on average across all the days contains max # of steps

```r
library(ggplot2)
averagedap <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=averagedap, aes(x=interval, y=steps)) +
    xlab("5-minute interval") +
ylab("mean number of steps taken")+geom_line()
```

![plot of chunk unnamed-chunk-3](PA1_template_files/figure-html/unnamed-chunk-3.png) 

```r
averagedap[which.max(averagedap$steps),]
```

```
##     interval steps
## 104      835 206.2
```
## **To Impute Missing Values**
Calculate and report the total number of missing values in the dataset. Missing values are filled with mean value for that 5 minute interval. Make histogram of total number of steps taken each day and calculate and report mean and median

```r
missingvalues <- is.na(data$steps)
table(missingvalues)
```

```
## missingvalues
## FALSE  TRUE 
## 15264  2304
```

```r
complete.value <- function(steps, interval) {
    completed <- NA
    if (!is.na(steps))
        completed <- c(steps)
    else
        completed <- (averagedap[averagedap$interval==interval, "steps"])
    return(completed)
}

completed.data <- data
completed.data$steps <- mapply(complete.value, completed.data$steps, completed.data$interval)
compute.steps <- tapply(completed.data$steps, completed.data$date, FUN=sum)
qplot(compute.steps, binwidth=1500, xlab="total number of steps taken each day",ylab="number",color="cyl")
```

![plot of chunk unnamed-chunk-4](PA1_template_files/figure-html/unnamed-chunk-4.png) 

```r
mean(compute.steps)
```

```
## [1] 10766
```

```r
median(compute.steps)
```

```
## [1] 10766
```
## **Differences in activity patterns between weekdays and weekends?**
Create a new factor variable in the dataset with two levels ¨Cweekday and weekend \newline
Make a panel plot containing a time series of the 5 minute interval and average # of steps taken averaged across all weekday days of weekend days


```r
Weekday_weekend <- function(date) {
    dayoftheweek <- weekdays(date)
    if (dayoftheweek %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (dayoftheweek %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("date is not valid")
}

completed.data$date <- as.Date(completed.data$date)
completed.data$day <- sapply(completed.data$date, FUN=Weekday_weekend)
averagedap <- aggregate(steps ~ interval + day, data=completed.data, mean)
ggplot(averagedap, aes(interval, steps)) + facet_grid(day ~ .) +
xlab("5-minute interval") + ylab("Step count")+geom_line()
```

![plot of chunk unnamed-chunk-5](PA1_template_files/figure-html/unnamed-chunk-5.png) 
