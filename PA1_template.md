# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
# Read input data
indata <- read.csv("activity.csv")

# Convert the interval field into hours and minutes
indata$hours <- trunc( indata$interval/100)
indata$hm <-  indata$interval/100

indata$minutes <- round ( (indata$hm - indata$hours)*100)

# Create a subset excluding records with any missing data.
activity <- indata[complete.cases(indata), ]
```


## What is mean total number of steps taken per day?

```r
# Calculate the total number of steps
agg <- aggregate(activity$steps, activity['date'], sum)

library(ggplot2)

# Histogram of the total number of steps taken each day
qplot(x, data=agg, binwidth = 800,
      main= "Total Number of Steps Taken per Day",
      xlab= "Number of Steps",
      ylab= "Count")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
#calculate and print the mean steps taken per day
mean_daily_steps <- mean(agg$x)
mean_daily_steps
```

```
## [1] 10766.19
```

```r
# Calculate median steps per day
median_daily_steps <- median(agg$x)
median_daily_steps
```

```
## [1] 10765
```



## What is the average daily activity pattern?

```r
# Calculate mean number of steps in each 5-minute interval
agg2 <- aggregate(activity$steps, activity['hm'], mean)

#Create time series plot for mean number of steps by 
qplot(hm, x, data=agg2, geom = "line",
      main= "Average Number of Steps Taken \nin Each 5-Minute Interval ",
      xlab= " Interval ",
      ylab= "Average Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
# find row containing interval with max mean number of steps
rm <- agg2[agg2$x == max(agg2$x) , ]
# Print interval (hh.mm) with maximum average number of steps across all days
rm$hm
```

```
## [1] 8.35
```


## Imputing missing values

```r
#Print the number of rows having NAs for the number of steps
nrow(indata[ is.na(indata$steps) == TRUE , ])
```

```
## [1] 2304
```

```r
# verify that there are no NAs for interval and date
indata[ is.na(indata$interval) == TRUE , ]
```

```
## [1] steps    date     interval hours    hm       minutes 
## <0 rows> (or 0-length row.names)
```

```r
indata[ is.na(indata$date) == TRUE , ]
```

```
## [1] steps    date     interval hours    hm       minutes 
## <0 rows> (or 0-length row.names)
```

```r
# Merge imported dataframe with vector of mean steps for each interval,
# merging by interval, keeping all input rows,
# even if not matched (left join).
m <- merge(indata, agg2, all.indata = TRUE)

# Replace missing values for the number of steps with the
# mean number of steps for the interval
m$steps <- ifelse( is.na(m$steps), m$x, m$steps )
# Check whether there are still any missing values
summary(m$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00    0.00    0.00   37.38   27.00  806.00
```

```r
# Calculate the total number of steps each day,
# using dataset with missing data imputed
agg10 <- aggregate(m$steps, m['date'], sum)

# Histogram of the total number of steps taken each day, 
# with imputed values for missing data.
qplot(x, data=agg10, binwidth = 800,
      main= "Total Number of Steps Taken per Day
      \n(Includes Imputed Values for Missing Data)",
      xlab= "Number of Steps",
      ylab= "Count")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
#calculate and print the mean steps taken per day
mean_daily_steps <- mean(agg10$x)
mean_daily_steps
```

```
## [1] 10766.19
```

```r
# Calculate and print the median steps per day
median_daily_steps <- median(agg10$x)
median_daily_steps
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?

```r
m$act_date <- as.Date(m$date)
m$weekday <- weekdays(m$act_date)
m$weekend <- factor( ifelse(m$weekday %in% c("Saturday", "Sunday"), "Weekend", "Weekday") )

# Calculate mean number of steps in each 5-minute interval
agg20 <- aggregate(m$steps, m[ c('weekend', 'hm')],  mean)

#Create time series plot for mean number of steps 
#by 5-minute interval and weekday/weekend
qplot(hm, x, data=agg20, geom = "line" , facets =  weekend~ .,
      main= "Average Number of Steps Taken \nin Each 5-Minute Interval\nfor Weekdays and Weekend
         \n(Includes Imputed Values for Missing Data)",
      xlab= "Interval ",
      ylab= "Average Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

