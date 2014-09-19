# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data
Load the data from the activity.zip...


```r
data <- read.table(unz("activity.zip", "activity.csv"), header=T, quote="\"", sep=",")
## convert the dates from factors to dates
data$date <- as.Date(data$date)
```
### Summary() of the data loaded

```
##      steps            date               interval   
##  Min.   :  0.0   Min.   :2012-10-01   Min.   :   0  
##  1st Qu.:  0.0   1st Qu.:2012-10-16   1st Qu.: 589  
##  Median :  0.0   Median :2012-10-31   Median :1178  
##  Mean   : 37.4   Mean   :2012-10-31   Mean   :1178  
##  3rd Qu.: 12.0   3rd Qu.:2012-11-15   3rd Qu.:1766  
##  Max.   :806.0   Max.   :2012-11-30   Max.   :2355  
##  NA's   :2304
```

### Str() of the data loaded

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day?
1. Make a histogram of the total number of steps taken each day
2. Calculate and report the mean and median total number of steps taken per day


```r
##Make a histogram of the total number of steps taken each day
totalDailySteps <- tapply(data$steps, data$date, sum, na.rm = TRUE)
totalDailyStepsDF <- melt(totalDailySteps, varnames=c("date"), value.name="steps")
hist(totalDailyStepsDF$value, main = "Total Daily Steps", breaks=61,xaxt="n", xlab = "Day")
```

![plot of chunk hist_total](figure/hist_total.png) 


```r
## calculate the mean daily steps...
meanDailySteps <- tapply(data$steps, data$date, mean, na.rm = TRUE)
## ... the tapply created an array. convert this to a dataframe
meanDailyStepsDF <- melt( meanDailySteps, varnames=c("date"), value.name="steps")
meanDailyStepsDF$date <- as.Date(meanDailyStepsDF$date)
## !bug in melt currently ignoring value.name!
## ... so set the column names as desired
colnames(meanDailyStepsDF) <- c("date", "steps")
```
The **mean** number of steps per day = 37.3826.
The **median** number of steps per day = 37.3785

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
## calculate the average daily activity per interval
averageActivity <- tapply(data$steps, data$interval, mean, na.rm = TRUE)
## ... the tapply created an array. convert this to a dataframe
averageActivityDF <- melt(averageActivity, varnames=c("interval"), value.name="meanActivity")
## ... so set the column names as desired
colnames(averageActivityDF) <- c("interval", "meanActivity")
```
Here's the time series plot...


```r
plot(meanActivity ~ interval, averageActivityDF, xaxt = "n", type = "l")
```

![plot of chunk timeseriesplot](figure/timeseriesplot.png) 

Now look for the interval with the maximum number of steps...

```r
# reorganise the data so that we can see the interval with the average maximum number of steps
avMaxSteps_DF <- aggregate(interval ~ meanActivity, averageActivityDF, max)
```

The **interval** with the average maximum number of steps is interval 835 with 206.1698 steps

Note that there are some missing datapoints due to missing data in the original data set.

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

###calculate the number of missing values

```r
## find where the missing values are.
## lets work on a copy of data called data2
data2 <- data
missingData <- is.na(data2)
```
There are 2304 records with NA or missing values out of 52704 records.

###Strategy for imputing the values
In order to impute the missing values I have chosen to calculate the mean value per interval per day of the week and use this as a substitute for the NAs.


```r
## calculate the mean per period per day of the week

data2 <- cbind(data2, as.POSIXlt(data2$date)$wday)
colnames(data2) <- c(colnames(data2[1:3]),"dow")

meanDOWIntervalSteps <- tapply(data2$steps, data2[,3:4], mean, na.rm = TRUE)
## this is a matrix with the means per day of the week and per interval
## which can then be used to impute missing values.

head(meanDOWIntervalSteps)

## to use this I will convert it to a dataframe...
meanDOWIntervalStepsDF <-as.data.frame.table(meanDOWIntervalSteps)
```

###Fill in the missing data

```r
## substitute the missing values with the calculated mean values
## I can only think of a FOR loop to do the impute - will check out alternatives later

for (i in 1:nrow(data2)) {

        if (is.na(data2$steps[i])) {
                # include the value from the row in the dataframe meanDOWIntervalStepsDF
                #       where the interval = data2$interval[i]
                #       and the dow = data2$dow[i]
                data2$steps[i] <- meanDOWIntervalStepsDF[meanDOWIntervalStepsDF$dow == data2$dow[i] & meanDOWIntervalStepsDF$interval == data2$interval[i], 3]
        }
}

## create the new dataset
meanDailySteps2 <- tapply(data2$steps, data2$date, mean, na.rm = TRUE)
## ... the tapply created an array. convert this to a dataframe
meanDailyStepsDF2 <- melt( meanDailySteps2, varnames=c("date"), value.name="steps")
meanDailyStepsDF2$date <- as.Date(meanDailyStepsDF2$date)

## !bug in melt currently ignoring value.name!
## ... so set the column names as desired
colnames(meanDailyStepsDF2) <- c("date", "steps")
```

###Show the difference with the imputed values

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
##Make a histogram of the updated total number of steps taken each day
totalDailySteps2 <- tapply(data2$steps, data2$date, sum, na.rm = TRUE)
totalDailySteps2DF <- melt(totalDailySteps2, varnames=c("date"), value.name="steps")
hist(totalDailySteps2DF$value, main = "Total Daily Steps 2", breaks=61,xaxt="n", xlab = "Day")
```

![plot of chunk hist_total2](figure/hist_total2.png) 

Compare | Mean | Median
------- | -------------- | -----------------
Original Value | 37.3826 | 37.3785
New Imputed Value | 37.5736 | 38.2465

Imputing the missing values has made a diffierence to both the mean and the median values.

## Are there differences in activity patterns between weekdays and weekends?
The following plot shows that there is a difference between activity patterns at the weekend to those during the week.


```r
## make a logical vector to determine whether the date is a weken or not.
## and use that logical vector to add a factor of weekday or weekend to the data

isWeekend <- weekdays(data$date) %in% c("Saturday", "Sunday")
data <- cbind(data,factor(isWeekend, levels=c(FALSE, TRUE), labels=c("weekday", "weekend")))
colnames(data) <- c(colnames(data[1:3]),"daytype")

## calcualte the average daily activity pattern for both weekdays and weekends
averageActivity <- tapply(data$steps, data[,3:4], mean, na.rm = TRUE)
averageActivityDF <- melt(averageActivity, varnames=c("interval"), value.name="meanActivity")
colnames(averageActivityDF) <- c("interval", "type", "averageActivity")

## now plot the data in two panels
gWD <- ggplot(averageActivityDF, aes(x=interval))
gWD <- gWD + geom_point(aes(y=averageActivity, color=type, group=type))
gWD <- gWD + geom_line(aes(y=averageActivity, color=type, group=type))
gWD <- gWD + labs(title = "Average Daily Activity Pattern - Weekday versus Weekend")
gWD <- gWD + xlab("Interval")
gWD <- gWD + ylab("Mean Activity")
gWD <- gWD + facet_grid(type ~ .)
print(gWD)
```

![plot of chunk weekends](figure/weekends.png) 
