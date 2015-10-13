# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

We extract the .csv file from the zip and adjust the column "date" withe a Date format:

```r
DF <- read.csv(unz("activity.zip", "activity.csv"), colClasses=c("integer", "Date", "integer"))
```
## What is mean total number of steps taken per day?

First we select the data we need and group it per date. Then we sum the steps for each day:


```r
library(dplyr)
stepsDay<-DF %>%
        select(steps, date) %>%
        group_by(date) %>%
        summarise(sumSteps=sum(steps))
```

Now we prepae the histogram of the total number of steps taken each day:
(we adjust the bindwith to 1.000 steps)


```r
library(ggplot2)
ggplot(data=stepsDay, aes(sumSteps)) + 
        geom_histogram(binwidth = 1000) +
        labs(title="Steps taken each day", 
             x = "Number of steps each day", y = "Number of times in a day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

Now we calculate the **mean** and **median** total number of steps taken per day with this code 

```r
stepsMean   <- mean(stepsDay$sumSteps, na.rm=TRUE)
stepsMedian <- median(stepsDay$sumSteps, na.rm=TRUE)
```

The result:
The **mean** is 10766  
The **median** is 10765    

## What is the average daily activity pattern?

1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?



## Imputing missing values
Note that there are a number of days/intervals where there are missing
values (coded as `NA`). The presence of missing days may introduce
bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

4. Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?



## Are there differences in activity patterns between weekdays and weekends?
For this part the `weekdays()` function may be of some help here. Use
the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

1. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using **simulated data**:
