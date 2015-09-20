

## Loading and preprocessing the data

setwd("E:\\Users\\Andrea\\Downloads\\Data\\Reproducible Research\\RepData_PeerAssessment1")
unzip(zipfile="activity.zip")
activity <- read.csv("activity.csv", colClasses = c("numeric", "character", 
                                              "numeric"))
# cast $date as date
activity$date <- as.Date(activity$date, "%Y-%m-%d")

#
activity$day<-weekdays(activity$date)

for (i in 1:nrow(activity)) {
    if ((activity$day[i] == "sabato") | (activity$day[i] == "domenica")){
        activity$daytype[i] <- "Weekend"
    } else {
        activity$daytype[i] <- "Weekday"
    }
}

#check
head(activity)


## What is mean total number of steps taken per day?

totalXday <- aggregate(activity$steps, by=list(activity$date), FUN=sum, na.rm=TRUE)

names(totalXday) <- c("Date", "Total")
totalXday$Total<-as.numeric(totalXday$Total)

summary(totalXday)

hist(totalXday$Total, 
     breaks=10, 
     xlab="Total number of steps", 
     col = "green", 
     main = "Histogram of Total Steps per Day")

mean   <- mean(totalXday$Total, na.rm=TRUE)
median <- median(totalXday$Total, na.rm=TRUE)

## What is the average daily activity pattern?

mean <- aggregate(activity$steps, 
                       by=list(activity$interval), 
                       FUN=mean, 
                       na.rm=TRUE)

time_series <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)


plot(row.names(time_series), time_series, type = "l", xlab = "Interval", 
     ylab = "Average number of steps", main = "Average Daily Activity Pattern", 
     col = "blue")

max_interval <- which.max(time_series)
max_int <- names(max_interval)


## Imputing missing values

daily_mean <- aggregate(activity$steps , list(activity$day), mean , na.rm=TRUE)

names(daily_mean) <- c("Day", "Mean")

daily_mean

n_activity <- activity

for (i in 1:nrow(n_activity)) {
    if (is.na(n_activity$steps[i])){
        n_activity$steps[i]<- daily_mean$Mean[daily_mean$Day==n_activity$day[i]]
    }
    
}

sum(is.na(activity$steps))


n_totalXday <- aggregate(n_activity$steps, by=list(n_activity$date), FUN=sum, na.rm=TRUE)

names(n_totalXday) <- c("Date", "Total")
n_totalXday$Total<-as.numeric(n_totalXday$Total)

summary(n_totalXday)

hist(totalXday$Total, 
     breaks=10,      
     col=rgb(0.1,0.1,0.1,0.5),
     )

hist(n_totalXday$Total, 
     breaks=10,      
     col=rgb(0.8,0.8,0.8,0.5),
     add=T
     )


n_mean   <- mean(n_totalXday$Total, na.rm=TRUE)
n_median <- median(n_totalXday$Total, na.rm=TRUE)


## Are there differences in activity patterns between weekdays and weekends?

library(lattice)

mean_by_day <- aggregate(activity$steps, 
                       by=list(activity$daytype, 
                               activity$day, activity$interval), mean, na.rm = TRUE)

names(mean_by_day) <- c("daytype", "weekday", "interval", "mean")

head(mean_by_day)

xyplot(mean ~ interval | daytype, mean_by_day, 
       type="l", 
       lwd=1, 
       xlab="Interval", 
       ylab="Number of steps", 
       layout=c(1,2))






library(lattice)

means_by_weekdays <- aggregate(activity$steps, 
                               by=list(activity$daytype, 
                                       activity$day, 
                                       activity$interval), mean)

names(means_by_weekdays) <- c("daytype", "weekday", "interval", "mean")


xyplot(mean ~ interval | daytype, means_by_weekdays, 
       type="l", 
       lwd=1, 
       xlab="Interval", 
       ylab="Number of steps", 
       layout=c(1,2))


