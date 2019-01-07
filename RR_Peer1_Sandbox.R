##Step 1: Read data into data frame activity
library(lubridate)
library(ggplot2)
setwd("/home/scott/R/RepData_PeerAssessment1")
unzip("activity.zip")
activity<-read.csv("activity.csv")

##Step 2: Sum steps by date and plot Histogram
totalSteps<-aggregate(steps ~ date, activity, sum)
hist(totalSteps$steps, 
     main="Total Number of Steps Taken per Day", 
     xlab="Number of Steps",
     col="red")
dev.copy(png, file="plot2.png")
dev.off()

##Step 3: Determine Mean and Median # of steps per day
meanSteps<-mean(totalSteps$steps)
medianSteps<-median(totalSteps$steps)

meanSteps
medianSteps

##Step 4: Time Series plot of Average # of Steps taken

averageSteps<-aggregate(steps ~ interval, activity, mean)
plot(averageSteps$steps~averageSteps$interval, 
     ylab="Average Number of Steps",
     xlab="Interval",
     type="l")

dev.copy(png, file="plot4.png")
dev.off()

##Step 5: Determine the 5 minute interval that on average contains the maximum number of steps

maxSteps<-averageSteps[which.max(averageSteps$steps),]
maxSteps$interval

##Step 6: Code to describe and show strategy for imputing missing data
countNA<-sum(is.na(activity$steps))
countNA

#use average number of steps for interval to impute NA

imputeSteps<-transform(activity, steps = ifelse(is.na(activity$steps), averageSteps$steps, activity$steps))

#Step 7: Plot histogram of total number of steps after imputing missing values
totalSteps_imputed<-aggregate(steps ~ date, imputeSteps, sum)
hist(totalSteps_imputed$steps, 
     main="Total Number of Steps Taken per Day", 
     xlab="Number of Steps",
     col="blue")
dev.copy(png, file="plot7.png")
dev.off()
#Step 8: Add additional column to dataframe identifying the day of the week

activity$day<-sapply(activity$date, function(x) {
        if(weekdays(as.Date(x)) == "Saturday" | weekdays(as.Date(x)) == "Sunday")
        {y <- "Weekend"} else
        {y <- "Weekday"}
})

#Panel plot comparing average number of steps taken per 5-minute interval across weekends vs weekday

steps_by_day<-aggregate(steps~interval + day, activity, mean, na.rm = TRUE)
ggplot(steps_by_day, aes(x = interval, y = steps, color = day)) +
        geom_line() +
        labs(title = "Average daily steps by weekday/weekend", x = "Interval", y = "Average # of Steps") +
        facet_wrap(~day, ncol = 1, nrow = 2)
dev.copy(png, file="plot8.png")
dev.off()
