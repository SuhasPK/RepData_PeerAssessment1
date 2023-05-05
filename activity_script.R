library(ggplot2)
library(dplyr)

# loaing the activity.csv data
activity <- read.csv("activity.csv")
View(activity)
dim(activity)
summary(activity)

# What is mean total mean number of steps taken per day?
stepsPerDay <- aggregate(steps ~ date, activity, sum)
# mean and median of step count per day.
mean_StepsPerDay <- round(mean(stepsPerDay$steps),digits = 3)
median_StepsPerDay <- round(median(stepsPerDay$steps), digits = 3)
mean_StepsPerDay
median_StepsPerDay
# plotting
png(filename = "stepsPerDay.png",width=720,height=720,units = "px" )
hist(stepsPerDay$steps, main = "Steps per day", xlab = "Step count", col = "green", breaks = 8)
abline(v = mean_StepsPerDay,col = "red", lty ="dashed" ,lwd = 5 )
text(x = mean_StepsPerDay*1.7, y = 15 , 
     paste("Mean =", mean_StepsPerDay," steps"), col ="red", cex = 1.5 )
abline(v=median_StepsPerDay, col="black",lwd=2)
text(x = median_StepsPerDay*1.7, y = 13,
     paste("Median =", median_StepsPerDay, " steps"), col = "black", cex=1.5)
dev.off()

# What is the average daily activity pattern?
average_interval <- aggregate(steps~interval, activity, mean, na.rm = TRUE)
View(average_interval)
# plotting
png(filename = "average_daily_activity_pattern.png",
    width = 720, height = 500, units = 'px')
plot(x = average_interval$interval, y =average_interval$steps,
     type = "l", xlab = "5 minutes interval", ylab = " Avergae step count",
     main = "Average Daily Activity Pattern", col = "blue")
max_step_interval <- average_interval$interval[which.max(average_interval$steps)]
abline(v= max_step, col = "red", lwd = 1.5)
text(x = 1750, y =200,
     paste("5-minute interval having max average step count = ",max_step_interval),col="red")
dev.off()

# Imputing the missing values
# Calculate and report the total number of missing values in the dataset.
missing_values <- sum(!complete.cases(activity)==TRUE)
missing_values

# Replace NA values by 0 for the 5min interval.
activity[is.na(activity$steps),"steps"] <- mean(activity$steps,na.rm = TRUE)
View(activity)

stepsPerDay_noNA <- aggregate(steps ~ date, activity, sum)
# mean and median of step count per day.
mean_StepsPerDay_noNA <- round(mean(stepsPerDay_noNA$steps),digits = 3)
median_StepsPerDay_noNA <- round(median(stepsPerDay_noNA$steps), digits = 3)
mean_StepsPerDay_noNA
median_StepsPerDay_noNA

# plotting
png(filename = "stepsPerDay_noNA_byMeanSteps.png",width=720,height=720,units = "px" )
hist(stepsPerDay_noNA$steps, main = "Steps per day (NA values are filled by mean(activity steps without NA)  )", xlab = "Step count", col = "green", breaks = 8)
abline(v = mean_StepsPerDay_noNA,col = "red", lty ="dashed" ,lwd = 5 )
text(x = mean_StepsPerDay_noNA*1.7, y = 15 , 
     paste("Mean =", mean_StepsPerDay_noNA," steps"), col ="red", cex = 1.5 )
abline(v=median_StepsPerDay_noNA, col="black",lwd=2)
text(x = median_StepsPerDay_noNA*1.7, y = 13,
     paste("Median =", median_StepsPerDay_noNA, " steps"), col = "black", cex=1.5)
dev.off()

# Are there difference in activity patterns between weekdays and weekends?
activity$day <- as.POSIXlt(activity$date)$wday
activity$dayType <- as.factor(
    ifelse(activity$date == 0 | activity$day == 6, "weekend", "weekday")
)
activity <- subset(activity, select = -c(day))
head(activity)

weekday_data <- activity[activity$dayType =="weekday",]
weekend_data <- activity[activity$dayType == "weekend",]

weekday_stepInterval <- aggregate(steps~interval, weekday_data,mean)
weekend_stepInterval <- aggregate(steps~interval, weekend_data,mean)

png(filename = "dayType_activity.png",
    width=720,height = 720,units='px')
par(mfrow=c(2,1))

plot(weekday_stepInterval, type='l', col='green', main="Weekdays")
plot(weekend_stepInterval, type='l', col="red", main="Weekend")
dev.off()


