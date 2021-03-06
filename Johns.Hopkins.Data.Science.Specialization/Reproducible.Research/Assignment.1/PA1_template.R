##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
##
##  File name:  PA1_template.R
##  Date:       07Apr2019
##
##  Assignment 1 for Reproducible Research course
##  
##
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------

require('plyr')
require('dplyr')
require('zoo')
require('lubridate')
require('lattice')

##----------------------------------------------------------------------------
## Loading and preprocessing the data
##----------------------------------------------------------------------------

rawData <- read.csv("activity.csv")

##----------------------------------------------------------------------------
## What is mean total number of steps taken per day?
##----------------------------------------------------------------------------

stepsPerDay <- aggregate(x = rawData$steps,
                           by = list(rawData$date), sum)
colnames(stepsPerDay) <- c("date","steps.per.day")

hist(stepsPerDay$steps.per.day, breaks = seq(from = 0, to = 25000, by = 1250), 
     xlab = "", main = "Steps per day")

medianSteps <- median(stepsPerDay$steps.per.day, na.rm = TRUE)
meanSteps <- round(mean(stepsPerDay$steps.per.day, na.rm = TRUE),1)

##----------------------------------------------------------------------------
## What is the average daily activity pattern?
##----------------------------------------------------------------------------

meanStepsInterval <- aggregate(x = rawData$steps,
                         by = list(rawData$interval), mean, na.rm = TRUE)
colnames(meanStepsInterval ) <- c("interval","mean.steps.per.interval")

plot(y = meanStepsInterval $mean.steps, x= meanStepsInterval $interval, type = "l", 
     xaxt = "n", xlab = "5-min Interval", ylab = "Average Steps", main = "Mean.Steps")

axis(side = 1, at = c(seq(from = 0, to = 2355, by = 60)))

MaxmeanStepsInterval <- match(max(meanStepsInterval $mean.steps),
                                meanStepsInterval $mean.steps)

maxInterval <- meanStepsInterval$interval[MaxmeanStepsInterval ]

##----------------------------------------------------------------------------
## Imputing missing values
##----------------------------------------------------------------------------

NAdataPoints <- length(rawData$steps[is.na(rawData$steps)])

#replacement value will be the 5 minute average of that interval

#first, replace first day with average value

replacedNA <- rawData
replacedNA$steps[1:288] <- meanStepsInterval $mean.steps.per.interval 

replacedNA <- ddply(replacedNA, .(interval), transform, steps=na.approx(steps, rule=2))
replacedNA <- replacedNA[order(replacedNA$date,replacedNA$interval), ]

stepsPerDayNAremoved <- aggregate(x = replacedNA$steps,
                         by = list(replacedNA$date), sum)
colnames(stepsPerDayNAremoved) <- c("date","steps.per.day")

par(mfrow= c(1,2))

hist(stepsPerDay$steps.per.day, breaks = seq(from = 0, to = 25000, by = 1250), 
     xlab = "", main = "Steps per day", ylim = c(0,14))

hist(stepsPerDayNAremoved$steps.per.day, 
     breaks = seq(from = 0, to = 25000, by = 1250), xlab = "", 
     main = "Steps per day NA removed", ylim = c(0,14))

medianStepsNAremoved <- median(stepsPerDayNAremoved$steps.per.day, 
                               na.rm = TRUE)
meanStepsNAremoved <- round(mean(stepsPerDayNAremoved$steps.per.day, 
                                 na.rm = TRUE),1)

##----------------------------------------------------------------------------
##Are there differences in activity patterns between weekdays and weekends?
##----------------------------------------------------------------------------


replacedNA$date <- ymd(replacedNA$date)


weekday.weekend.data <- replacedNA %>%
        mutate(weekday.weekend = ifelse((weekdays(date) == "Saturday")
                                        | (weekdays(date) == "Sunday"),
                                        "weekend", "weekday"))

weekday.weekend.data <- aggregate(x = weekday.weekend.data$steps,
                                  by = list(weekday.weekend.data$interval, 
                                            weekday.weekend.data$weekday.weekend), 
                                  mean, na.rm = TRUE)
colnames(weekday.weekend.data) <- c("interval", "weekday.weekend",
                                      "mean.steps.per.interval")


print(xyplot(mean.steps.per.interval ~ interval | factor(weekday.weekend), 
       data=weekday.weekend.data, type = 'l', layout = c(1,2),
       xlab = '5-min Interval', ylab = 'Mean Steps', 
       main = "Mean Steps per 5-min interval"))

