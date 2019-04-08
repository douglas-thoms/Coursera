##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
##
##  File name:  plot1.R
##  Date:       07Apr2019
##
##  Assginment 1 for Reproducible Research course
##  
##
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------

require('plyr')
require('dplyr')
require('zoo')

##----------------------------------------------------------------------------
## Loading and preprocessing the data
##----------------------------------------------------------------------------

rawData <- read.csv("activity.csv")

#rawDataNAremoved <- rawData[!is.na(rawData$steps),]
#rawDataNAremoved$date <- droplevels(rawDataNAremoved$date)

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

meanSteps <- aggregate(x = rawData$steps,
                         by = list(rawData$interval), mean, na.rm = TRUE)
colnames(meanSteps) <- c("interval","mean.steps.per.interval")

plot(y = meanSteps$mean.steps, x= meanSteps$interval, type = "l", 
     xaxt = "n", xlab = "5-min Interval", ylab = "Average Steps", main = "Mean.Steps")

axis(side = 1, at = c(seq(from = 0, to = 2355, by = 60)))


MaxMeanSteps = match(max(meanSteps$mean.steps),
                                meanSteps$mean.steps)

maxInterval = meanSteps$interval[MaxMeanSteps]

##----------------------------------------------------------------------------
## Imputing missing values
##----------------------------------------------------------------------------

NAdataPoints <- length(rawData$steps[is.na(rawData$steps)])

#replacement value will be the 5 minute average of that interval

#first, replace first day with average value

replacedNA <- rawData
replacedNA$steps[1:288] <- meanSteps$mean.steps.per.interval 

#try this 
#replacedNA <- ddply(replacedNA, .(interval), na.locf)
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

medianStepsNAremoved <- median(stepsPerDay$steps.per.day, na.rm = TRUE)
meanStepsNAremoved <- round(mean(stepsPerDay$steps.per.day, na.rm = TRUE),1)

##----------------------------------------------------------------------------
##Are there differences in activity patterns between weekdays and weekends?
##----------------------------------------------------------------------------

