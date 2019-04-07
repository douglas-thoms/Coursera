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

require('dplyr')

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
colnames(stepsPerDay) <- c("date","steps")

hist(stepsPerDay$steps, breaks = seq(from = 0, to = 25000, by = 1250), 
     xlab = "", main = "Steps per day")

medianSteps <- median(stepsPerDay$steps, na.rm = TRUE)
meanSteps <- round(mean(stepsPerDay$steps, na.rm = TRUE),1)

##----------------------------------------------------------------------------
## What is the average daily activity pattern?
##----------------------------------------------------------------------------