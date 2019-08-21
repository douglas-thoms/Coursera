##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
##
##  File name:  HOEP.ARIMA.analysis.R
##  Date:       June2019
##
##  
##
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------

session.info <- sessionInfo()

##----------------------------------------------------------------------------
#Libraries
##----------------------------------------------------------------------------

library(dplyr)
library(forecast)

##----------------------------------------------------------------------------
#Data Processing and acquiring
##----------------------------------------------------------------------------
#Acquire and clean data for forecast and actual HOEP price 2017-2018
if(!exists("PredispatchData")){
        source("data_cleaning.R")
}

#Create raw data with since variable and remove leap year day in 2016

leap.day <- grepl("(2016-02-29).*",DispatchData$TimestampHS)

DispatchData <- DispatchData %>%
        select(TimestampHS,HOEP)
        
DispatchData <- DispatchData[leap.day == FALSE,]

#Create time series
tsData <- ts(DispatchData$HOEP, start = c(2017, 1), frequency = 8760)


#Look at Outliers
boxplot(DispatchData$HOEP)
tsCleanedData <- tsclean(tsData)
boxplot(tsCleanedData)
plot(tsCleanedData, ylab = 'HOEP')

#shift to get ride of negative values
tsCleanedData <- tsCleanedData + 100
boxplot(tsCleanedData)

print(summary(tsCleanedData))
print(summary(DispatchData$HOEP))
print(fivenum(tsCleanedData))
print(fivenum(DispatchData$HOEP))

plot(diff(tsCleanedData), ylab='Difference HOPE')
plot(log10(tsCleanedData),ylab='Log (HOEP)')
plot(diff(log10(tsCleanedData)),ylab='Differenced Log (HOEP)')

par(mfrow = c(1,2))
acf(ts(diff(log10(tsCleanedData))),main='HOEP')
pacf(ts(diff(log10(tsCleanedData))),main='HOEP')

ARIMAfit = auto.arima(log10(tsCleanedData), approximation=FALSE,trace=FALSE)
summary(ARIMAfit)