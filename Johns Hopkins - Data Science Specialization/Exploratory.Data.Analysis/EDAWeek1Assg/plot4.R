##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
##
##  File name:  plot4.R
##  Date:       09Mar2019
##
##Computes:
## 
## Args: None
##         
##
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------

#check if data.table installed
if(!require(data.table)){
  
  install.packages("data.table")
  
}

if(!require(dplyr)){
  
  install.packages("dplyr")
  
}

library(data.table)
library(dplyr)

#read selective rows of file
inputData.df <- as.data.frame(fread("household_power_consumption.txt", 
                                    header=T, sep=";")[-c(1:66636,69517:2075259),])

#create date time column
#combine date time column and non date data
data.df <- cbind(datetime = strptime(paste(inputData.df$Date, inputData.df$Time), format = '%d/%m/%Y %H:%M:%S')
                 ,select(inputData.df, -Date,-Time))

#coerce energy values into numeric
for (i in 2:7) {data.df[ ,i] <- as.numeric(as.character(data.df[ ,i]))}


#save it to a PNG file with a width of 480 pixels and a height of 480 pixels.
#open device
#Name each of the plot files as \color{red}{\verb|plot1.png|}plot1.png, \color{red}{\verb|plot2.png|}plot2.png, etc.
png(filename = "plot4.png", width = 480, height = 480)

print(dev.cur())

#set 4 charts
par(mfcol = c(2,2))

#construct plot #1
with(data.df,plot(datetime,Global_active_power, type = "n",
                  ylab = "Global Active Power (kilowatts)",
                  xlab = ""))
with(data.df,lines(datetime,Global_active_power))

#Construct the plot #2
with(data.df,plot(datetime,Sub_metering_1, type = "n",
                  ylab = "Energy sub metering",
                  xlab = ""))
with(data.df,lines(datetime,Sub_metering_1))
with(data.df,lines(datetime,Sub_metering_2, col = "Red"))
with(data.df,lines(datetime,Sub_metering_3, col = "Blue"))

legend("topright", legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
       col = c("Black","Red","Blue"), lty=1, bty = "n")

#construct plot #3
with(data.df,plot(datetime, Voltage, type = "n",
                  ylab = "Voltage",
                  xlab = "datetime"))

with(data.df,lines(datetime,Voltage))

#construct plot #4
with(data.df,plot(datetime, Global_reactive_power, type = "n",
                  xlab = "datetime"))

with(data.df,lines(datetime,Global_reactive_power))

#close device
dev.off()
print(dev.cur())