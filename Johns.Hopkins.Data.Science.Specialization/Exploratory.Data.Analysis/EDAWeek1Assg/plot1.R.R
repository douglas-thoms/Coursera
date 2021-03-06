##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
##
##  File name:  plot1.R
##  Date:       09Mar2019
##
##         
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
data.df <- cbind(Date = strptime(paste(inputData.df$Date, inputData.df$Time), 
                                 format = '%d/%m/%Y %H:%M:%S'),
                                select(inputData.df, -Date,-Time))

#coerce energy values into numeric
for (i in 2:7) {data.df[ ,i] <- as.numeric(as.character(data.df[ ,i]))}

#save it to a PNG file with a width of 480 pixels and a height of 480 pixels.
#open device
#Name each of the plot files as \color{red}{\verb|plot1.png|}plot1.png, \color{red}{\verb|plot2.png|}plot2.png, etc.
png(filename = "plot1.png", width = 480, height = 480)

print(dev.cur())


#Construct the plot
hist(data.df$Global_active_power, col = "Red", 
     xlab = "Global Active Power (kilowatts)", main = "Global Active Power")

#close device
dev.off()
print(dev.cur())