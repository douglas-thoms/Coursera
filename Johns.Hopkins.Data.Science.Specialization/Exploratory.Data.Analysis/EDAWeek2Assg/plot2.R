##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
##
##  File name:  plot2.R
##  Date:       26Mar2019
##
##  Part of Course Project 2 for Exploratory data course
##      
##
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------

#Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
#(fips == "24510") from 1999 to 2008? 
#Use the base plotting system to make a plot answering this question.

require(dplyr)

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

df <- lapply(NEI[,c("fips","SCC","Pollutant","type","year")], as.factor)
df$Emissions <- NEI$Emissions
df <- as.data.frame(df)

#summarize values and filter for Baltimore
graph_data <- df %>%
               group_by(year) %>%
               filter(fips =="24510") %>%
               summarise(total.emission = sum(Emissions,na.rm = TRUE))

#make graph
y <- graph_data$total.emission
x <- graph_data$year

options(scipen = 999)

#save it to a PNG file with a width of 480 pixels and a height of 480 pixels.
#open device
#Name each of the plot files as \color{red}{\verb|plot1.png|}plot1.png, \color{red}{\verb|plot2.png|}plot2.png, etc.
png(filename = "plot2.png", width = 480, height = 480)

print(dev.cur())

barplot(y/1000, names.arg=x, xlab = "", ylab = "kiloton",
        main = "Baltimore Total PM2.5 Emissions")
abline(h = min(graph_data$total.emission))

#close device
dev.off()
print(dev.cur())
