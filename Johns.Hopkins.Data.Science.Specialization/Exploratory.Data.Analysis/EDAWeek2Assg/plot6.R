##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
##
##  File name:  plot6.R
##  Date:       28Mar2019
##
##  Part of Course Project 2 for Exploratory data course
##      
##
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------

#Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
#which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
#Which have seen increases in emissions from 1999–2008? 
#Use the ggplot2 plotting system to make a plot answer this question.

require(tidyr)
require(ggplot2)
require(dplyr)
require(gridExtra)

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

df <- as.data.frame(lapply(NEI[,c("fips","SCC","Pollutant","type","year")], as.factor))
df$Emissions <- NEI$Emissions

#need to find list of coal-related SCC to filter NEI
#do grep search for coal (upper and lower), take vector and take the relevant SCC

#UPDATE TO FUEL Comb * COAL

vehicle.logical <- apply(SCC,1,function(row) length(grep("motor vehicle",row, ignore.case = TRUE))>0)
vehicle.SCC <- SCC[vehicle.logical,]$SCC


scatter.graph.data <- df %>%
               filter(SCC %in% vehicle.SCC)

bar.graph.data <- df %>%
               filter(SCC %in% vehicle.SCC, fips == (24510 | )) 


#bar graph - total
barplot(bar.graph.data$Emissions, names.arg=bar.graph.data$year, 
        ylab = "ton")
title("Baltimore Motor Vehicle PM2.5 Emissions")
abline(h = min(bar.graph.data$Emissions))

