##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
##
##  File name:  plot1.R
##  Date:       26Mar2019
##
##  Part of Course Project 2 for Exploratory data course
##      
##
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------

#Have total emissions from PM2.5 decreased in the United States from 
#1999 to 2008? Using the base plotting system, make a plot showing 
#the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

require(dplyr)

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

df <- lapply(NEI[,c("fips","SCC","Pollutant","type","year")], as.factor)
df$Emissions <- NEI$Emissions
df <- as.data.frame(df)

graph_data <- df %>%
               group_by(year) %>%
               summarise(total.emission = sum(Emissions,na.rm = TRUE))

y <- graph_data$total.emission
x <- graph_data$year

barplot(y, names.arg=x, xlab = "Year", ylab = "Total PM2.5 Emissions (ton)")

