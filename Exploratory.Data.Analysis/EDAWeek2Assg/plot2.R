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

graph_data <- df %>%
               group_by(year) %>%
               filter(fips =="24510") %>%
               summarise(total.emission = sum(Emissions,na.rm = TRUE))

y <- graph_data$total.emission
x <- graph_data$year

barplot(y, names.arg=x, xlab = "Year", ylab = "Baltimore Total PM2.5 Emissions (ton)")
abline(h = min(graph_data$total.emission))
