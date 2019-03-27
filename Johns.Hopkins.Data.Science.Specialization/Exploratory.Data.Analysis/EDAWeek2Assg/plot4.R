##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
##
##  File name:  plot4.R
##  Date:       27Mar2019
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

coal.logical <- apply(SCC,1,function(row) length(grep("[cC][oO][aA][lL]",row))>0)
coal.SCC <- SCC[coal.logical,]$SCC


bar_graph_data <- df %>%
               filter(SCC %in% coal.SCC) %>%
               group_by(year) %>% 
               summarise(total.emission = sum(Emissions,na.rm = TRUE))
               



