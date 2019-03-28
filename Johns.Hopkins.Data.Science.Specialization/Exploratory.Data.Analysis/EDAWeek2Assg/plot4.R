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

coal.logical <- apply(SCC,1,function(row) length(grep("([cC][oO][mM][bB]).*([cC][oO][aA][lL])",row))>0)
coal.SCC <- SCC[coal.logical,]$SCC


scatter.graph.data <- df %>%
               filter(SCC %in% coal.SCC) #%>%

bar.graph.data <- df %>%
               filter(SCC %in% coal.SCC) %>%
               group_by(year) %>% 
               summarise(total.emission = sum(Emissions, na.rm = TRUE),
               median.emission = median(Emissions, na.rm = TRUE),
               mean.emission = mean(Emissions, na.rm = TRUE),
               percent.zero = mean(Emissions == 0))

#do a chart of percentage of zero values


#set 4 charts
par(mfcol = c(1,2))

#create bar graph
               
y <- bar.graph.data$total.emission
x <- bar.graph.data$year

barplot(y, names.arg=x, xlab = "Year", ylab = "Total PM2.5 Emissions (ton)")
abline(h = min(bar.graph.data$total.emission))

# do full box plot
boxplot(scatter.graph.data$Emissions ~ scatter.graph.data$year, ylim = c(0,6))
abline(h = 0)

#do chart of median

#do chart of mean

