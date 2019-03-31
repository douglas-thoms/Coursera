##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
##
##  File name:  plot3.R
##  Date:       26Mar2019
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

df <- lapply(NEI[,c("fips","SCC","Pollutant","type","year")], as.factor)
df$Emissions <- NEI$Emissions
df <- as.data.frame(df)

#summarize data
graph.data <- df %>%
               filter(fips =="24510") %>%
               group_by(type,year) %>% 
               summarise(total.emission = sum(Emissions,na.rm = TRUE)) %>%
               ungroup()

names(graph.data) <- make.names(names(graph.data))

#save it to a PNG file with a width of 480 pixels and a height of 480 pixels.
#open device
#Name each of the plot files as \color{red}{\verb|plot1.png|}plot1.png, \color{red}{\verb|plot2.png|}plot2.png, etc.
png(filename = "plot3.png", width = 480, height = 480)

print(dev.cur())

#create and plot graph
graph <- ggplot(graph.data,aes(fill = type, x = year, y = total.emission)) +
                  geom_bar(stat = "identity") +
                  xlab("") + ylab("ton") + facet_grid( . ~ type)
                  ggtitle("Baltimore PM2.5 Emissions")
plot(graph)


#close device
dev.off()
print(dev.cur())

