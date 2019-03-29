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

graph.data <- df %>%
               filter(fips =="24510") %>%
               group_by(type,year) %>% 
               summarise(total.emission = sum(Emissions,na.rm = TRUE)) %>%
               spread(type,total.emission)

names(graph.data) <- make.names(names(graph.data))

#save it to a PNG file with a width of 480 pixels and a height of 480 pixels.
#open device
#Name each of the plot files as \color{red}{\verb|plot1.png|}plot1.png, \color{red}{\verb|plot2.png|}plot2.png, etc.
png(filename = "plot3.png", width = 480, height = 480)

print(dev.cur())

NON.ROAD <- ggplot(graph.data,aes(x = graph.data$year, y = graph.data$NON.ROAD)) +
                  geom_bar(stat = "identity") +
                  xlab("") + ylab("ton") + 
                  ggtitle("Non.road") +
                  geom_hline(yintercept = min(graph.data$NON.ROAD))

NONPOINT <- ggplot(graph.data,aes(x = graph.data$year, y = graph.data$NONPOINT)) +
                  geom_bar(stat = "identity") +
                  xlab("") + ylab("ton") + 
                  ggtitle("Nonpoint") +
                  geom_hline(yintercept = min(graph.data$NONPOINT))

ON.ROAD <- ggplot(graph.data,aes(x = graph.data$year, y = graph.data$ON.ROAD)) +
                  geom_bar(stat = "identity") +
                  xlab("") + ylab("ton") + 
                  ggtitle("On.road") +
                  geom_hline(yintercept = min(graph.data$ON.ROAD))

POINT <- ggplot(graph.data,aes(x = graph.data$year, y = graph.data$POINT)) +
                  geom_bar(stat = "identity") +
                  xlab("") + ylab("ton") + 
                  ggtitle("Point") +
                  geom_hline(yintercept = min(graph.data$POINT))

grid.arrange(NON.ROAD, NONPOINT, ON.ROAD, POINT,
             layout_matrix = rbind(c(1,2),c(3,4)), top = "Baltimore Total PM2.5 Emissions")



#close device
dev.off()
print(dev.cur())

