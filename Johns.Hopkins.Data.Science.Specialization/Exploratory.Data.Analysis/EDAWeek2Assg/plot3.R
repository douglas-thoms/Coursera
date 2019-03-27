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

graph_data <- df %>%
               filter(fips =="24510") %>%
               group_by(type,year) %>% 
               summarise(total.emission = sum(Emissions,na.rm = TRUE)) %>%
               spread(type,total.emission)

names(graph_data) <- make.names(names(graph_data))

NON.ROAD <- ggplot(graph_data,aes(x = graph_data$year, y = graph_data$NON.ROAD)) +
                  geom_bar(stat = "identity") +
                  xlab("") + ylab("Baltimore Total PM2.5 Emissions (ton)") + 
                  ggtitle("NON-ROAD") +
                  geom_hline(yintercept = min(graph_data$NON.ROAD))

NONPOINT <- ggplot(graph_data,aes(x = graph_data$year, y = graph_data$NONPOINT)) +
                  geom_bar(stat = "identity") +
                  xlab("") + ylab("Baltimore Total PM2.5 Emissions (ton)") + 
                  ggtitle("NONPOINT") +
                  geom_hline(yintercept = min(graph_data$NONPOINT))

ON.ROAD <- ggplot(graph_data,aes(x = graph_data$year, y = graph_data$ON.ROAD)) +
                  geom_bar(stat = "identity") +
                  xlab("") + ylab("Baltimore Total PM2.5 Emissions (ton)") + 
                  ggtitle("ON.ROAD") +
                  geom_hline(yintercept = min(graph_data$ON.ROAD))

POINT <- ggplot(graph_data,aes(x = graph_data$year, y = graph_data$POINT)) +
                  geom_bar(stat = "identity") +
                  xlab("") + ylab("Baltimore Total PM2.5 Emissions (ton)") + 
                  ggtitle("POINT") +
                  geom_hline(yintercept = min(graph_data$POINT))

grid.arrange(NON.ROAD, NONPOINT, ON.ROAD, POINT,
             layout_matrix = rbind(c(1,2),c(3,4)))


