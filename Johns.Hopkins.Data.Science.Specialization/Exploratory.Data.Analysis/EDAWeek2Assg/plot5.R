##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
##
##  File name:  plot5.R
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

#create vector to see what SCC codes apply to these areas
LA.Baltimore.logical <- (subset(df, (df$fips == "06037") | (df$fips == "24510")))$SCC
LA.Baltimore.SCC <- SCC[LA.Baltimore.logical,]

vehicle.SCC <- filter(SCC, Data.Category == "Onroad")
vehicle.logical <- apply(vehicle.SCC,1,function(row) 
                          length(grep("vehicle|vehicles|trucks|buses",
                          row, ignore.case = TRUE))>0)


vehicle.SCC <- vehicle.SCC[vehicle.logical,]$SCC

#level2 vehicle
#level3 vehicle truck

scatter.graph.data <- df %>%
               filter(SCC %in% vehicle.SCC, fips == "24510")

bar.graph.data <- df %>%
               filter(SCC %in% vehicle.SCC, fips == "24510") %>%
               group_by(year) %>% 
               summarise(total.emission = sum(Emissions, na.rm = TRUE),
               median.emission = median(Emissions, na.rm = TRUE),
               mean.emission = mean(Emissions, na.rm = TRUE),
               percent.zero = mean(Emissions == 0))

#set 4 charts
par(mfrow = c(2,2))

#bar graph - total
barplot(bar.graph.data$total.emission, names.arg=bar.graph.data$year, 
        ylab = "Total PM2.5 Emissions (ton)")
title("Total Emissions")

# scatter plot
plot(y = scatter.graph.data$Emissions, x = scatter.graph.data$year, pch = 1)
title("Spread of readings")

#do chart of mean
barplot(bar.graph.data$mean.emission, names.arg=bar.graph.data$year, 
        ylab = "Mean PM2.5 Emissions (ton)")
title("Mean of Emissions")

#do chart of median
barplot(bar.graph.data$median.emission, names.arg=bar.graph.data$year, 
        ylab = "Median PM2.5 Emissions (ton)")
title("Median of Emissions")

