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
require(dplyr)

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

df <- as.data.frame(lapply(NEI[,c("fips","SCC","Pollutant","type","year")], as.factor))
df$Emissions <- NEI$Emissions

#need to find list of coal-related SCC to filter NEI
#do grep search for coal (upper and lower), take vector and take the relevant SCC

#UPDATE TO FUEL Comb * COAL

coal.logical <- apply(SCC,1,function(row) length(grep("comb.*coal",row, ignore.case = TRUE))>0)
coal.SCC <- SCC[coal.logical,]$SCC


scatter.graph.data <- df %>%
               filter(SCC %in% coal.SCC)

bar.graph.data <- df %>%
               filter(SCC %in% coal.SCC) %>%
               group_by(year) %>% 
               summarise(total.emission = sum(Emissions, na.rm = TRUE),
               median.emission = median(Emissions, na.rm = TRUE),
               mean.emission = mean(Emissions, na.rm = TRUE),
               percent.zero = mean(Emissions == 0))

#save it to a PNG file with a width of 480 pixels and a height of 480 pixels.
#open device
#Name each of the plot files as \color{red}{\verb|plot1.png|}plot1.png, \color{red}{\verb|plot2.png|}plot2.png, etc.
png(filename = "plot4.png", width = 480, height = 480)

print(dev.cur())

#set 4 charts
par(mfrow = c(2,3), oma = c(0,0,2,0))

#bar graph - total
barplot(bar.graph.data$total.emission, names.arg=bar.graph.data$year, 
        ylab = "Total PM2.5 Emissions (ton)")
title("Total Emissions")
abline(h = min(bar.graph.data$total.emission))

# scatter plot
plot(y = scatter.graph.data$Emissions, x = scatter.graph.data$year, pch = 1,
     ylab = "ton")
title("Spread of readings")

#do chart of mean
barplot(bar.graph.data$mean.emission, names.arg=bar.graph.data$year, 
        ylab = "Mean PM2.5 Emissions (ton)")
title("Mean of Emissions")

#do chart of median
barplot(bar.graph.data$median.emission, names.arg=bar.graph.data$year, 
        ylab = "Median PM2.5 Emissions (ton)")
title("Median of Emissions")

#do a bar chart of percentage of zero values
barplot(bar.graph.data$percent.zero, names.arg=bar.graph.data$year, 
        ylab = "%")
title("Percentage of Zero readings")

mtext("Baltimore Vehicle-related PM2.5 Emissions", outer = TRUE, cex = 1.5)

#close device
dev.off()
print(dev.cur())
