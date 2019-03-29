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

#How have emissions from motor vehicle sources changed from 1999–2008 
#in Baltimore City?

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

#create vector to see what SCC codes apply to these areas - not related to actual data output
LA.Baltimore.logical <- (subset(df, (df$fips == "06037") | (df$fips == "24510")))$SCC
LA.Baltimore.SCC <- SCC[LA.Baltimore.logical,]

#select SCC numbers relevant to vehicles operation
vehicle.SCC <- filter(SCC, Data.Category == "Onroad")
vehicle.logical <- apply(vehicle.SCC,1,function(row) 
                          length(grep("vehicle|vehicles|trucks|buses",
                          row, ignore.case = TRUE))>0)

#subset vehicle related SCC number in SCC file
vehicle.SCC <- vehicle.SCC[vehicle.logical,]$SCC



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
par(mfrow = c(2,3), oma = c(0,0,2,0))

#bar graph - total
barplot(bar.graph.data$total.emission, names.arg=bar.graph.data$year, 
        ylab = "Total PM2.5 Emissions (ton)")
title("Total Emissions")

# scatter plot
boxplot(scatter.graph.data$Emissions ~ scatter.graph.data$year, ylim = c(-0,5))
title("Spread of readings")

# scatter plot
boxplot(scatter.graph.data$Emissions ~ scatter.graph.data$year)
title("Spread of readings")

#do chart of mean
barplot(bar.graph.data$mean.emission, names.arg=bar.graph.data$year, 
        ylab = "Mean (ton)")
title("Mean Emissions")

#do chart of median
barplot(bar.graph.data$median.emission, names.arg=bar.graph.data$year, 
        ylab = "Median (ton)")
title("Median Emissions")

mtext("Baltimore Vehicle-related PM2.5 Emissions", outer = TRUE, cex = 1.5)

