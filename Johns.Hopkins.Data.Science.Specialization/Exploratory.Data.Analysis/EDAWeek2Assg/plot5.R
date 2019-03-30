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
require(dplyr)

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

bar.graph.data <- df %>%
               filter(SCC %in% vehicle.SCC, fips == "24510") %>%
               group_by(year) %>% 
               summarise(total.emission = sum(Emissions, na.rm = TRUE),
               median.emission = median(Emissions, na.rm = TRUE)) %>%
               ungroup()

#save it to a PNG file with a width of 480 pixels and a height of 480 pixels.
#open device
#Name each of the plot files as \color{red}{\verb|plot1.png|}plot1.png, \color{red}{\verb|plot2.png|}plot2.png, etc.
png(filename = "plot5.png", width = 600, height = 600)

print(dev.cur())

#set 4 charts
par(mar = c(5, 5, 3, 5))

barplot(bar.graph.data$total.emission, names.arg=bar.graph.data$year,
        ylab = "Total (ton)", col = "red", 
        main = "Motor Vehicle PM2.5 Emissions in Baltimore")

#create 2nd part of graph
par(new = TRUE)

barplot(bar.graph.data$median.emission, names.arg=bar.graph.data$year,
        ylab = "", xaxt = "n", yaxt = "n", col = "blue", ylim =c(0,0.35))
axis(side = 4)

mtext("Median (ton)", side = 4, line = 3)

legend("topright", c("Total", "Median"),
       col = c("red", "blue"), lty = c(1, 1))

#close device
dev.off()
print(dev.cur())