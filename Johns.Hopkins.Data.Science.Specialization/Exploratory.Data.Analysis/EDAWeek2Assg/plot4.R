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

#Across the United States, how have emissions from coal combustion-related 
#sources changed from 1999–2008?

require(tidyr)
require(dplyr)

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

df <- NEI
df$Emissions <- NEI$Emissions

#need to find list of coal-related SCC to filter NEI
#do grep search for coal (upper and lower), take vector and take the relevant SCC
coal.logical <- apply(SCC,1,function(row) length(grep("comb.*coal",row, ignore.case = TRUE))>0)
coal.SCC <- SCC[coal.logical,]$SCC

#filter and summarise data into median and mean
bar.graph.data <- df %>%
               filter(SCC %in% coal.SCC) %>%
               group_by(year) %>% 
               summarise(total.emission = sum(Emissions, na.rm = TRUE),
               median.emission = median(Emissions, na.rm = TRUE)) %>%
               ungroup()



#save it to a PNG file with a width of 480 pixels and a height of 480 pixels.
#open device
#Name each of the plot files as \color{red}{\verb|plot1.png|}plot1.png, \color{red}{\verb|plot2.png|}plot2.png, etc.
png(filename = "plot4.png", width = 480, height = 480)

print(dev.cur())

#create chart
par(mar = c(5,5,5,5), mfrow = c(1,1))

q <- barplot(bar.graph.data$total.emission, names.arg=bar.graph.data$year,
        ylab = "Total (ton)", col = "green", 
        main = "Coal Combustion-related PM2.5 Emissions")

par(new = T, mar = c(5,5,5,5), mfrow = c(1,1))

with(bar.graph.data, plot(year, median.emission, type = "l", col = "red",
                          axes=F, xlab=NA, ylab=NA, lwd = 3))
axis(side = 4)
mtext(side = 4, line = 3, 'Median (Ton)')

legend("topright", c("Total", "Median"),
       col = c("green", "red"), lty = c(1, 1), lwd = 3)

#close device
dev.off()
print(dev.cur())
