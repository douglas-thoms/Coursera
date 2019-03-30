##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
##
##  File name:  plot6.R
##  Date:       28Mar2019
##
##  Part of Course Project 2 for Exploratory data course
##      
##
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------

#Compare emissions from motor vehicle sources in Baltimore City with emissions 
#from motor vehicle sources in Los Angeles County, California (fips == "06037")
#Which city has seen greater changes over time in motor vehicle emissions?

require(tidyr)
require(ggplot2)
require(dplyr)
require(gridExtra)
require(reshape2)

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

change.calc <- df %>%
               filter(SCC %in% vehicle.SCC, 
                      (fips == "24510" | df$fips == "06037")) %>%
              group_by(fips,year) %>% 
              summarise(total.emission = sum(Emissions,na.rm = TRUE),
                        mean.emission = mean(Emissions,na.rm = TRUE),
                        median.emission = median(Emissions,na.rm = TRUE)) %>%
              melt(id.vars = c("fips","year")) %>%
              ungroup()

change.calc$fips <- gsub("24510","Baltimore",change.calc$fips)
change.calc$fips <- gsub("06037","LA",change.calc$fips)

change.calc$new.variable <- paste(change.calc$fips,
                                     as.character(change.calc$variable), sep = ".")

change.calc <- change.calc %>%
                  select(-fips,-variable) %>%
                  group_by(year) %>%
                  spread(new.variable,value) %>%
                  ungroup()


diffCalc <- function(col){
  x <- col - lag(col)
  return(round(x,1))
}

perCalc <- function(col){
  x <- (col - lag(col))/lag(col) 
  return(round(x,2))
}


change.calc$Baltimore.mean.diff <- diffCalc(change.calc$Baltimore.mean.emission)
change.calc$Baltimore.mean.diff.per <- perCalc(change.calc$Baltimore.mean.emission)
change.calc$Baltimore.total.diff <- diffCalc(change.calc$Baltimore.total.emission)
change.calc$Baltimore.total.diff.per <- perCalc(change.calc$Baltimore.total.emission)
change.calc$LA.mean.diff <- diffCalc(change.calc$LA.mean.emission)
change.calc$LA.mean.diff.per <- perCalc(change.calc$LA.mean.emission)
change.calc$LA.total.diff <- diffCalc(change.calc$LA.total.emission)
change.calc$LA.total.diff.per <- perCalc(change.calc$LA.total.emission)

change.calc <- change.calc[2:4,] 

change.calc <- change.calc %>%
                  select(-Baltimore.mean.emission,-Baltimore.median.emission,
                         -Baltimore.total.emission,-LA.mean.emission,
                         -LA.median.emission,-LA.total.emission)


x <- select(change.calc, 1:5)
colnames(x) <- c("Date", "mean.diff","mean.diff.per","total.diff","total.diff.per")
x$City <- "Baltimore"


y <- select(change.calc, c(1,6:9))
colnames(y) <- c("Date", "mean.diff","mean.diff.per","total.diff","total.diff.per")
y$City <- "LA"

bar.graph.data <- dplyr::bind_rows(x,y)


#save it to a PNG file with a width of 480 pixels and a height of 480 pixels.
#open device
#Name each of the plot files as \color{red}{\verb|plot1.png|}plot1.png, \color{red}{\verb|plot2.png|}plot2.png, etc.
png(filename = "plot6.png", width = 600, height = 600)

print(dev.cur())


g <- ggplot(bar.graph.data, aes(fill=City, y=mean.diff, x=Date)) + 
       geom_bar(position="dodge", stat="identity") +
       labs(y = "Mean Change (ton)", x = "")
  

h <- ggplot(bar.graph.data, aes(fill=City, y=mean.diff.per, x=Date)) + 
       geom_bar(position="dodge", stat="identity") +
       labs(y = "Mean Change (%)", x = "")

i <- ggplot(bar.graph.data, aes(fill=City, y=total.diff, x=Date)) + 
       geom_bar(position="dodge", stat="identity") +
       labs(y = "Total Change (ton)", x = "")
            

j <- ggplot(bar.graph.data, aes(fill=City, y=total.diff.per, x=Date)) + 
       geom_bar(position="dodge", stat="identity") +
       labs(y = "Total Change (%)", x = "") 
  

k <- grid.arrange(g, h, i, j,
              layout_matrix = rbind(c(1,2),c(3,4)), 
              top = "Motor Vehicle PM2.5 Emissions Change from last period")
plot(k)

#close device
dev.off()
print(dev.cur())