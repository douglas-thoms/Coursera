quiz4question2 <- function(){
  
  library(dplyr)
  library(stringr)
  
  if(!file.exists("data")){
    
    dir.create("data")
    
  }
  
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
  download.file(fileURL, destfile = "./data/quiz4question2data.csv")
  
  quiz4question1df<- read.csv("./data/quiz4question2data.csv")
  

  
  economicData <- read.csv("./data/quiz4question2data.csv", header = FALSE, 
                     skip = 5, as.is = TRUE)
  
  economicData <- select(economicData,-V3,-V6,-V7,-V8,-V9,-V10)
  

  names(economicData) <- c("ABREV","RANK","COUNTRY","GDP")
  
  economicData$RANK[economicData$RANK == ""] <- NA
  economicData <- filter(economicData, !is.na(RANK))
  
  print(grep("^United",economicData$COUNTRY), 3)
  
  
  
  GDPValue <-(grep("[0-9]",economicData$GDP, value = TRUE))
  
  GDPValue<-as.numeric(gsub("\\,", "", GDPValue))
  
  print(GDPValue)
  
  return(mean(GDPValue))
  
  #grab numbers in GDP column, put in vector, average
}