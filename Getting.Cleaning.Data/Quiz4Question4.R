quiz4question4 <- function(){
  
  library(dplyr)
  library(stringr)
  
  if(!file.exists("data")){
    
    dir.create("data")
    
  }
  
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
  download.file(fileURL, destfile = "./data/quiz4question4data_a.csv")
  
  economicData <- read.csv("./data/quiz4question4data_a.csv", header = FALSE, 
                           skip = 5, as.is=TRUE)

  economicData <- select(economicData,-V3,-V6,-V7,-V8,-V9,-V10)
  
  names(economicData) <- c("CountryCode","RANK","COUNTRY","GDP")
  
  economicData$RANK[economicData$RANK == ""] <- NA
  economicData <- filter(economicData, !is.na(RANK))
    
  fileURL2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
  download.file(fileURL2, destfile = "./data/quiz4question4data_b.csv")
  
  educationalData <- read.csv("./data/quiz4question4data_b.csv", header = TRUE, as.is=TRUE)
  
  combinedDF <- merge(economicData, educationalData)
  
  fiscal <-(grep("[F-f][I-i][S-s][C-c][A-a][L-l]",combinedDF, value = TRUE))
  
  
  return(fiscal)
  
}