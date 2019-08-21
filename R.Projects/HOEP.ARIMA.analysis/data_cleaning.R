##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
##
##  File name:  data_cleaning.R
##  Date:       01Mar2019
##
## Computes:
## Combines and cleans input files for rank_generation_data.R
## 
## Returns:
## a dataset of IESO predispatch data and IESO dispatch price data       
## See codebook and read me for more info
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------



  library(dplyr)
  library(tidyverse)
  library(lubridate)
  
  
  if(!file.exists("data")){
    
    dir.create("data")
    
  }
  
  if(!file.exists("output")){
    
    dir.create("output")
    
  }
  
  #Download dispatch files from IESO
  fileURL <- "http://reports.ieso.ca/public/PriceHOEPPredispOR/PUB_PriceHOEPPredispOR_2016.csv"
  download.file(fileURL, destfile = "./output/downloaded.DispatchData1.csv")
  
  fileURL2 <- "http://reports.ieso.ca/public/PriceHOEPPredispOR/PUB_PriceHOEPPredispOR_2017.csv"
  download.file(fileURL2, destfile = "./output/downloaded.DispatchData2.csv")
  
  fileURL3 <- "http://reports.ieso.ca/public/PriceHOEPPredispOR/PUB_PriceHOEPPredispOR_2018.csv"
  download.file(fileURL3, destfile = "./output/downloaded.DispatchData3.csv")
  
  #Get and record date of download
  dateDownloaded <- c("download date is", date())
  write.table(dateDownloaded, "./output/data_info.csv", append = T)
  
  #Input file as dataframe
  inputDispatchData1 <- read.csv("./output/downloaded.DispatchData1.csv", skip = 3, as.is = TRUE)
  
  inputDispatchData2 <- read.csv("./output/downloaded.DispatchData2.csv", skip = 3, as.is = TRUE)
  
  inputDispatchData3 <- read.csv("./output/downloaded.DispatchData3.csv", skip = 3, as.is = TRUE)
  
  DispatchData <- do.call("rbind", list(inputDispatchData1, inputDispatchData2, inputDispatchData3))
  
  
  #Combine date and hour, rename Hour to HourEnding, Date to timestamp
  DispatchData <- DispatchData %>%  
    mutate(Date = paste(Date, " ", Hour-1, ":00", sep = "")) %>% 
    rename("HE" = Hour) %>%
    rename("TimestampHS" = Date)
  
  #Remove any commas in HOEP and Coerce HOEP column into number class
  DispatchData$HOEP <- sapply(DispatchData$HOEP, function(x) gsub(",","",x))
  DispatchData <- mutate(DispatchData, HOEP = as.numeric(HOEP))
  
  
  #Coerce as POSIX, set to Eastern Standard Time
  DispatchData$TimestampHS <- as.POSIXlt(DispatchData$TimestampHS, 
                                         tz = "EST", format = "%Y-%m-%d %H:%M")
  
  DispatchData$TimestampHS <- as.POSIXct(DispatchData$TimestampHS)
  
  timezone <- c("time zone is", tz(DispatchData$TimestampHS))
  write.table(dateDownloaded, "./data/date_inputDispatchDATA_downloaded1.csv", append = T)
  
  #write file to csv
  write.table(DispatchData, "./output/inputDispatch.csv", sep = ",", col.names = T, row.names = F, append = F)  
  
  #read in 2nd set of files 
  #file provided by Logan who took it from sygration database
  inputPredispatchData1 <- read.csv("./data/HOEP Forecast 011118-190219.csv")
  
  inputPredispatchData1 <- inputPredispatchData1 %>%
                          rename(Ref.Hour = RefHr) %>%
                          rename(OR.10.Min.Sync.Forecast = OR10S) %>%
                          rename(OR.10.Min.non.sync.Forecast = OR10N) %>%
                          rename(OR.30.Min.Forecast = OR30R) %>%
                          select(Date,Hour,Ref.Hour,MCP,OR.10.Min.Sync.Forecast,OR.10.Min.non.sync.Forecast,OR.30.Min.Forecast)
  		
    #file provided by Logan who took it from sygration database
  inputPredispatchData2 <- read.csv("./data/HOEP_predispatch_2017-2018.csv")
  
  inputPredispatchData2 <- inputPredispatchData2 %>%
                          rename(Ref.Hour = Ref_Hour) %>%
                          rename(OR.10.Min.Sync.Forecast = OR_10S) %>%
                          rename(OR.10.Min.non.sync.Forecast = OR_10N) %>%
                          rename(OR.30.Min.Forecast = OR_30R) %>%
                          select(Date,Hour,Ref.Hour,MCP,OR.10.Min.Sync.Forecast,OR.10.Min.non.sync.Forecast,OR.30.Min.Forecast)
  
  PredispatchData <- rbind(inputPredispatchData1, inputPredispatchData2)
  
  

  #Combine date and hour, rename Hour to HourEnding, Date to timestamp
  PredispatchData <- PredispatchData %>%  
    mutate(Date = paste(Date, " ", Hour-1, ":00", sep = "")) %>% 
    rename("HE" = Hour) %>%
    rename("TimestampHS" = Date)
  
  #Coerce as POSIX, set to Eastern Standard Time
  PredispatchData$TimestampHS <- as.POSIXlt(PredispatchData$TimestampHS, 
                                            tz = "EST", format = "%Y-%m-%d %H:%M")
  
  PredispatchData$TimestampHS <- as.POSIXct(PredispatchData$TimestampHS)
  
  PredispatchData <- arrange(PredispatchData, Ref.Hour, TimestampHS)
  
  PredispatchData <- PredispatchData[!duplicated(PredispatchData[c(1,3)]), ]
  
  #write file to csv
  write.table(PredispatchData, "./output/inputPredispatch.csv", sep = ",", col.names = T, row.names = F, append = F) 
  
  #rm unnecessary files
  rm(inputDispatchData1,inputDispatchData2,inputPredispatchData1,inputPredispatchData2)
  
  
