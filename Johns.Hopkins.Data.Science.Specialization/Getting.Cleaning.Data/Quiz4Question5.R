quiz4question5 <- function(){
  
  library(quantmod)
  library(dplyr)
  library(lubridate)

  
  amzn = getSymbols("AMZN",auto.assign=FALSE)
  sampleTimes = index(amzn)
  
 
  
  #Coerce as POSIX
  sampleTimes <- as.POSIXlt(sampleTimes, tz = "America/Toronto", format = "%Y-%m-%d")
  
  sampleTimes2012logical <- year(sampleTimes) == 2012
                    
  sampleTimes2012 <- sampleTimes[sampleTimes2012logical]
  
  print(sampleTimes2012)
  
  sampleTimes2012Mondaylogical <- wday(sampleTimes2012) == 2
  
  sampleTimes2012Monday <- sampleTimes2012[sampleTimes2012Mondaylogical]
  
  print(sampleTimes2012Monday)
  
  return(c(length(sampleTimes2012),length(sampleTimes2012Monday)))
  
}