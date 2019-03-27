Quize2Question2 <- function(){

  if(!file.exists("data")){
    
    dir.create("data")
    
  }
  
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
  download.file(fileURL, destfile = "question_2_data.csv")
  
  acs <- read.csv("question_2_data.csv")
  
  return(acs)

}


question 2 - sqldf("select pwgtp1 from acs where AGEP < 50") 

question 3 - b <- sqldf("select distinct AGEP from acs")