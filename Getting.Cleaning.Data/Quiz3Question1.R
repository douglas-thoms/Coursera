Quiz3Question1 <- function(){
  
  if(!file.exists("data")){
    
    dir.create("data")
    
  }
  
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
  download.file(fileURL, destfile = "./data/quiz3_question_1_data.csv")
  
  agriculture <- read.csv("./data/quiz3_question_1_data.csv")
  
  #variables ACR - 3 for 10 acre or more, AGS - 6 of $10,000 or more
  
  agricultureLogical <- (agriculture$ACR >= 3) & (agriculture$AGS >= 6)
  
  print(agricultureLogical)
  
  
  return(which(agricultureLogical))
  
}

##filter(cran, !is.na(r_version))