quiz4question1 <- function(){
  
  library(dplyr)
  library(stringr)
  
  if(!file.exists("data")){
    
    dir.create("data")
    
  }
  
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
  download.file(fileURL, destfile = "./data/quiz4question1data.csv")
  
  quiz4question1df<- read.csv("./data/quiz4question4data.csv")
  
  print(strsplit(names(quiz4question1df), 'wgtp'))
  
}