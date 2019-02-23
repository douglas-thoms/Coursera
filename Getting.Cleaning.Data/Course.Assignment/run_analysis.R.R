cleaning.data.assignment <- function(){

  
  if(!file.exists("data")){
    
    dir.create("data")
    
  }
  
  if(!file.exists("./data/cleaning.data.assignment.zip")){
        
        dir.create("./data/cleaning.data.assignment.zip")
      
          fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
          download.file(fileURL, destfile = "./data/cleaning.data.assignment.zip")
  
  }

        outPath <- "C:\\Users\\Douglas\\Documents\\Coursera\\Coursera\\Getting.Cleaning.Data\\Course.Assignment\\data"
        locFile = "./data/cleaning.data.assignment.zip"
        unzip(locFile,exdir = outPath, junkpaths = TRUE)
           
        print(list.files("./data/"))
  #economicData <- read.csv("./data/quiz4question4data_a.csv", header = FALSE, 
   #                        skip = 5, as.is=TRUE)
  
}