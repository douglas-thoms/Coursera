Quiz3Question2 <- function(){
  
  if(!file.exists("data")){
    
    dir.create("data")
    
  }
  
  library(jpeg)
  
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
  download.file(fileURL, destfile = "./data/quiz3_question_2_data.jpg", mode = "wb")
  
  photo <- readJPEG("./data/quiz3_question_2_data.jpg", native = TRUE)
  
  print(quantile(photo,probs = c(0.3,0.8)))
  
}