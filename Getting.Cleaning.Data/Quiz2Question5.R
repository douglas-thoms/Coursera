scrape_web <-function(){

  
  if(!file.exists("data")){
    
    dir.create("data")
    
  }
  
  url <- ("https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for")
  
  download.file(url,destfile = "./data/quiz2question5data.for")
  
  raw.data <- read.fortran(file = "./data/quiz2question5data.for",
                           c("1X", "A9",
                            "5X","F4.0","F4.0",
                            "5X","F4.0","F4.0",
                            "5X","F4.0","F4.0",
                            "5X","F4.0","F4.0"),
                            skip=4) 
  
  
  
  
  return(sum(raw.data$V4))
  
}