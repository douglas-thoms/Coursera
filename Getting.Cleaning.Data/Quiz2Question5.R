scrape_web <-function(){
  
  url <- ("https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for")
  
  download.file(url,destfile = "quiz2question5data.for")
  
  raw.data <- read.fortran("quiz2question5data.for") 
  
  
  json2 <- jsonlite::fromJSON(jsonlite::toJSON(raw.data))
  
  
  return(json2)
  
}