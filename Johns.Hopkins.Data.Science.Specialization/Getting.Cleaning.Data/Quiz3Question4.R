Quiz3Question4 <- function(){
  
  library(dplyr)
  
  if(!file.exists("data")){
    
    dir.create("data")
    
  }
  
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
  download.file(fileURL, destfile = "./data/quiz3_question_3a_data.csv")
  
  GDP.df<- read.csv("./data/quiz3_question_3a_data.csv")
  
  colnames(GDP.df)[1] <- "CountryCode"
  colnames(GDP.df)[2] <- "Ranking"
  
  GDP.df$Ranking[GDP.df$Ranking == ""] <- NA
  GDP.df <- filter(GDP.df, !is.na(Ranking))
  
  GDP.df$Ranking <- as.numeric(as.character(GDP.df$Ranking))
  
  print(GDP.df$Ranking)
  print(class(GDP.df$Ranking))
  
  fileURL2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
  download.file(fileURL2, destfile = "./data/quiz3_question_3b_data.csv")
  
  educational.df<- read.csv("./data/quiz3_question_3b_data.csv")
  
  combined.df <- merge(GDP.df,educational.df)
  
  return(tapply(combined.df$Ranking,combined.df$Income.Group,mean))
  
}