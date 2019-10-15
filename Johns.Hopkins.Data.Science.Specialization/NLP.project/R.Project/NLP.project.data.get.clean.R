##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
##
##  File name:  NLP.project.data.get.clean
##  Date:       15OCT2019
##
##  Script to get and clean data
##  
##
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------

##----------------------------------------------------------------------------
## Background
##----------------------------------------------------------------------------

##----------------------------------------------------------------------------
## Libraries and system
##----------------------------------------------------------------------------

session.info.list <- sessionInfo()



home.directory <- getwd()
data.directory <- paste(home.directory,"data", sep = "/")
training.data.file <- paste(data.directory,"Coursera-SwiftKey.zip",                                                          
                           sep = "/")

dir.create(data.directory)

##----------------------------------------------------------------------------
## Acquiredata
##---------------------------------------------------------------------------

#training.data.loc <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"

#set up if loop to check for file
#set up download date for file
if(!file.exists(training.data.file)){
training.data.file <- download.file(training.data.loc, training.data.file)

training.data.date <- date()

zipF <- file.choose()
unzip(zipF, exdir = data.directory)

}

#loops reads in texts files from zip file
file.vector <- list.files(path = "./data/final/en_US")

for (i in 1:3){
        
        path = paste("./data/final/en_US", file.vector[i], sep = "/")
             assign(file.vector[i], read.delim(path))
        
}




