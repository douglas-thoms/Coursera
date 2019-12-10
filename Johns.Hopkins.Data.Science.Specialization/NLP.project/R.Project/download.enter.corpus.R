##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
##
##  File name:  download.enter.corpus
##  Date:       13Nov19
##
##  Step 1 in process
##  Download corpus files
##
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------

##----------------------------------------------------------------------------
## Functions
##----------------------------------------------------------------------------

# this file downloads and unzips a file
download.unzip <- function(file.loc,destination.path){
#download and unzip file if not present
        if(!file.exists(destination.path)){ 
                
                download.file(file.loc,destination.path)
        
                unzip(destination.path, exdir = data.directory)
        }      
}

# this file downloads and untars a file
download.untar <- function(file.loc,destination.path){
        #download and unzip file if not present
        if(!file.exists(destination.path)){ 
                
                download.file(file.loc,destination.path)
                
                untar(destination.path, exdir = data.directory)
        }      
}

##----------------------------------------------------------------------------
## Libraries, data and system
##----------------------------------------------------------------------------

session.info.list <- sessionInfo()

library(quanteda)
library(dplyr)
library(ggplot2)
library(lexicon)
library(stringr)
library(ngram)

data(profanity_zac_anger)
data(grady_augmented)


#for reproducibility, same randomness
set.seed(3353)

#create path for files to download
home.directory <- getwd()
data.directory <- paste(home.directory,"data", sep = "/")
training.data.file.path <- paste(data.directory,"Coursera-SwiftKey.zip",                                                          
                                 sep = "/")

training.data.file.path.2 <- paste(data.directory,"aclImdb_v1.tar.gz",                                                          
                                 sep = "/")

dir.create(data.directory, showWarnings = FALSE)

##----------------------------------------------------------------------------
## Acquiredata and Clean
##---------------------------------------------------------------------------

#download profanity list

training.data.loc <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"


#set up on for movie reviews
training.data.loc.2 <- "http://ai.stanford.edu/~amaas/data/sentiment/aclImdb_v1.tar.gz"

download.unzip(training.data.loc,training.data.file.path)
download.untar(training.data.loc.2,training.data.file.path.2)

#gutenberg partial corpus (manually download) and unzipped manually because of error
#https://web.eecs.umich.edu/~lahiri/gutenberg_dataset.html

