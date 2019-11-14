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

download.unzip <- function(file.loc,destination.path){
#download and unzip file if not present
        if(!file.exists(destination.path)){ 
                
                download.file(file.loc,destination.path)
        
                unzip(destination.path, exdir = data.directory)
        }      
}

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


home.directory <- getwd()
data.directory <- paste(home.directory,"data", sep = "/")
training.data.file.path <- paste(data.directory,"Coursera-SwiftKey.zip",                                                          
                                 sep = "/")

training.data.file.path.2 <- paste(data.directory,"aclImdb_v1.tar.gz",                                                          
                                 sep = "/")

training.data.file.path.3 <- paste(data.directory,"Gutenberg.zip",                                                          
                                   sep = "/")

dir.create(data.directory, showWarnings = FALSE)

##----------------------------------------------------------------------------
## Acquiredata and Clean
##---------------------------------------------------------------------------

#download profanity list

training.data.loc <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"

#news difficult files - need to learn them, may not be worth it "http://kdd.ics.uci.edu/databases/reuters21578/reuters21578.tar.gz"

#set up on for movie reviews
training.data.loc.2 <- "http://ai.stanford.edu/~amaas/data/sentiment/aclImdb_v1.tar.gz"

download.unzip(training.data.loc,training.data.file.path)
download.untar(training.data.loc.2,training.data.file.path.2)



#set up one for gutenberg partial corpus (manually download) and unzipped manually because of error
#https://web.eecs.umich.edu/~lahiri/gutenberg_dataset.html

# #download and unzip file if not present
# if(!file.exists(training.data.file.path)){
#         download.file(training.data.loc, training.data.file.path)
#         
#         training.data.date <- date()
#         
#         
#         unzip(training.data.file.path, exdir = data.directory)
#         
# }



#                unzip(training.data.file.path, exdir = data.directory)




# fn <- "http://s.wordpress.org/resources/survey/wp2011-survey.tar.gz"
# download.file(fn,destfile="tmp.tar.gz")
# untar("tmp.tar.gz",list=TRUE)  ## check contents
# untar("tmp.tar.gz")
# ## or, if you just want to extract the target file:
# untar("tmp.tar.gz",files="wp2011-survey/anon-data.csv")
# X <- read.csv("wp2011-survey/anon-data.csv")