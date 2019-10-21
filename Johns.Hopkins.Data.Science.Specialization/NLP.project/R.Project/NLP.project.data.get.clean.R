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
## Functions
##----------------------------------------------------------------------------

get.lines <- function(df,type.info) {
        con <- file(as.character(df[type.info,2]),'rb')
        x <- 0
        #tmp2 <- as.character()
        
        for(i in 1:df[type.info,1]){
                tmp <- readLines(con, 1, encoding = "UTF-8", skipNul = TRUE)
                
                if(rbinom(1,1,sample.rate) & length(tmp)){
                        x <- x + 1
                        if(x == 1) tmp2 <- tmp else tmp2 <- c(tmp2,tmp)
                        
                }
                
        }
        close(con)
        return(as.character(tmp2))
}

create.corpus <- function(input,file,URL){
        
        output <- corpus(input, docnames = rep("news.sample",length(input)))
        metadoc(output, "source") <- "cheese"
        return(output)
}

##----------------------------------------------------------------------------
## Libraries and system
##----------------------------------------------------------------------------

session.info.list <- sessionInfo()

library(quanteda)

set.seed(3353)


home.directory <- getwd()
data.directory <- paste(home.directory,"data", sep = "/")
training.data.file.path <- paste(data.directory,"Coursera-SwiftKey.zip",                                                          
                           sep = "/")

dir.create(data.directory)

##----------------------------------------------------------------------------
## Acquiredata and Clean
##---------------------------------------------------------------------------


training.data.loc <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"

#set up if loop to check for file
#set up download date for file
if(!file.exists(training.data.file.path)){
download.file(training.data.loc, training.data.file.path)

training.data.date <- date()


unzip(training.data.file.path, exdir = data.directory)

}

#determine sample - assume normal distribution
# 95% confidence interval
# Sample Size Calculation:
#         Sample Size = (Distribution of 50%) / ((Margin of Error% / Confidence Level Score)Squared)
# Finite Population Correction:
#         True Sample = (Sample Size X Population) / (Sample Size + Population – 1)

#use sample of 2000


input.info.df <- data.frame(
        num.lines = c(1010274, 899289, 2360149),
        path = c('.//data//final//en_US//en_US.news.txt',
                  './/data//final//en_US//en_US.blogs.txt',
                  './/data//final//en_US//en_US.twitter.txt'),
        names = c('news','blogs','twitter')
)
        
news.sample <- (0.5 * (1-0.5))/((.05/2.576)^2)
news.sample.size <- (news.sample * input.info.df[1,1])/(news.sample + input.info.df[1,1] - 1)

blogs.sample <- (0.5 * (1-0.5))/((.05/2.576)^2)
blogs.sample.size <- (blogs.sample * input.info.df[2,1])/(blogs.sample + input.info.df[2,1] - 1)

twitter.sample <- (0.5 * (1-0.5))/((.05/2.576)^2)
twitter.sample.size <- (twitter.sample * input.info.df[3,1])/(twitter.sample + input.info.df[3,1] - 1)

sample.rate = 2000/input.info.df[2,1]*10

news<- get.lines(input.info.df,1)
blogs<- get.lines(input.info.df,2)
twitter<- get.lines(input.info.df,3)




##----------------------------------------------------------------------------
## Tokenize and Prfanity removal
##---------------------------------------------------------------------------

#use quanteda

#create corpus

news.cor <- create.corpus(news,"en_US.news.txt",
                          "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip")
blogs.cor <- create.corpus(blogs, "en_US.blogs.txt",
                           "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip")
twitter.cor <- create.corpus(twitter, "en_US.twitter.txt",
                             "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip")

#user doc vars
#source is file
#URL

#corpus + corpus to join

#use package to remove profanity and stop words
# blog.prof.l <- grepl(paste(profanity, collapse="|"),blogs)
# blog.prof <- length(blogs[!blog.prof])


