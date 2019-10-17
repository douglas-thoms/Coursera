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


set.seed(3353)


home.directory <- getwd()
data.directory <- paste(home.directory,"data", sep = "/")
training.data.file.path <- paste(data.directory,"Coursera-SwiftKey.zip",                                                          
                           sep = "/")

dir.create(data.directory)

##----------------------------------------------------------------------------
## Acquiredata
##---------------------------------------------------------------------------

training.data.loc <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"

#set up if loop to check for file
#set up download date for file
if(!file.exists(training.data.file.path)){
download.file(training.data.loc, training.data.file.path)

training.data.date <- date()


unzip(training.data.file.path, exdir = data.directory)

}

#loops reads in texts files from zip file
#file.vector <- list.files(path = "./data/final/en_US")

# for (i in 1:3){
#         
#         path = paste("./data/final/en_US", file.vector[i], sep = "/")
#              assign(file.vector[i], read.delim(path, header = F, fileEncoding = "UTF-8"))
#         
# }

#determine sample - assume normal distribution
# 95% confidence interval
# Sample Size Calculation:
#         Sample Size = (Distribution of 50%) / ((Margin of Error% / Confidence Level Score)Squared)
# Finite Population Correction:
#         True Sample = (Sample Size X Population) / (Sample Size + Population – 1)

#use sample of 1000

news.lines <- 1010274
blogs.lines <-  899289 
twitter.lines <- 2360149
        
news.sample <- (0.5 * (1-0.5))/((.05/2.576)^2)
news.sample.size <- (news.sample * news.lines)/(news.sample + news.lines - 1)

blogs.sample <- (0.5 * (1-0.5))/((.05/2.576)^2)
blogs.sample.size <- (blogs.sample * blogs.lines)/(blogs.sample + blogs.lines - 1)

twitter.sample <- (0.5 * (1-0.5))/((.05/2.576)^2)
twitter.sample.size <- (twitter.sample * twitter.lines)/(twitter.sample + twitter.lines - 1)

sample.rate = 1000/blogs.lines

#pseudo code
#read one line  -> variable x
#rbinom -> if good, keep in new data frame


        
# #input and sample news
news.con <- file('./data/final/en_US/en_US.news.txt','rb')
x <- 0
for(i in 1:news.lines){
        tmp <- readLines(news.con, 1, encoding = "UTF-8", skipNul = TRUE)
        if(rbinom(1,1,sample.rate)){
              x <- x + 1
              if(x == 1) news <- tmp else news <- rbind(news,tmp)
        }

}
close(news.con)
news <- as.data.frame(news)
colnames(news) <- "news"
rownames(news) <-c(1:x)

# #input and sample blogs
blogs.con <- file('./data/final/en_US/en_US.blogs.txt','rb')
x <- 0
for(i in 1:blogs.lines){
        tmp <- data.frame()
        tmp <- readLines(blogs.con, 1, encoding = "UTF-8", skipNul = TRUE)
        if(rbinom(1,1,sample.rate)){
                x <- x + 1
                if(x == 1) blogs <- tmp else blogs <- rbind(blogs,tmp)
        }

}
close(blogs.con)
blogs <- as.data.frame(blogs)
colnames(blogs) <- "blogs"
rownames(blogs) <-c(1:x)

#input and sample twitter
twitter.con <- file('./data/final/en_US/en_US.twitter.txt', 'rb')
x <- 0
for(i in 1:twitter.lines){
        tmp <- readLines(twitter.con, 1, encoding = "UTF-8", skipNul = TRUE)
        if(rbinom(1,1,sample.rate)){
                x <- x + 1
                if(x == 1) twitter <- tmp else twitter <- rbind(twitter,tmp)
        }

}
twitter <- as.data.frame(twitter)
colnames(twitter) <- "twitter"
close(twitter.con)
rownames(twitter) <-c(1:x)

# blogs.con <- file('./data/final/en_US/en_US.blogs.txt','rb')
# en_US.blogs.txt <- readLines(blogs.con, 5, encoding = "UTF-8", skipNul = TRUE)
# close(blogs.con)
# en_US.blogs <- data.frame(en_US.blogs.txt, stringsAsFactors = FALSE)
# 
# #en_US.twitter.txt<-fread("./data/final/en_US/en_US.twitter.txt", sep='\n', header=FALSE)
# twitter.con <- file('./data/final/en_US/en_US.twitter.txt','rb')
# en_US.twitter.txt <- readLines(twitter.con, 5, encoding = "UTF-8", skipNul = TRUE)
# close(twitter.con)
# en_US.twitter <- data.frame(en_US.twitter.txt)