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

#read lines from a text file, sampling them using rbinom
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

#creates corpus from large matrix, adds URL and source name is docvars
create.corpus <- function(input,text_name,file,URL){
        
        output <- corpus(input, docnames = rep(text_name,length(input)))
        docvars(output, "source") <- as.character(file)
        docvars(output, "URL") <- as.character(URL)
        return(output)
}

#create matrix of statistics
get.stats <- function(dfm){
        return(data.frame(num.features = nfeat(dfm),
                          sparsity = sparsity(dfm),
                          num.docs = ndoc(dfm)))
               
}

##----------------------------------------------------------------------------
## Libraries and system
##----------------------------------------------------------------------------

session.info.list <- sessionInfo()

library(quanteda)
library(dplyr)
library(ggplot2)

#for reproducibility, same randomness
set.seed(3353)


home.directory <- getwd()
data.directory <- paste(home.directory,"data", sep = "/")
training.data.file.path <- paste(data.directory,"Coursera-SwiftKey.zip",                                                          
                           sep = "/")

dir.create(data.directory)

##----------------------------------------------------------------------------
## Acquiredata and Clean
##---------------------------------------------------------------------------

#download profanity list
profanity.file <- paste(data.directory,"profanities.csv",sep = "/")
profanity <- as.vector(read.csv2(profanity.file, header = FALSE, stringsAsFactors = FALSE))

training.data.loc <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"


#download and unzip file if not present
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


#create data frame of 3 different files info
input.info.df <- data.frame(
        num.lines = c(1010274, 899289, 2360149),
        path = c('.//data//final//en_US//en_US.news.txt',
                 './/data//final//en_US//en_US.blogs.txt',
                 './/data//final//en_US//en_US.twitter.txt'),
        names = c('news','blogs','twitter')
)


#determine sample rate to use
news.sample <- (0.5 * (1-0.5))/((.05/2.576)^2)
news.sample.size <- (news.sample * input.info.df[1,1])/(news.sample + input.info.df[1,1] - 1)

blogs.sample <- (0.5 * (1-0.5))/((.05/2.576)^2)
blogs.sample.size <- (blogs.sample * input.info.df[2,1])/(blogs.sample + input.info.df[2,1] - 1)

twitter.sample <- (0.5 * (1-0.5))/((.05/2.576)^2)
twitter.sample.size <- (twitter.sample * input.info.df[3,1])/(twitter.sample + input.info.df[3,1] - 1)

sample.rate = 2000/input.info.df[2,1]*10
#use sample of 2000

#use function to read lines from text file
news<- get.lines(input.info.df,1)
blogs<- get.lines(input.info.df,2)
twitter<- get.lines(input.info.df,3)

#create corpus from matrices

news.corpus <- create.corpus(news,"news.sample","en_US.news.txt",
                             "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip")
blogs.corpus <- create.corpus(blogs,"blogs.sample", "en_US.blogs.txt",
                              "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip")
twitter.corpus <- create.corpus(twitter, "twitter.sampletex", "en_US.twitter.txt",
                                "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip")

#create one corpus
total.corpus <- corpus(news.corpus) + corpus(blogs.corpus) + corpus(twitter.corpus)



##----------------------------------------------------------------------------
## Clean Data/Tokenization/Ngrams
##----------------------------------------------------------------------------


#create dictionary of words to exclude
dict.fixed <- dictionary(list(profanity = profanity$V1))

#create regex expression to exclude
dict.regex <- dictionary(list(at.mark = "[@!#%&()*+./<=>_]",
                        number = "[0-9]"
))

#create tokens
total.tokens <- total.corpus %>%
                tokens(remove_punct = TRUE, 
                     remove_numbers = TRUE) %>%
                tokens_wordstem() %>%
                tokens_select(stopwords('english'),selection='remove') %>%
                tokens_select(dict.regex, selection = 'remove', valuetype = "regex") %>%
                tokens_select(dict.fixed, selection = 'remove', valuetype = "fixed") %>%
                tokens_tolower()

ngrams.tokens <- tokens_ngrams(total.tokens, 2:3)

#create dfm

ngrams.dfm <- dfm(ngrams.tokens)
words.dfm <- dfm(total.tokens)

#head(kwic(total.tokens, "love", window = 3))


##----------------------------------------------------------------------------
## Explore Data and N-grams
##----------------------------------------------------------------------------

#make matrix of features of unigram, bi-gram, tri-gram
#nfeat
#top features
#sparsity

x <- get.stats(words.dfm)

#matrix of topfeatures comparing unigram,bigram,trigram
y <- topfeatures(words.dfm)

#find raw number of feature
nfeat(words.dfm)
#find number of docs
ndoc(words.dfm)


