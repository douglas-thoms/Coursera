##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
##
##  File name:  
##  Date:       04NOV2019
##
##  Step 2 in process
##  Supplemental in process
##  Input script is download.enter.corpus.R
##  Create swiftkey ngrams
##
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------

##----------------------------------------------------------------------------
## Functions
##----------------------------------------------------------------------------

# this function outputs a table measuring the frequency of each ngram phase
get.frequency <- function(file, ngram){

file <- readtext(file,encoding = "UTF-8")
chunk.corpus <- corpus(file)


toks <- chunk.corpus %>%
        tokens(
                remove_punct = TRUE,
                remove_numbers = TRUE,
                remove_symbols = TRUE,
                remove_url = TRUE,
                remove_twitter = TRUE) %>%
        tokens_select(dict.profane, selection = 'remove', valuetype = "fixed") %>%
        tokens_select(dict.english, selection = 'keep', valuetype = "fixed")


#combine tokens into ngrams
dfm <- dfm(toks, ngrams = ngram)

#determine minimum frequency of ngrams to keep
dfm.trim <- dfm_trim(dfm, min_termfreq = 1)

#sum up all frequencies in all different sources of each of 3 corpus
vector <- sort(colSums(dfm.trim),decreasing = TRUE)

#combine different same ngram level of 3 different sources
        if (!exists("frequency.df"))
        {
                frequency.df <- data.frame(vector,stringsAsFactors = FALSE)
                frequency.df$name <- rownames(frequency.df)
                frequency.df <- frequency.df %>%
                        select(name, everything())
        } else {
                temp <- data.frame(vector,stringsAsFactors = FALSE)
                temp$name <- rownames(temp)
                temp <- temp %>%
                        select(name, everything())
                frequency.df <- full_join(frequency.df,temp, by = "name")
                frequency.df[is.na(frequency.df)] <- 0
        }

        return(frequency.df)
}

##----------------------------------------------------------------------------
## Libraries, data and system
##----------------------------------------------------------------------------

start <- date()

session.info.list <- sessionInfo()

library(quanteda)
library(dplyr)
library(ggplot2)
library(lexicon)
library(stringr)
library(ngram)
library(readtext)
library(lexicon)


data(profanity_zac_anger)
data(grady_augmented)



dict.profane <- dictionary(list(profanity = profanity_zac_anger))
dict.english <- dictionary(list(grady = grady_augmented))  

input <- c("data/final/en_US/en_US.blogs.txt",
             "data/final/en_US/en_US.news.txt",
             "data/final/en_US/en_US.twitter.txt")

rm(frequency.df)

#for all three sources, choose which ngram to calculate using second input
for(i in 1:3){
frequency.df <- get.frequency(input[i],1)     
}

#sum up rows of each ngram across three different sources to create a frequency table
frequency.df <- data.frame(name = frequency.df$name,
                           frequency = rowSums(frequency.df[, c(2,3,4)]),
                           stringsAsFactors = FALSE)

#save to file for future use
saveRDS(frequency.df,"data/unigram.rds")

test <- readRDS("data/unigram.rds")
