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

#add supplement dfm

dfm <- dfm(toks, ngrams = ngram)

dfm.trim <- dfm_trim(dfm, min_termfreq = 1)

vector <- sort(colSums(dfm.trim),decreasing = TRUE)


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

for(i in 1:3){
frequency.df <- get.frequency(input[i],1)                  
}

frequency.df <- data.frame(name = frequency.df$name,
                           frequency = rowSums(frequency.df[, c(2,3,4)]),
                           stringsAsFactors = FALSE)

#frequency.df$ngram.length <- sapply(frequency.df$name,wordcount,sep = "_")

saveRDS(frequency.df,"data/unigram.rds")

test <- readRDS("data/unigram.rds")

#keep as separate data frames and access them as necessary, for speed