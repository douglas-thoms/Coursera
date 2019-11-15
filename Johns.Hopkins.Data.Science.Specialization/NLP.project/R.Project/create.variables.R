##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
##
##  File name:  
##  Date:       04NOV2019
##
##  Step 3 in process
##  Input script prep.freq.tables.supp.R and prep.freq.tables.R
##  Create ngram variables
##
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------

start <- date()
session.info.list <- sessionInfo()

library(quanteda)
library(dplyr)
library(ggplot2)
library(lexicon)
library(stringr)
library(ngram)


##----------------------------------------------------------------------------
## Functions
##----------------------------------------------------------------------------

combine.vocab <- function(swiftkey.ngram,other.ngram){
        combined <- rbind(swiftkey.ngram,other.ngram)
        combined <- combined %>%
                    group_by(name) %>%
                    summarise(x = sum(frequency)) %>%
                    rename(frequency = x)
}

##----------------------------------------------------------------------------
## Creating variables
##----------------------------------------------------------------------------

unigram <- readRDS("data/unigram.rds")
unigram.supp <- readRDS("data/unigram.supp.rds")

unigram <- combine.vocab(unigram,unigram.supp)

bigram <- readRDS("data/bigram.rds")
bigram.supp <- readRDS("data/bigram.supp.rds")

bigram <- combine.vocab(bigram,bigram.supp)

trigram <- readRDS("data/trigram.rds")
trigram.supp <- readRDS("data/trigram.supp.rds")

trigram <- combine.vocab(trigram,trigram.supp)

quadgram <- readRDS("data/quadgram.rds")
quadgram.supp <- readRDS("data/quadgram.supp.rds")

quadgram <- combine.vocab(quadgram,quadgram.supp)

pentagram <- readRDS("data/pentagram.rds")
pentagram.supp <- readRDS("data/pentagram.supp.rds")

pentagram <- combine.vocab(pentagram,pentagram.supp)




