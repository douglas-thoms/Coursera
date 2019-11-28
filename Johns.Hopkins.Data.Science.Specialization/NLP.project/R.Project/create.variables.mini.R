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

unigram <- readRDS("data/unigram.mini.rds")


bigram <- readRDS("data/bigram.mini.rds")


trigram <- readRDS("data/trigram.mini.rds")


quadgram <- readRDS("data/quadgram.mini.rds")


pentagram <- readRDS("data/pentagram.mini.rds")




