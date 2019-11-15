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

unigram <- readRDS("data/unigram.rds")

bigram <- readRDS("data/bigram.rds")

trigram <- readRDS("data/trigram.rds")

quadgram <- readRDS("data/quadgram.rds")

pentagram <- readRDS("data/pentagram.rds")




