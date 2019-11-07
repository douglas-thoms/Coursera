start <- date()
session.info.list <- sessionInfo()

library(quanteda)
library(dplyr)
library(ggplot2)
library(lexicon)
library(stringr)
library(ngram)

unigram <- data.frame(readRDS("data/unigram.rds", ))
unigram <- unigram %>%
        transform(X2 = as.numeric(X2))

bigram <- readRDS("data/bigram.rds")

trigram <- readRDS("data/trigram.rds")

quadgram <- readRDS("data/quadgram.rds")

pentagram <- readRDS("data/pentagram.rds")

vocab.df <- data.frame(rbind(unigram,bigram,trigram,quadgram,pentagram))

vocab.df$X2 <- as.numeric(vocab.df$X2) 

names(vocab.df) <- c("ngram","frequency")



