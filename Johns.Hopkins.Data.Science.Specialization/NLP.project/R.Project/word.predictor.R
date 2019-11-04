##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
##
##  File name:  WordPredictor.R
##  Date:       04NOV2019
##
##  Uses word.vocab.builder.R to predict the next word
##  
##
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------

##----------------------------------------------------------------------------
## Library, Environment
##----------------------------------------------------------------------------

session.info.list <- sessionInfo()

library(quanteda)
library(dplyr)
library(ggplot2)
library(lexicon)
library(stringr)
library(ngram)

##----------------------------------------------------------------------------
## Get vocabulary
##----------------------------------------------------------------------------

#if statement to call vocab if dfm not generated

#Call ngram.dfm for vocabulary
if (!exists("ngram.dfm")) source("word.vocab.builder.R")

#find way to remove all variables except for ngram from stuff


