##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
##
##  File name:  readlines.R
##  Date:       04NOV2019
##
##  Get data from text files
##  
##
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------

##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
##
##  File name:  word.vocab.builder.R
##  Date:       04NOV2019
##
##  Script to get and clean data.  Outputs ngram.dfm for word.predictor.R
##  
##
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------


##----------------------------------------------------------------------------
## Functions
##----------------------------------------------------------------------------



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
                        

file <- readtext("data/final/en_US/en_US.twitter.txt",encoding = "UTF-8")
chunk.corpus <- corpus(file)


toks <- chunk.corpus %>%
               tokens(
               remove_punct = TRUE,
               remove_numbers = TRUE,
               remove_symbols = TRUE,
               remove_url = TRUE,
               remove_twitter = TRUE) %>%
        #tokens_select(stopwords('english'),selection='remove') %>%
        #tokens_select(dict.regex, selection = 'remove', valuetype = "regex") %>%
        tokens_select(dict.profane, selection = 'remove', valuetype = "fixed") %>%
        tokens_select(dict.english, selection = 'keep', valuetype = "fixed")

dfm <- dfm(toks, 
            ngrams = 1,
           # remove_numbers = TRUE, 
           # remove_punct = TRUE,
           # remove_symbols = TRUE,
           # remove_separators = TRUE,
           # remove_twitter = TRUE,
           # remove_hyphens = TRUE,
           # #remove non-english words
            )



dfm.trim <- dfm_trim(dfm, min_termfreq = 4)

vector <- sort(colSums(dfm.trim),decreasing = TRUE)


if (!exists("frequency.df"))
{
        frequency.df <- data.frame(vector)
        frequency.df$names <- rownames(frequency.df)
        frequency.df <- frequency.df %>%
                select(names, everything())
} else {
        temp <- data.frame(vector)
        temp$names <- rownames(temp)
        temp <- temp %>%
                select(names, everything())
        frequency.df <- full_join(frequency.df,temp, by = "names")
        frequency.df[is.na(frequency.df)] <- 0
}

x <- cbind(frequency.df$names,rowSums(frequency.df[, c(2,3,4)]))