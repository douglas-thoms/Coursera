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
start <- date()

##----------------------------------------------------------------------------
## Functions
##----------------------------------------------------------------------------

#creates corpus from large matrix, adds URL and source name is docvars
create.corpus <- function(input,text_name,file,URL){

        output <- corpus(input, docnames = rep(text_name,length(input)))
        docvars(output, "source") <- as.character(file)
        docvars(output, "URL") <- as.character(URL)
        return(output)
}

##----------------------------------------------------------------------------
## Libraries, data and system
##----------------------------------------------------------------------------

session.info.list <- sessionInfo()

library(quanteda)
library(dplyr)
library(ggplot2)
library(lexicon)
library(stringr)
library(ngram)

data(profanity_zac_anger)
data(grady_augmented)


#for reproducibility, same randomness
set.seed(3353)



##----------------------------------------------------------------------------
## Get vocabulary
##----------------------------------------------------------------------------

#if statement to call vocab if dfm not generated

#Call ngram.dfm for vocabulary
if (!exists("news")) {
        print("inputting")
        source("readlines.R")
        #ngram.dfm <- get.data()
}

##----------------------------------------------------------------------------
## Clean Data/Tokenization/Ngrams
##----------------------------------------------------------------------------


#create dictionary of words to exclude
#add profane from lexicon
dict.profane <- dictionary(list(profanity = profanity_zac_anger))

#create regex expression to exclude
#dict.regex <- dictionary(list(at.mark = "[@!#%&()*+./<=>_]",
#                              number = "[0-9]-[0-9]"
#))

#add names - US and stuff
#See if these others work later
dict.english <- dictionary(list(grady = grady_augmented
))

#use lexicon grady_augmented and profane

#make do with worstemming for now, if need lemitization use later

#create tokens
#remove profane, stopwords and non-words
total.tokens <- total.corpus %>%
        tokens(remove_punct = TRUE,
               remove_numbers = TRUE,
               remove_symbols = TRUE,
               remove_url = TRUE,
               remove_twitter = TRUE) %>%
        #tokens_select(stopwords('english'),selection='remove') %>%
        #tokens_select(dict.regex, selection = 'remove', valuetype = "regex") %>%
        tokens_select(dict.profane, selection = 'remove', valuetype = "fixed")

#remove non-english words
total.tokens <- tokens_select(total.tokens,dict.english, selection = 'keep', valuetype = "fixed")

#reduce features but uncapitalizing and word stem
total.tokens <- total.tokens %>%
        #tokens_wordstem() %>%
        tokens_tolower()

num.tokens <- sum(ntoken(total.tokens))

ngram.toks <- tokens_ngrams(total.tokens, n=1:5)

ngram.dfm <- ngram.toks %>%
             dfm()

#trim vocab for low likely ngrams
ngram.trim <- dfm_trim(ngram.dfm, min_termfreq = 3)

nfeat.ngram <- nfeat(ngram.trim)
       
end <- date()