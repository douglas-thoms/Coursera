start <- date()
session.info.list <- sessionInfo()

library(quanteda)
library(dplyr)
library(ggplot2)
library(lexicon)
library(stringr)
library(ngram)

##----------------------------------------------------------------------------
## Function
##----------------------------------------------------------------------------

retrieve.candidates <- function(sentence){

#make this section a function

#add hyphens
sentence.w.under <- gsub(" ", "_", sentence)

#add in underscore, add wildcard * to last word
ngram.search <- paste(sentence.w.under,"*",sep="")

#get root n.gram
ngram.root.search <-sentence.w.under <- gsub( "^[^_]*_","",sentence.w.under)

#search appropriate data frame
if(length(sentence) == 4) {
                           ngram.df <- pentgram
                           root.df <- quantgram

}else if(length(sentence) == 3) {
                                 ngram.df <- quantgram
                                 root.df <- trigram

}else if(length(sentence) == 2) {
                                 ngram.df <- trigram
                                 root.df <- bigram
}else if(length(sentence) == 1) {
                                 ngram.df <- bigram
                                 root.df <- unigram
}else {
        ngram.df <- unigram
}

#search for ngram candidates
ngram.df <- ngram.df %>%
                 filter(name == grepl(ngram.search, name))
                        
root.df <- root.df %>%
                 filter(name == grepl(ngram.root.search, name))

return(merge(root.df,ngram.df))
}




##----------------------------------------------------------------------------
## Load vocab files
##----------------------------------------------------------------------------

unigram <- readRDS("data/unigram.rds")

bigram <- readRDS("data/bigram.rds")

trigram <- readRDS("data/trigram.rds")

quadgram <- readRDS("data/quadgram.rds")

pentagram <- readRDS("data/pentagram.rds")

##----------------------------------------------------------------------------
## Model
##----------------------------------------------------------------------------
#Create very small test set initially - only a couple of lines


#input string of words and remove punctuation, etc
sentence <- "the end of"
sentence <- sentence %>%
        tokens(remove_punct = TRUE,
               remove_numbers = TRUE,
               remove_symbols = TRUE,
               remove_url = TRUE,
               remove_twitter = TRUE)# %>%
#tokens_select(stopwords('english'),selection='remove')
sentence <- paste(sentence[[1]],collapse=" ")
print(sentence)

#determine string length
sentence.length <- wordcount(sentence)

#if over 4, truncuate to last 4 words
if (sentence.length >4) sentence <- word(sentence, start = sentence.length-3, end = sentence.length)


#regex is (^tell_me_.*)|(^tell_me$)
#check if ngram is observed

#search for n-1gram with *wildcard, if observed, move on
#if not drop first word in 4gram
#while there are less than 2 features bigram will be shortened

#df <- retrieve.candidates(sentence)
#ngram.root.search <- gsub( "^[^_]*_","",sentence.w.under)

sentence.w.under <- gsub(" ", "_", sentence)

#add in underscore, add wildcard * to last word
ngram.search <- paste("^",sentence.w.under,"_*",sep="")

#get root n.gram
ngram.root.search <- paste("^",sentence.w.under,"$",sep="")

#search appropriate data frame
if(wordcount(sentence) == 4) {
        ngram.df <- pentagram
        root.df <- quadgram
        
}else if(wordcount(sentence) == 3) {
        ngram.df <- quadgram
        root.df <- trigram
        
}else if(wordcount(sentence) == 2) {
        ngram.df <- trigram
        root.df <- bigram
}else if(wordcount(sentence) == 1) {
        ngram.df <- bigram
        root.df <- unigram
}else {
        ngram.df <- unigram
}

#search for ngram candidates
result.df <- ngram.df %>%
        filter(grepl(ngram.search,name))

result.root.df <- root.df %>%
        filter(grepl(ngram.root.search,name))

x <- full_join(result.root.df,result.df)

        
# df.select <- dfm_select(ngram.trim,pattern = sentence.prep, valuetype = "regex")
# while (nfeat(df.select) < 2){
#         sentence.w.under <- gsub( "^[^_]*_","",sentence.w.under)
#         sentence.prep <- paste("(^",sentence.w.under,"_[a-z]*$)|(^",sentence.w.under,"$)"
#                                ,sep = "")
#         print(sentence)
#         print(sentence.prep)
#         
#         #select file
# 
#         
#         if (grepl("_",sentence.w.under) == FALSE){
#                 #create top features subset of one
# 
#                 break
#         }
#         
# }




