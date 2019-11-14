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

retrieve.candidates <- function(ngram.search,ngram.root.search){

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
        
        return(full_join(result.root.df,result.df))
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
sentence <- "Be grateful for the good times and keep the faith during the"
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

iter.counter <-wordcount(sentence)

#regex is (^tell_me_.*)|(^tell_me$)
#check if ngram is observed

sentence.w.under <- gsub(" ", "_", sentence)

#add in underscore, add wildcard * to last word
#regex example "^friend_{1}"
ngram.search <- paste("^",sentence.w.under,"_{1}",sep="")

#get root n.gram
ngram.root.search <- paste("^",sentence.w.under,"$",sep="")

candidates.df <- retrieve.candidates(ngram.search, ngram.root.search)

while (length(candidates.df$name) < 2){
        

        #demote ngram down one level
        ngram.search <- gsub( "^[^_]*_","",ngram.search)
        ngram.search <- paste("^",ngram.search,sep="")
        
        ngram.root.search <- gsub( "^[^_]*_","",ngram.root.search)
        ngram.root.search <- paste("^",ngram.root.search,sep="")
        print(paste("ngram.root.search is",ngram.root.search))
        
        #count down on iterations
        iter.counter <- iter.counter - 1
        
        sentence.length <- wordcount(sentence)
        sentence <- word(sentence,start=2,end=sentence.length)
        
        
        
        #use counter based on word length
        if(iter.counter == 0){
                print("break loop")
                candidates.df <- data.frame(readRDS("data/unigram.rds"),stringsAsFactors = FALSE)
                break
        }
        
        candidates.df <- retrieve.candidates(ngram.search, ngram.root.search)
        print(paste("candidates.df observations are",length(candidates.df$name)))
}


#get length of ngram
candidates.df$ngram.length <- sapply(candidates.df$name,wordcount,sep = "_")

root.frequency <- candidates.df %>%
                  arrange(ngram.length) %>%
                  filter(ngram.length == min(ngram.length))


#use stupid back off model
#based it on 5-gram model
#alpha = 0.4
#if at least final bigram in model, use following calculation

if(iter.counter >=1){
                answer.df <- candidates.df %>%
                             filter(ngram.length != min(ngram.length)) %>%
                             mutate(score = 0.4^(5-ngram.length)*(frequency/root.frequency[1,2])) %>%
                             arrange(desc(score))
                
                             print(paste("answer is",answer.df[1,1]))
                
#if final bigram not in vocabulary                
}else if(iter.counter == 0){

                            answer.df <- candidates.df %>% 
                            filter(frequency == max(frequency))
                       
                            print(paste("answer is",answer.df[1,1]))
}
