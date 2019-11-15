#NEXT STEPS 
#-add supplemental files in input files
#-simplify this with a function

##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
##
##  File name:  
##  Date:       04NOV2019
##
##  Step 4 in process
##  Input script is create.variables.R
##  Create ngram variables in Rstudio environment
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
## Model
##----------------------------------------------------------------------------
#Create very small test set initially - only a couple of lines


#input string of words and remove punctuation, etc
sentence <- "I like how the same people are in almost all of Adam Sandler's"

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

output.df <- retrieve.candidates(ngram.search, ngram.root.search)

#get length of ngram
if(length(output.df$name)) {
        
        output.df$ngram.length <- sapply(output.df$name,wordcount,sep = "_")

root.frequency <- output.df %>%
        arrange(ngram.length) %>%
        filter(ngram.length == min(ngram.length))

output.df <- output.df %>%
        filter(ngram.length != min(ngram.length)) %>%
        mutate(score = 0.4^(5-ngram.length)*(frequency/root.frequency[1,2])) %>%
        arrange(desc(score))
}

for (i in iter.counter:0){
        
        print(i)
        if(i>1){
                print("other ngram")
                
                #demote ngram down one level
                ngram.search <- gsub( "^[^_]*_","",ngram.search)
                ngram.search <- paste("^",ngram.search,sep="")
                
                ngram.root.search <- gsub( "^[^_]*_","",ngram.root.search)
                ngram.root.search <- paste("^",ngram.root.search,sep="")
                print(paste("ngram.root.search is",ngram.root.search))
                
                #adjust sentence removing first word to rerun retrieve.candidates function
                sentence.length <- wordcount(sentence)
                sentence <- word(sentence,start=2,end=sentence.length)
                
                candidates.df <- retrieve.candidates(ngram.search, ngram.root.search)
                if(length(candidates.df$name)) {
                        #get length of ngram
                        candidates.df$ngram.length <- sapply(candidates.df$name,wordcount,sep = "_")
                        
                        root.frequency <- candidates.df %>%
                                arrange(ngram.length) %>%
                                filter(ngram.length == min(ngram.length))
                        
                        #use stupid back off model
                        #based it on 5-gram model
                        #alpha = 0.4
                        #if at least final bigram in model, use following calculation
                        
                        candidates.df <- candidates.df %>%
                                filter(ngram.length != min(ngram.length)) %>%
                                mutate(score = 0.4^(5-ngram.length)*(frequency/root.frequency[1,2]))
                                
                                output.df <- full_join(output.df,candidates.df)
                        
                        print(paste("candidates.df observations are",length(candidates.df$name)))
                }
        }else if(i==0){
                print("looking for unigram")
                
                #get total corpus size
                corpus.size <- sum(length(unigram$name),
                                   length(bigram$name),
                                   length(trigram$name),
                                   length(quadgram$name),
                                   length(pentagram$name))
                
                candidates.df <- unigram
                
                candidates.df <- retrieve.candidates(ngram.search, ngram.root.search)
                
                candidates.df <- candidates.df %>%
                        mutate(ngram.length = 1) %>%
                        mutate(score = 0.4^(5-ngram.length)*frequency/corpus.size) %>%
                        filter(score == max(score))
                        
                
                output.df <- full_join(output.df,candidates.df)
                
                #consolidate ngrams with same last words, combine values
                #first create last word column
                output.df <- mutate(output.df,output.word = 
                                            word(output.df$name,
                                                 start=output.df$ngram.length,
                                                 sep = "_"))
                
                #summarise different values according to last word
                final.output.df <- output.df %>%
                                        group_by(output.word) %>%
                                        summarise(score = sum(score)) %>%
                                        ungroup %>%
                                        #remove stop words
                                        filter(!(output.word %in% stopwords("english"))) %>%
                                        arrange(desc(score))
                
                print(paste("output.df observations are",length(output.df$name)))
                print(paste("Most likely answer is \'",final.output.df[1,1],"\'.", sep = ""))
                
        }
}