
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
##
##  File name:  
##  Date:       09DEC2019
##
##  Step 4 in process
##  Input script is create.variables.R
##  Output a score table using Stupid Backoff algorithm
##
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------

start <- date()
session.info.list <- sessionInfo()

library(data.table)
library(dplyr)
library(stringr)
library(ngram)
library(purrr)

##----------------------------------------------------------------------------
## Function
##----------------------------------------------------------------------------

retrieve.candidates <- function(name,ngram.length.predicted){
  
        #load appropriate corpus of ngrams
        if(ngram.length.predicted == 5) {
                root.df <- quadgram
                
        }else if(ngram.length.predicted == 4) {
                root.df <- trigram
                
        }else if(ngram.length.predicted == 3) {
                root.df <- bigram
        }else if(ngram.length.predicted == 2) {
                root.df <- unigram
        }else {
        }
        
        #create score tables
        if(ngram.length.predicted >= 2){
                
                #create dataframe from input sentences
                ngram.df <- name
                
                #attach root name to frequency table
                root.df <- root.df %>%
                        rename(root.name = name,
                               root.frequency = frequency)

                #merge in root name frequency
                result.df <- merge(ngram.df,root.df)
                
                #calculate scores and only take top 5 scores per rootname
                result.df <- result.df %>%
                        mutate(coefficient = 0.15^(5-ngram.length.predicted),
                               score = coefficient*(frequency/root.frequency)) %>%
                        #keep only top ten scoring per root.name
                        arrange(root.name,desc(score)) %>%
                        group_by(root.name) %>%
                        slice(1:5) %>%
                        ungroup()
                        
        
                return(result.df)
                
        
        #calculate final recursion level for stupid backoff model (different calculation)
        #only keep top 200 scoring
        } else if(ngram.length.predicted == 1){
                #get total corpus size
                corpus.size <- sum(length(unigram$name),
                                   length(bigram$name),
                                   length(trigram$name),
                                   length(quadgram$name),
                                   length(pentagram$name))
                result.df <- name %>%
                        mutate(root.name = NA,
                               root.frequency = corpus.size,
                               #ngram.length = ngram.length,
                               coefficient = 0.15^(5-ngram.length.predicted),
                               score = coefficient*(frequency/root.frequency)) %>%
                               #filter out 1 frequency unigrams - probably not real words
                        filter(frequency != 1) %>%
                        select(root.name,name,frequency,root.frequency,coefficient,score) %>%
                        arrange(desc(score)) %>%
                        slice(1:200)
                
                return(result.df)
                
        }
}


##----------------------------------------------------------------------------
## Model
##----------------------------------------------------------------------------


#create list of variables to use in loop below
ngram.sources <- list(unigram,bigram,trigram,quadgram,pentagram)

score.table <- NULL

#loop function that calculates score using stupid.backoff algo and
#root name (n-1gram)
for(i in 1:5){
        
        ngram.length.predicted <- i
        
        #enter vector of ngram to calculate it scores
             
        #use purr to avoid loop and count ngram.length
        
        input.table <- ngram.sources[[i]]
        
        input.table$root.name <- map_chr(input.table$name,sub,pattern = "_*[a-zA-Z0-9]*$", replacement = "")
        
        
        #output needs to be predicted phrase, frequency, score
        df <- retrieve.candidates(input.table, ngram.length.predicted = ngram.length.predicted)
        score.table <- rbind(score.table,df)
}


#set up score.table as data.table and sort
score.table <- data.table(score.table)
setkey(score.table, frequency)
score.table <- score.table[frequency >=12]
score.table[, `:=` (frequency = NULL,root.frequency = NULL, coefficient = NULL)]
setkey(score.table, root.name)
