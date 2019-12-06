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
                #ngram.df <- pentagram
                root.df <- quadgram
                
        }else if(ngram.length.predicted == 4) {
                #ngram.df <- quadgram
                root.df <- trigram
                
        }else if(ngram.length.predicted == 3) {
                #ngram.df <- trigram
                root.df <- bigram
        }else if(ngram.length.predicted == 2) {
                #ngram.df <- bigram
                root.df <- unigram
        }else {
                #ngram.df <- unigram
        }
        
        #replace grepl with subset or filter
        #rename is fast, so rename to root, then innerjoin or merge
        #sub pentagram %>% mutate(root.name = sub("_*[a-zA-Z0-9]*$","",name))
        #need to review innerjoin, merge to see if works as expected
        #search for ngram candidates on bigrams and up, using stupid backoff recursion
        if(ngram.length.predicted >= 2){
                
                #create dataframe from input sentences
                ngram.df <- name
                
                root.df <- root.df %>%
                        rename(root.name = name,
                               root.frequency = frequency)

                
                result.df <- merge(ngram.df,root.df)

                result.df <- result.df %>%
                        mutate(coefficient = 0.2^(5-ngram.length.predicted),
                               score = coefficient*(frequency/root.frequency)) %>%
                        #keep only top ten scoring per root.name
                        arrange(root.name,desc(score)) #%>%
                        #group_by(root.name) %>%
                        #slice(1:5) %>%
                        #ungroup()
                        
        
                return(result.df)
                
        
        #calculate final recursion level for stupid backoff model
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
                               coefficient = 0.2^(5-ngram.length.predicted),
                               score = coefficient*(frequency/root.frequency)) %>%
                        select(root.name,name,frequency,root.frequency,coefficient,score) %>%
                        arrange(desc(score)) %>%
                        slice(1:500)
                
                return(result.df)
                
        }
}


##----------------------------------------------------------------------------
## Model
##----------------------------------------------------------------------------

#take vector of same n-grams in underscore format
#Create very small test set initially - only a couple of lines

ngram.sources <- list(unigram,bigram,trigram,quadgram,pentagram)

final.values <- NULL

for(i in 1:5){
        
        ngram.length.predicted <- i
        
        #enter vector of ngram to calculate it scores
             
        #use purr to avoid loop and count ngram.length
        
        score.table <- ngram.sources[[i]]
        
        score.table$root.name <- map_chr(score.table$name,sub,pattern = "_*[a-zA-Z0-9]*$", replacement = "")
        
        
        #output needs to be predicted phrase, frequency, score
        df <- retrieve.candidates(score.table, ngram.length.predicted = ngram.length.predicted)
        final.values <- rbind(final.values,df)
}


#set up final.values as data.table
final.values <- data.table(final.values)
setkey(final.values, root.name)