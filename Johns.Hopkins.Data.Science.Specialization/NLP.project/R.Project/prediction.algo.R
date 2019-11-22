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
#this function generates search terms to filter ngram tables to a max of pentagrams
generate.search.terms <- function(sentence){

        n.words <- wordcount(sentence)
        sentence <- gsub(" ", "_", sentence)
        
        output <- NULL
        
        for(i in 1:n.words) {
                #generate ngrams from blank to pentagram
                output <- c(output, word(sentence, start = i, end = n.words, sep = "_"))
        }
        
        #create dataframe
        output <- data.frame(name = output, stringsAsFactors = FALSE)
        output <- output %>%
                #regex example ^friend$
                mutate(root.search = paste("^",name,"$",sep="")) %>%
                #regex example "^friend_{1}"
                mutate(combo.search = paste("^",name,"_{1}",sep=""))
        
        #add blank row
        output <- rbind(output, c("","","",""))
        
        return(output)
}

retrieve.candidates <- function(name,root.search,combo.search){
        #get sentence level
        sentence.length <- wordcount(name, sep = "_")
        
        #load appropriate corpus of ngrams
        if(sentence.length == 4) {
                ngram.df <- pentagram
                root.df <- quadgram
                
        }else if(sentence.length == 3) {
                ngram.df <- quadgram
                root.df <- trigram
                
        }else if(sentence.length == 2) {
                ngram.df <- trigram
                root.df <- bigram
        }else if(sentence.length == 1) {
                ngram.df <- bigram
                root.df <- unigram
        }else {
                ngram.df <- unigram
        }
        
        #search for ngram candidates on bigrams and up, using stupid backoff recursion
        if(sentence.length >= 1){

                #filter according to regex
                result.df <- ngram.df %>%
                        filter(grepl(combo.search,name))
                
                #if found result
                if(length(result.df$name)){
                
                #filter to regex
                result.root.df <- root.df %>%
                filter(grepl(root.search,name))
                
                #find root frequency
                root.frequency <- result.root.df$frequency[[1]]
                
                #calculate score using stupid backoff model
                result.df <- result.df %>%
                        mutate(root.name = name,
                               root.frequency = root.frequency,
                               ngram.length = sentence.length + 1,
                               score = 0.4^(5-ngram.length)*(frequency/root.frequency))
        
                return(result.df)
                }
        
        #calculate final recursion level for stupid backoff model
        } else if(sentence.length == 0){
                #get total corpus size
                corpus.size <- sum(length(unigram$name),
                                   length(bigram$name),
                                   length(trigram$name),
                                   length(quadgram$name),
                                   length(pentagram$name))
                result.df <- ngram.df %>%
                        mutate(root.name = NA,
                               root.frequency = corpus.size,
                               ngram.length = sentence.length+1,
                               score = 0.4^(5-ngram.length)*(frequency/root.frequency))
                
                return(result.df)
                
        }
}


##----------------------------------------------------------------------------
## Model
##----------------------------------------------------------------------------
#Create very small test set initially - only a couple of lines


#input string of words and remove punctuation, etc
sentence <- "I always go the"

sentence <- sentence %>%
        tokens(remove_punct = TRUE,
               remove_numbers = TRUE,
               remove_symbols = TRUE,
               remove_url = TRUE,
               remove_twitter = TRUE
               ) %>%
                tokens_tolower()
#tokens_select(stopwords('english'),selection='remove')
sentence <- paste(sentence[[1]],collapse=" ")
print(sentence)

#determine string length
sentence.length <- wordcount(sentence)

#if over 4, truncuate to last 4 words
if (sentence.length >4) sentence <- word(sentence, start = sentence.length-3, end = sentence.length)

#determine number of words
iterations <-wordcount(sentence)+1

#run function
search.terms <- generate.search.terms(sentence)

output.df <- NULL
for(i in 1:iterations){

df <- retrieve.candidates(search.terms[i,1],search.terms[i,2],search.terms[i,3])
output.df <- rbind(output.df,df)
}

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


