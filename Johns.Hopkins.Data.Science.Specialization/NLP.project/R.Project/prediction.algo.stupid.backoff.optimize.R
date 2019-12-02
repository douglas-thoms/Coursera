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
        output <- rbind(output, c("","",""))
        
        return(output)
}

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

                
                x <- merge(ngram.df,root.df)

                x <- x %>%
                        mutate(coefficient = 0.4^(5-ngram.length.predicted),
                               score = coefficient*(frequency/root.frequency))
                
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
                root.name <- result.root.df$name[[1]]
                
                #calculate score using stupid backoff model
                result.df <- result.df %>%
                        mutate(prediction = name,
                               root.name = root.name,
                               root.frequency = root.frequency,
                               ngram.length = ngram.length,
                               score = 0.4^(4-ngram.length)*(frequency/root.frequency)) %>%
                        select(-name) %>%
                        group_by(root.frequency) %>%
                        arrange(desc(score)) %>%
                        ungroup() %>%
                        top_n(5,score)
        
                return(result.df)
                }
        
        #calculate final recursion level for stupid backoff model
        } else if(ngram.length == 0){
                #get total corpus size
                corpus.size <- sum(length(unigram$name),
                                   length(bigram$name),
                                   length(trigram$name),
                                   length(quadgram$name),
                                   length(pentagram$name))
                result.df <- ngram.df %>%
                        mutate(root.name = NA,
                               root.frequency = corpus.size,
                               ngram.length = ngram.length,
                               score = 0.4^(5-ngram.length)*(frequency/root.frequency))
                
                return(result.df)
                
        }
}


##----------------------------------------------------------------------------
## Model
##----------------------------------------------------------------------------

#take vector of same n-grams in underscore format
#Create very small test set initially - only a couple of lines

ngram.length.predicted <- 4

#enter vector of ngram to calculate it scores
     
#use purr to avoid loop and count ngram.length

score.table <- data.frame(entry = c("I_want_to_see",
                                   "there_are_lots_of",
                                   "it_is_nice_to",
                                   "there_are_lots_to"
                                   ),
                          frequency = c(30,20,10,5), 
                          stringsAsFactors = FALSE)

score.table$root.name <- map_chr(score.table$entry,sub,pattern = "_*[a-zA-Z0-9]*$", replacement = "")

             

#NEED TO CREATE SCORES AND THEN MERGE WITH SCORE TABLE

# mtcars %>%
#         split(.$cyl) %>% 
#         map(~ lm(mpg ~ wt, data = .)) %>%
#         map(summary) %>%
#         map_dfr("r.squared")

#output needs to be predicted phrase, frequency, score
x <- retrieve.candidates(score.table, ngram.length.predicted = ngram.length.predicted)



#PSEUDO

#determine sentence length
#choose appropriate calculation
#calculate the score using root ngram and ngram tables

#determine string length
sentence.length <- wordcount(sentence, sep = "_")

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


