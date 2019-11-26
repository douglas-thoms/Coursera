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
        ngram.length <- NULL
        
        for(i in 1:n.words) {
                #generate ngrams from blank to pentagram
                output <- c(output,word(sentence, start = i, end = n.words, sep = "_"))
                ngram.length <- c(ngram.length, n.words + 1 - i)
        }
        
        #create dataframe
        output <- data.frame(name = output,
                             ngram.length = ngram.length, 
                             stringsAsFactors = FALSE)
        
        output <- output %>%
                #regex example is "*_like_cheese$" for ngram
                mutate(#choose candidates,
                        candidate.regex = paste("^",name,"_*", sep = ""),
                        lower.ngram.regex = paste("^",name,"$", sep = ""),
                        preceeding.ngram.regex = paste("_",name,"_{1}",sep=""),
                        #set up lower ngram name
                        #create regex
                        lower.ngram.root = word(name, start = 1, end = ngram.length - 1, sep = "_"),
                        #regex is "*_like" for n-1 ngram
                        preceeding.lower.ngram = paste("_",lower.ngram.root,"$",sep=""),
                        #regex is "like_ for n-1 ngram
                        proceeding.lower.ngram = paste("^",lower.ngram.root,"_{1}",sep=""))
        
        first.recursion.reduction <-word(sentence,start=n.words,sep="_")
        
        #add blank row
        output <- rbind(output, c("","",first.recursion.reduction,
                                             paste("^",first.recursion.reduction,"_{1}",sep="")))
        
        return(output)
}

#generate data frame of candidates and run generate.search.terms on each
#step 1, find proper ngram vocab based on sentence length +1
#step 2 filter
generate.candidates <- function(search.terms){
        #get sentence level
        
        #load appropriate corpus of ngrams
        if(search.terms$ngram.length == 4) {
                ngram.df <- pentagram
                lower.ngram <- quadgram
                
        }else if(search.terms$ngram.length == 3) {
                higher.ngram <- pentagram
                ngram.df <- quadgram
                lower.ngram <- trigram
                
        }else if(search.terms$ngram.length == 2) {
                higher.ngram <- quadgram
                ngram.df <- trigram
                lower.ngram <- bigram
        
        }else if(search.terms$ngram.length == 1) {
                higher.ngram <- trigram
                ngram.df <- bigram
                lower.ngram <- unigram
        }else {
                higher.ngram <- bigram
                ngram.df <- unigram
        }

        if(wordcount(search.terms$name, sep = "_") == 0){

                 
      
                        preceeding.ngram <- function(search.term,ngrams){
                        
                        search.term <- paste("_",search.term,"$",sep = "")
                        
                        test.df <- higher.ngram %>%
                                filter(grepl(search.term,name))
                        
                        return(length(test.df$name)) 
                }
                
                ngrams <- ngram.df
                
                ngrams$unique.preceeding.types <- sapply(ngrams$name,preceeding.ngram,ngrams=ngram.df)
                
                pkn.cont <- ngrams %>% 
                        mutate(pkn.cont = unique.preceeding.types/length(ngram.df$name),
                               pkn = 0)
                
                print("first round")
                        
                return(pkn.cont)
                        
        } else if(n.words - wordcount(search.terms$name, sep = "_") > 1){
                
                preceeding.ngram <- function(search.term,ngrams){
                        
                        search.term <- paste("_",search.term,"$",sep = "")
                        
                        test.df <- higher.ngram %>%
                                filter(grepl(search.term,name))
                        
                        return(length(test.df$name)) 
                }
                
                ngrams <- ngram.df
                
                ngrams$unique.preceeding.types <- sapply(ngrams$name,preceeding.ngram,ngrams=ngram.df)
                
                pkn.cont <- ngrams %>% 
                        #need to update calculations to match formula
                        mutate(pkn.cont = unique.preceeding.types/length(ngram.df$name),
                               pkn = 0)
                
                print("middle")
                
                return(pkn.cont)
                
        }
        
        else {             

                preceeding.ngram <- function(search.term,ngrams){
                        
                        search.term <- paste("_",search.term,"$",sep = "")
                        
                        test.df <- higher.ngram %>%
                                filter(grepl(search.term,name))
                        
                        return(length(test.df$name)) 
                }
                
                ngrams <- ngram.df
                
                ngrams$unique.preceeding.types <- sapply(ngrams$name,preceeding.ngram,ngrams=ngram.df)
                
                pkn.cont <- ngrams %>% 
                        mutate(pkn.cont = unique.preceeding.types/length(ngram.df$name),
                               pkn = 0)
                
                print("final")
                
                return(pkn.cont)
                
                #get frequency of lower.ngram
                lower.ngram.freq <- lower.ngram %>% 
                         filter(grepl(search.terms$lower.ngram.regex,name)) 
                
                #get number of unique values for preceeding.lower.ngram
                preceeding.lower.ngram.uni.val <-length(lower.ngram[grepl(search.terms$preceeding.lower.ngram,lower.ngram$name),]$name)
                
                #get number of unique values for proceeding.lower.ngram
                proceeding.lower.ngram.uni.val <-length(lower.ngram[grepl(search.terms$proceeding.lower.ngram,lower.ngram$name),]$name)
                
                #alternate way to get values
                #preceeding.lower.ngram <- lower.ngram %>%
                #        filter(grepl("*_there$",name))
                #length <- length(preceeding.lower.ngram$name)
        
                #can I improve this line?
                lower.ngram.count <- lower.ngram.freq$frequency[[1]]
                        
                candidates.df <- ngram.df %>% 
                        #find count of predicted ngrams
                        filter(grepl(search.terms$candidate.regex,name)) %>%
                        #find count of lower ngram
                        mutate(lower.ngram.name = sentence,
                               lower.ngram.freq = lower.ngram.count,
                               #find number of type of ngrams, preceeding lower ngram
                               preceeding.lower.ngram.uni.val = preceeding.lower.ngram.uni.val,
                               #find number of type ngrams proceeding lower ngram
                               proceeding.lower.ngram.uni.val = proceeding.lower.ngram.uni.val
                               )
                
                #narrow set of ngrams for preceeding n.gram
                ngrams <- higher.ngram %>%
                        filter(grepl(search.terms$preceeding.ngram.regex,name))
        
                #get frequency for preceeding.ngram
                preceeding.ngram <- function(search.term,ngrams){
                        
                        search.term <- paste("*_",search.term,"$",sep = "")
                        
                        print(search.term)
                        
                        test.df <- ngrams %>%
                                filter(grepl(search.term,name))
                        
                        return(length(test.df$name)) 
                }
        
        candidates.df$test <- sapply(candidates.df$name,preceeding.ngram,ngrams=ngrams)
        
        return(candidates.df)
        }
}

##----------------------------------------------------------------------------
## Model
##----------------------------------------------------------------------------
#Create very small test set initially - only a couple of lines


#input string of words and remove punctuation, etc
sentence <- "Hello, how are you"

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

#n.words for model
n.words <- wordcount(sentence)

#determine number of words
iterations <-wordcount(sentence)+1

#generate search terms
search.terms <- generate.search.terms(sentence)


#sample data to make it more mangeable building
unigram <- data.frame(name = c("today","there","fish","fowl"),
                      freq = c(5,2,1,1),
                      stringsAsFactors = FALSE)
bigram <- data.frame(name = c("you_today","you_there","you_fish"),
                      freq = c(5,2,1),
                      stringsAsFactors = FALSE)
trigram <- data.frame(name = c("are_you_today","are_you_there"),
                       freq = c(5,2),
                       stringsAsFactors = FALSE)
quadgram <- data.frame(name = c("how_are_you_today","how_are_you_there"),
                        freq = c(5,2),
                        stringsAsFactors = FALSE)
pentagram <- data.frame(name = c("Hello_how_are_you_today","Hello_how_are_you_there"),
                        freq = c(5,2),
                        stringsAsFactors = FALSE)

output.df <- NULL
pkn.cont <- NULL
for(i in iterations:1){
        
df <- generate.candidates(search.terms[i,])
output.df <- rbind(output.df,df)
print(i)
output.df <- output.df %>% transform(pkn = if_else(pkn.cont != 0, pkn.cont,pkn))
}



for(i in 1:iterations){

df <- retrieve.candidates(search.terms[i,1],search.terms[i,2],search.terms[i,3])
output.df <- rbind(output.df,df[[1]])
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


