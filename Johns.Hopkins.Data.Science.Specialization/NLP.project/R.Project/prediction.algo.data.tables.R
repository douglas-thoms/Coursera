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
library(data.table)

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
        output <- data.frame(name = output,stringsAsFactors = FALSE)
        # 
        # #regex example ^friend$
        # outputDT[, root.search := paste("^",name,"$",sep="")] 
        # 
        # #regex example "^friend_{1}"
        # outputDT[, combo.search := paste("^",name,"_{1}",sep="")]
        
        output <- output %>%
                #regex example ^friend$
                mutate(root.search = paste("^",name,"$",sep="")) %>%
                #regex example "^friend_{1}"
                mutate(combo.search = paste("^",name,"_{1}",sep=""))
        
        #add blank row
        output <- rbind(output, list("","",""))
        
        return(output)
}

retrieve.candidates <- function(name,root.search,combo.search){
        #get sentence level
        sentence.length <- wordcount(name, sep = "_")
        ngram.length <- sentence.length + 1
        
        #load appropriate corpus of ngrams
        if(sentence.length == 4) {
                ngramDT <- as.data.table(pentagram)
                rootDT <- as.data.table(quadgram)
                
        }else if(sentence.length == 3) {
                ngramDT <- as.data.table(quadgram)
                rootDT <- as.data.table(trigram)
                
        }else if(sentence.length == 2) {
                ngramDT <- as.data.table(trigram)
                rootDT <- as.data.table(bigram)
        }else if(sentence.length == 1) {
                ngramDT <- as.data.table(bigram)
                rootDT <- as.data.table(unigram)
        }else {
                ngramDT <- as.data.table(unigram)
        }
        
        #search for ngram candidates on bigrams and up, using stupid backoff recursion
        if(sentence.length >= 1){

                #filter according to regex
                
                
                resultDT <- ngramDT[name %like% combo.search]
                
                # resultDT <- ngramDT %>%
                #         filter(grepl(combo.search,name))
                
                
                #if found result
                if(length(resultDT$name)){
                        
                        result.rootDT <- rootDT[name %like% root.search]
                        
                        #filter to regex
                        # result.rootDT <- rootDT %>%
                        # filter(grepl(root.search,name))
                        
                        #find root frequency
                        root.frequency <- result.rootDT$frequency[[1]]
                        
                        resultDT[, `:=` (root.name = name, root.frequency = root.frequency,
                                ngram.length = ngram.length,
                                score= 0.4^(5-ngram.length)*(frequency/root.frequency))]
                        
                        #calculate score using stupid backoff model
                        # resultDT <- resultDT %>%
                        #         mutate(root.name = name,
                        #                root.frequency = root.frequency,
                        #                ngram.length = sentence.length + 1,
                        #                score = 0.4^(5-ngram.length)*(frequency/root.frequency))
                
                        return(resultDT)
                }
        
        #calculate final recursion level for stupid backoff model
        } else if(sentence.length == 0){
                #get total corpus size
                root.frequency <- sum(length(unigram$name),
                                   length(bigram$name),
                                   length(trigram$name),
                                   length(quadgram$name),
                                   length(pentagram$name))
                
                resultDT <- ngramDT[, `:=` (root.name = NA, root.frequency = root.frequency, 
                                            ngram.length = sentence.length+1, 
                                            score = 0.4^(5-ngram.length)*(frequency/root.frequency))]
                
                # resultDT <- ngramDT %>%
                #         mutate(root.name = NA,
                #                root.frequency = corpus.size,
                #                ngram.length = 0,
                #                score = 0.4^(5-ngram.length)*(frequency/root.frequency))
                return(resultDT)
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

outputDT <- NULL
for(i in 1:iterations){

DT <- retrieve.candidates(search.terms[i,1],search.terms[i,2],search.terms[i,3])
outputDT <- rbind(outputDT,DT)
}

#consolidate ngrams with same last words, combine values
#first create last word column

outputDT[, output.word := word(outputDT$name, start=outputDT$ngram.length,
                             sep = "_")]

# output.df <- mutate(output.df,output.word = 
#                             word(output.df$name,
#                                  start=output.df$ngram.length,
#                                  sep = "_"))

final.outputDT <- outputDT

#summarise different values according to last word
final.outputDT <- final.outputDT[, .(score = sum(score)), by = output.word]
final.outputDT <- final.outputDT[!(output.word %in% stopwords("english"))]
final.outputDT <- final.outputDT[,.(output.word,score)]

setorder(final.outputDT,-score)

# final.output.df <- output.df %>%
#         group_by(output.word) %>%
#         summarise(score = sum(score)) %>%
#         ungroup %>%
#         #remove stop words
#         filter(!(output.word %in% stopwords("english"))) %>%
#         arrange(desc(score))

print(paste("outputDT observations are",length(outputDT$name)))
print(paste("Most likely answer is \'",final.outputDT[1,1],"\'.", sep = ""))


