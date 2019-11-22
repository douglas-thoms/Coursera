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
                ngram.df <- unigram
        }

               

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
        #create function
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

retrieve.candidates <- function(name,preceeding.ngram.regex,preceeding.lower.ngram.regex,
                                proceeding.lower.ngram.regex){
        #get sentence level
        sentence.length <- wordcount(name, sep = "_")
        
        #load appropriate corpus of ngrams
        if(sentence.length == 4) {
                ngram.df <- pentagram
                lower.ngram <- quadgram
                
        }else if(sentence.length == 3) {
                ngram.df <- quadgram
                lower.ngram <- trigram
                
        }else if(sentence.length == 2) {
                ngram.df <- trigram
                lower.ngram <- bigram
        }else if(sentence.length == 1) {
                ngram.df <- bigram
                lower.ngram <- unigram
        }else {
                ngram.df <- unigram
        }
        
        #search for ngram candidates on bigrams and up, using stupid backoff recursion
        if(sentence.length >= 1){

                #filter according to regex
                num.type.preceeding.ngram <- ngram.df %>% filter(grepl(preceeding.ngram.regex,name)) 
                num.type.preceeding.lower.ngram <- ngram.df %>% filter(grepl(preceeding.lower.ngram.regex,name)) 
                num.type.proceeding.lower.ngram <- ngram.df %>% filter(grepl(proceeding.lower.ngram.regex,name)) 
                
                pkn <- c("num.type.preceeding.ngram" = length(num.type.preceeding.ngram$name),
                         "num.type.preceeding.lower.ngram" = length(num.type.preceeding.lower.ngram$name),
                         "num.type.proceeding.lower.ngram" = length(num.type.proceeding.lower.ngram$name))
                
                #calculate length of stuff
                
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
                               ngram.length = 0,
                               score = 0.4^(5-ngram.length)*(frequency/root.frequency))
                
                #### - OK - modelling at final level
                # #total frequency of bigrams
                # tot.freq <- sum(bigram$frequency)
                # tot.types <- length(bigram$name)
                # 
                # pkn <- bigram %>%
                #         filter(grepl(".*_like$",name)) %>%
                #         mutate(pkn = frequency/sum(tot.freq),
                #                pkn.cont = length(name)/tot.types)
                ####
                
        }
}


##----------------------------------------------------------------------------
## Model
##----------------------------------------------------------------------------
#Create very small test set initially - only a couple of lines


#input string of words and remove punctuation, etc
sentence <- "at the end"

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

#generate search terms
search.terms <- generate.search.terms(sentence)

candidates.df <- generate.candidates(search.terms[1,])


#NEED FUNCTION
#this function appends score to frame of all generated candidates
#step 1 search.terms function to generate required regex for:
#*_first_second - num.types (only unique)
#*_first$ (created at candidates)
#^first_{1} (created at candiates as universal)



#step 2 - collects all terms





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


