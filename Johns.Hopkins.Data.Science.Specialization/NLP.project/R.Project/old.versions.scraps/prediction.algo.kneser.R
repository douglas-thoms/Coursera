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

        output <- NULL
        ngram.length <- NULL
        filter <- NULL
        filter2 <- NULL
        
        for(i in 1:n.words) {
                #generate ngrams from blank to pentagram
                output <- c(output,word(sentence, start = i, end = n.words, sep = "_"))
                ngram.length <- c(ngram.length, n.words + 1 - i)
                filter <- c(filter,paste("^",word(sentence, start = i, end = n.words, sep = "_"),"_{1}",sep=""))
                filter2 <- c(filter2,paste("_",word(sentence, start = i, end = n.words, sep = "_"),"_{1}",sep=""))
        }
        
        output <- c(output,"")
        ngram.length <- c(ngram.length,0)
        filter <- c(filter,"")
        filter2 <- c(filter2,"")
        
        #create dataframe
        output <- data.frame(name = output,
                             ngram.length = ngram.length, 
                             filter = filter,
                             filter2 = filter2,
                             stringsAsFactors = FALSE)
        
        # output <- output %>%
        #         #regex example is "*_like_cheese$" for ngram
        #         mutate(#choose candidates,
        #                 candidate.regex = paste("^",name,"_*", sep = ""),
        #                 lower.ngram.regex = paste("^",name,"$", sep = ""),
        #                 preceeding.ngram.regex = paste("_",name,"_{1}",sep=""),
        #                 #set up lower ngram name
        #                 #create regex
        #                 lower.ngram.root = word(name, start = 1, end = ngram.length - 1, sep = "_"),
        #                 #regex is "*_like" for n-1 ngram
        #                 preceeding.lower.ngram = paste("_",lower.ngram.root,"$",sep=""),
        #                 #regex is "like_ for n-1 ngram
        #                 proceeding.lower.ngram = paste("^",lower.ngram.root,"_{1}",sep=""))
        
        #first.recursion.reduction <-word(sentence,start=n.words,sep="_")
        
        #add blank row

        
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

        if(search.terms$ngram.length == 0){
        
                        preceeding.ngram <- function(search.term,ngrams){
                        
                        search.term <- paste("_",search.term,"$",sep = "")
                        
                        test.df <- ngrams %>%
                                filter(grepl(search.term,name))
                        
                        return(length(test.df$name)) 
                }
                
                        #too many candidates to process so 1) only take top 20 unigrams 
                        #2) only bigrams related to larger ngrams
                
                ngrams <- ngram.df
                ngrams$ngram.length <- sapply(ngrams$name,wordcount,sep="_")
                #create.numer.name to match numer values
                df <- higher.ngram %>%
                        transform(name =  sub("^[a-zA-Z]*_{1}","",name),
                               number = 1)
                
                df <- aggregate(number ~ name, data = df, sum)
                df <- rename(df,numer = number)

                ngrams <- merge(ngrams,df)
                denom <- length(ngram.df$name)
                
                ngrams <- ngrams %>% 
                        mutate(d = 0,
                               lower.regex = NA,
                               prev.pkn.cont = 0,
                               unknown_smoothing = 0,
                               pkn = frequency/sum(frequency),
                               pkn.cont = numer/denom
                               
                               )
                
                print("first round")
                        
                return(ngrams)
                        
        } else if(n.words - search.terms$ngram.length > 0){
                
                preceeding.ngram <- function(search.term,ngrams){
                        
                        search.term <- paste("_",search.term,"$",sep = "")
                        
                        test.df <- ngrams %>%
                                filter(grepl(search.term,name))
                        
                        return(length(test.df$name)) 
                }
                
                
                #ngrams$ngram.length <- sapply(ngrams$name, wordcount, sep = "_")
                
                #get ngrams following preceeding word
                ngrams <- ngram.df
                ngrams <- ngrams %>% 
                        filter(grepl(search.terms$filter,name))
                
                ngrams$ngram.length <- sapply(ngrams$name,wordcount,sep="_")
                #ngrams$numer <- sapply(ngram.df$name,preceeding.ngram,ngrams = higher.ngram)
                #create.numer.name to match numer values
                df <- higher.ngram %>%
                        filter(grepl(search.terms$filter2,name)) %>%
                        transform(name =  sub("^[a-zA-Z]*_{1}","",name),
                                  number = 1)
                
                df <- aggregate(number ~ name, data = df, sum)
                df <- rename(df,numer = number)
                
                ngrams <- merge(ngrams,df)
                denom <- preceeding.ngram(search.terms$name, ngrams = ngram.df)
                
                
                
                proceeding.ngram <- function(search.term,ngrams){
                        
                        search.term <- paste("^",search.term,"_{1}",sep = "")
                        
                        test.df <- ngrams %>%
                                filter(grepl(search.term,name))
                        
                        return(length(test.df$name)) 
                }
                
                #start here - same for all rows
                proceeding.type <- proceeding.ngram(search.terms$name,ngrams = ngram.df)
                
                root.freq <- filter(lower.ngram,lower.ngram$name == search.terms$name)[[2]]
                pkn.cont <- filter(output.df,ngram.length == search.terms$ngram.length)
                
                n1 <- filter(ngram.df,frequency == 1)
                n1 <- length(n1$name)
                n2 <- filter(ngram.df,frequency == 2)
                n2 <- length(n2$name)
                
                ngrams <- ngrams %>% 
                        
                        #right now d doesn't calculate because removed all 1 frequency, 2 frequency ngrams
                        mutate(
                               d = 1, # n1/(n1 + 2*n2),
                               lower.regex = paste("^",sub("^[a-zA-Z]*_{1}","",name),"$",sep = "")
                               )
                
                find.prev.pkn.cont <- function(lower.regex, pkn.cont){
                                               
                                                prev.pkn.cont <- pkn.cont[grepl(lower.regex,pkn.cont$name), 9]
                                                
                                                return(prev.pkn.cont)
                                                }
                
                ngrams$prev.pkn.cont <- sapply(ngrams$lower.regex,find.prev.pkn.cont,pkn.cont = pkn.cont)
               
                ngrams <- ngrams %>%  
                        mutate(
                               unknown_smoothing = d*proceeding.type/denom*(d/denom)*prev.pkn.cont,
                               pkn =  pmax(frequency,0)/root.freq + unknown_smoothing,
                               pkn.cont = pmax(numer,0)/denom + unknown_smoothing
                               )
                
                print("middle")
                
                #use these to add pkn.cont value
                #y <- mutate(output.df, fr = output.df[grepl("_are_you_today",output.df$name), 4])
                #y <- mutate(df, fr = output.df[1,5]+output.df[1,2])
                #y <- mutate(output.df, fr = output.df[output.df$name == "fish", 4])
                
                return(ngrams)
                
        }
        
        else {             
                preceeding.ngram <- function(search.term,ngrams){
                        
                        search.term <- paste("_",search.term,"$",sep = "")
                        
                        test.df <- ngrams %>%
                                filter(grepl(search.term,name))
                        
                        return(length(test.df$name)) 
                }
                
                
                #ngrams$ngram.length <- sapply(ngrams$name, wordcount, sep = "_")
                
                #get ngrams following preceeding word
                ngrams <- ngram.df
                
                ngrams$ngram.length <- sapply(ngrams$name,wordcount,sep="_")
                ngrams$numer <- 0
                denom <- preceeding.ngram(search.terms$name, ngrams = ngram.df)
                
                proceeding.ngram <- function(search.term,ngrams){
                        
                        search.term <- paste("^",search.term,"_{1}",sep = "")
                        
                        test.df <- ngrams %>%
                                filter(grepl(search.term,name))
                        
                        return(length(test.df$name)) 
                }
                
                #start here - same for all rows
                proceeding.type <- proceeding.ngram(search.terms$name,ngrams = ngram.df)
                
                n1 <- filter(ngram.df,frequency == 1)
                n1 <- length(n1$name)
                n2 <- filter(ngram.df,frequency == 2)
                n2 <- length(n2$name)
                
                root.freq <- filter(lower.ngram,lower.ngram$name == search.terms$name)[[2]]
                pkn.cont <- filter(output.df,ngram.length == search.terms$ngram.length)
                
                ngrams <- ngrams %>% 
                        #NEED filter out non-candidates that don't match
                        filter(grepl(search.terms$filter,name)) %>%
                        mutate(
                                d = n1/(n1 + 2*n2),
                                lower.regex = paste("^",sub("^[a-zA-Z]*_{1}","",name),"$",sep = "")
                        )
                
                find.prev.pkn.cont <- function(lower.regex, pkn.cont){
                        
                        prev.pkn.cont <- pkn.cont[grepl(lower.regex,pkn.cont$name), 9]
                        
                        return(prev.pkn.cont)
                        
                }
                
                ngrams$prev.pkn.cont <- sapply(ngrams$lower.regex,find.prev.pkn.cont,pkn.cont = pkn.cont)
                
                ngrams <- ngrams %>%  
                        mutate(
                                unknown_smoothing = d*proceeding.type/denom*(d/denom)*prev.pkn.cont,
                                pkn = pmax(frequency,0)/root.freq + unknown_smoothing,
                                pkn.cont = 0
                               )
                
                print("final")
                
                return(ngrams)
                
                
                
        #         #get frequency of lower.ngram
        #         lower.ngram.freq <- lower.ngram %>% 
        #                  filter(grepl(search.terms$lower.ngram.regex,name)) 
        #         
        #         #get number of unique values for preceeding.lower.ngram
        #         preceeding.lower.ngram.uni.val <-length(lower.ngram[grepl(search.terms$preceeding.lower.ngram,lower.ngram$name),]$name)
        #         
        #         #get number of unique values for proceeding.lower.ngram
        #         proceeding.lower.ngram.uni.val <-length(lower.ngram[grepl(search.terms$proceeding.lower.ngram,lower.ngram$name),]$name)
        #         
        #         #alternate way to get values
        #         #preceeding.lower.ngram <- lower.ngram %>%
        #         #        filter(grepl("*_there$",name))
        #         #length <- length(preceeding.lower.ngram$name)
        # 
        #         #can I improve this line?
        #         lower.ngram.count <- lower.ngram.freq$frequency[[1]]
        #                 
        #         candidates.df <- ngram.df %>% 
        #                 #find count of predicted ngrams
        #                 filter(grepl(search.terms$candidate.regex,name)) %>%
        #                 #find count of lower ngram
        #                 mutate(lower.ngram.name = sentence,
        #                        lower.ngram.freq = lower.ngram.count,
        #                        #find number of type of ngrams, preceeding lower ngram
        #                        preceeding.lower.ngram.uni.val = preceeding.lower.ngram.uni.val,
        #                        #find number of type ngrams proceeding lower ngram
        #                        proceeding.lower.ngram.uni.val = proceeding.lower.ngram.uni.val
        #                        )
        #         
        #         #narrow set of ngrams for preceeding n.gram
        #         ngrams <- higher.ngram %>%
        #                 filter(grepl(search.terms$preceeding.ngram.regex,name))
        # 
        #         #get frequency for preceeding.ngram
        #         preceeding.ngram <- function(search.term,ngrams){
        #                 
        #                 search.term <- paste("*_",search.term,"$",sep = "")
        #                 
        #                 print(search.term)
        #                 
        #                 test.df <- ngrams %>%
        #                         filter(grepl(search.term,name))
        #                 
        #                 return(length(test.df$name)) 
        #         }
        # 
        # candidates.df$test <- sapply(candidates.df$name,preceeding.ngram,ngrams=ngrams)
        # 
        # return(candidates.df)
        }
}

##----------------------------------------------------------------------------
## Model
##----------------------------------------------------------------------------
#Create very small test set initially - only a couple of lines


#input string of words and remove punctuation, etc
sentence <- "the film is"

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
sentence <- gsub(" ", "_", sentence)

#determine number of words
iterations <-n.words + 1

#generate search terms
search.terms <- generate.search.terms(sentence)

# #NEEDcreate tokens of small parapgragh
# #sample data to make it more mangeable building
# unigram <- data.frame(name = c("today","there","fish","fowl","you"),
#                       freq = c(5,2,1,1,4),
#                       stringsAsFactors = FALSE)
# bigram <- data.frame(name = c("you_today","you_there","you_fish", "hey_you","are_you"),
#                       freq = c(5,2,1,3,1),
#                       stringsAsFactors = FALSE)
# trigram <- data.frame(name = c("are_you_today","are_you_there"),
#                        freq = c(5,2),
#                        stringsAsFactors = FALSE)
# quadgram <- data.frame(name = c("how_are_you_today","how_are_you_there"),
#                         freq = c(5,2),
#                         stringsAsFactors = FALSE)
# pentagram <- data.frame(name = c("Hello_how_are_you_today","Hello_how_are_you_there"),
#                         freq = c(5,2),
#                         stringsAsFactors = FALSE)


#calculate d value

# total.ngrams <- rbind(unigram,bigram,trigram,quadgram,pentagram)
# 
# n1 <- total.ngrams %>%
#         filter(frequency == 1)
# 
# n1 <- length(n1$name)
# 
# n2 <- total.ngrams %>%
#         filter(frequency == 2)
# 
# n2 <- length(n2$name)
# 
# n3 <- total.ngrams %>%
#         filter(frequency == 3)
# 
# n3 <- length(n3$name)
# 
# n4 <- total.ngrams %>%
#         filter(frequency == 4)
# 
# n4 <- length(n4$name)
# 
# y <- n1/(n1+2*n2)
# d0 <- 0
# d1 <- 1-2*y*(n2/n1)
# d2 <- 2-3*y*(n3/n2)
# d3 <- 3-4*y*(n4/n3)

output.df <- NULL
pkn.cont <- NULL
for(i in iterations:1){
        
df <- generate.candidates(search.terms[i,])
output.df <- rbind(output.df,df)
print(i)
#output.df <- output.df %>% transform(pkn = if_else(pkn.cont != 0, pkn.cont,pkn))
}

print("got here")


#consolidate ngrams with same last words, combine values
#first create last word column
output.df <- mutate(output.df,output.word = 
                            word(output.df$name,
                                 start=output.df$ngram.length,
                                 sep = "_"))

#summarise different values according to last word
final.output.df <- output.df %>%
        group_by(output.word) %>%
        summarise(pkn = sum(pkn)) %>%
        ungroup %>%
        #remove stop words
        filter(!(output.word %in% stopwords("english"))) %>%
        arrange(desc(pkn))

print(paste("output.df observations are",length(output.df$name)))
print(paste("Most likely answer is \'",final.output.df[1,1],"\'.", sep = ""))


