library(ngram)
library(dplyr)
library(stringr)

#start with sentence cleaned
#break into 4*3 dataframe, rows quad,tri,bi,uni
#columns actual ngram, root search, n+1 root search

sentence <- "I like"
n.words <- wordcount(sentence)
sentence <- gsub(" ", "_", sentence)

#bigram recursion set up
#bigram is already set up
#regex expression is "[\\w]_like$"

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



#trigram recursion
#get three types of ngrams - preceeding.ngram (* like cheese), preceeding bigram (* like),
#proceeding.bigram (like *)

#step1
#like_cheese
#determine all potential candidates
#regex is "I_like_* 

#step2
#find all *_like_cheese candidates

#regex is "*_like_cheese$" for trigram

#step 3
#remove last word with word count
#regex is "*_like" for bigram

#step 4
#remove last word with word count
#regex is "like_*" for bigram


        
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
                       preceeding.ngram.regex = paste("*_",name,"$",sep=""),
                       #set up lower ngram name
                       lower.ngram = word(name, start = 1, end = ngram.length - 1, sep = ""),
                       #regex is "*_like" for n-1 ngram
                       preceeding.lower.ngram = paste("^*_",lower.ngram,"$",sep=""),
                       #regex is "like_* for n-1 ngram
                       proceeding.lower.ngram = paste("^",lower.ngram,"_{1}",sep=""))
        



#already cut down to max 

#first vector - acutal ngram


