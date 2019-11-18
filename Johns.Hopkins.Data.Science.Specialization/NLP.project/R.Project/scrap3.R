library(ngram)
library(dplyr)
library(stringr)

#start with sentence cleaned
#break into 4*3 dataframe, rows quad,tri,bi,uni
#columns actual ngram, root search, n+1 root search

sentence <- "I like cheese"
n.words <- wordcount(sentence)
sentence <- gsub(" ", "_", sentence)

#bigram recursion set up
#bigram is already set up
#regex expression is "[\\w]_like$"

#### - OK
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



#already cut down to max 4

#first vector - acutal ngram

output <- NULL

for(i in 1:n.words) {
        
        output <- c(output, word(sentence, start = i, end = n.words, sep = "_"))
}

#then use dplyr for other sections
output <- data.frame(name = output, stringsAsFactors = FALSE)
output <- output %>%
        mutate(preceding.word = paste("^",name,"$",sep="")) %>%
        mutate(combo.search = paste("^",sentence,"_{1}",sep=""))
