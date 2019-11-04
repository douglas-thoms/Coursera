##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
##
##  File name:  WordPredictor.R
##  Date:       04NOV2019
##
##  Uses word.vocab.builder.R to predict the next word
##  
##
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------

##----------------------------------------------------------------------------
## Library, Environment
##----------------------------------------------------------------------------

session.info.list <- sessionInfo()

library(quanteda)
library(dplyr)
library(ggplot2)
library(lexicon)
library(stringr)
library(ngram)

##----------------------------------------------------------------------------
## Get vocabulary
##----------------------------------------------------------------------------

#if statement to call vocab if dfm not generated

#Call ngram.dfm for vocabulary
if (!exists("ngram.dfm")) {
        print("constructing variable ngram.dfm")
        source("word.vocab.builder.R")
        #ngram.dfm <- get.data()
}
        
##----------------------------------------------------------------------------
## Model
##----------------------------------------------------------------------------
#Create very small test set initially - only a couple of lines

#determine string length
#input string of words
sentence <- "If this isn't the cutest thing you've ever seen, then you must be"
print(sentence)
sentence.length <- wordcount(sentence)
#if over 4, truncuate to last 4 words
if (sentence.length >4) sentence <- word(sentence, start = sentence.length-4, end = sentence.length)

sentence.w.under <- gsub(" ", "_", sentence)

#add in underscore, add wildcard * to last work
sentence.prep <- paste("(","^",sentence.w.under,"_",")","|",
                       "(","^",sentence.w.under,"$",")",sep = "")

#regex is (^tell_me_.*)|(^tell_me$)
#check if ngram is observed

#search for n-1gram with *wildcard, if observed, move on
#if not drop first word in 4gram
#while there are less than 2 features bigram will be shortened
df.select <- dfm_select(ngram.dfm,pattern = sentence.prep, valuetype = "regex")
while (nfeat(df.select) < 2){
        sentence.w.under <- gsub( "^[^_]*_","",sentence.w.under)
        sentence.prep <- paste("(","^",sentence.w.under,"_",")","|",
                               "(","^",sentence.w.under,"$",")",sep = "")
        #print(sentence)
        #print(sentence.prep)
        
        df.select <- dfm_select(ngram.dfm,pattern = sentence.prep, valuetype = "regex")
        
        if (grepl("_",sentence.w.under) == FALSE) {
                
                print(paste("Error: \'", sentence.w.under, "\' not in vocabulary"))
                break 
        }
        
}



#else calculate frequency of all groups
#apply stupid back off
#Need to insert missing part, add regex - something like remove first
#n words based on ngram

df.select2 <- df.select %>%
        convert(to = "data.frame") %>%
        select(-document) %>%
        summarise_all(sum) %>%
        t() 


df.select2 <- data.frame(ngram = row.names(df.select2), df.select2)
df.select2 <- transform(df.select2, ngram = gsub("_"," ",ngram))

temp <- strsplit(df.select2$ngram,split=" ")
df.select2$ngram.type <- sapply(temp,length)

df.select2 <- rename(df.select2,frequency = df.select2)
df.select2 <- mutate(df.select2,score = 0.4^(5-ngram.type)*(frequency/max(frequency)))
#last.word = str_extract(ngram, "(\\S+)$"))



root.ngram <- filter(df.select2,ngram.type == min(ngram.type))[1,3]

df.select2 <- mutate(df.select2,last.word = word(ngram,start=min(ngram.type+1),end = ngram.type))
#"(\\s[a-z]*){2}$"
answer <- df.select2 %>%
        filter(frequency !=max(frequency)) %>%
        filter(score == max(score))
print(answer[1,5])

#use stupid back off model
#alpha = 0.4
#based it on 5-gram model
#then use kneser mayer as comparison





