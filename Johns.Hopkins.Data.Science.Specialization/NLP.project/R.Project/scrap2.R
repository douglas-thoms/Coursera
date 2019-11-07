start <- date()
session.info.list <- sessionInfo()

library(quanteda)
library(dplyr)
library(ggplot2)
library(lexicon)
library(stringr)
library(ngram)


##----------------------------------------------------------------------------
## Model
##----------------------------------------------------------------------------
#Create very small test set initially - only a couple of lines

#determine string length
#input string of words
sentence <- "Go on a romantic date at the"
sentence <- sentence %>%
        tokens(remove_punct = TRUE,
               remove_numbers = TRUE,
               remove_symbols = TRUE,
               remove_url = TRUE,
               remove_twitter = TRUE)# %>%
#tokens_select(stopwords('english'),selection='remove')
sentence <- paste(sentence[[1]],collapse=" ")
print(sentence)
sentence.length <- wordcount(sentence)



#if over 4, truncuate to last 4 words
if (sentence.length >4) sentence <- word(sentence, start = sentence.length-4, end = sentence.length)

sentence.w.under <- gsub(" ", "_", sentence)
#MAKE SIMPLE JUST CUT DOWN TO LAST WORD
#add in underscore, add wildcard * to last work
sentence.prep <- paste("(","^",sentence.w.under,"_",")","|",
                       "(","^",sentence.w.under,"$",")",sep = "")

#regex is (^tell_me_.*)|(^tell_me$)
#check if ngram is observed

#search for n-1gram with *wildcard, if observed, move on
#if not drop first word in 4gram
#while there are less than 2 features bigram will be shortened

#select with grepl
select.vocab.l <- grepl(sentence.prep,vocabulary$)
        
df.select <- dfm_select(ngram.trim,pattern = sentence.prep, valuetype = "regex")
while (nfeat(df.select) < 2){
        sentence.w.under <- gsub( "^[^_]*_","",sentence.w.under)
        sentence.prep <- paste("(^",sentence.w.under,"_[a-z]*$)|(^",sentence.w.under,"$)"
                               ,sep = "")
        print(sentence)
        print(sentence.prep)
        
        #select file

        
        if (grepl("_",sentence.w.under) == FALSE){
                #create top features subset of one

                break
        }
        
}




