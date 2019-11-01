library(dplyr)
library(quanteda)
library(ngram)



#determine string length
#input string of words
sentence <- "let use tree monkey"
#if over 4, truncuate to last 4 words

#add in underscore, add wildcard * to last work
sentence.prep <- paste(gsub(" ", "_", sentence),"*",sep = "")


#check if ngram is observed

#search for n-1gram with *wildcard, if observed, move on
#if not drop first word in  5gram
#while there are less than 2 features bigram will be shortened
df.select <- dfm_select(ngram.dfm,pattern = sentence.prep, valuetype = "glob")
while (nfeat(df.select) < 2){
        sentence.prep <- gsub( "^[^_]*_","",sentence.prep)
        print(sentence.prep)
        df.select <- dfm_select(ngram.dfm,pattern = sentence.prep, valuetype = "glob")
        
}

#

#else calculate frequency of all groups
#apply stupid back off


df.select2 <- df.select %>%
             convert(to = "data.frame") %>%
             select(-document) %>%
             summarise_all(sum) %>%
             t() 
  

df.select2 <- data.frame(ngram = row.names(df.select2), df.select2)
df.select2 <- transform(df.select2, ngram = gsub("_"," ",ngram))

temp <- strsplit(df.select2$ngram,split=" ")
df.select2$ngram.type <- sapply(temp,length)

#use stupid back off model
#alpha = 0.4
#based it on 5-gram model
#then use kneser mayer as comparison

