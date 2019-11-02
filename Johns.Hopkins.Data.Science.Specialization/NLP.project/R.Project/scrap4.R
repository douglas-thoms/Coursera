library(dplyr)
library(quanteda)
library(stringr)


#determine string length
#input string of words
sentence <- "tell me something"
#if over 4, truncuate to last 4 words
#check if as unigrams in dictionary
sentence.w.under <- gsub(" ", "_", sentence)

#add in underscore, add wildcard * to last work
sentence.prep <- paste("(","^",sentence.w.under,"_",")","|",
                       "(","^",sentence.w.under,"$",")",sep = "")

#(^tell_me_.*)|(^tell_me$)
#check if ngram is observed

#search for n-1gram with *wildcard, if observed, move on
#if not drop first word in  5gram
#while there are less than 2 features bigram will be shortened
df.select <- dfm_select(ngram.dfm,pattern = sentence.prep, valuetype = "regex")
while (nfeat(df.select) < 2){
        sentence.w.under <- gsub( "^[^_]*_","",sentence.w.under)
        sentence.prep <- paste("(","^",sentence.w.under,"_",")","|",
                               "(","^",sentence.w.under,"$",")",sep = "")
        print(sentence)
        print(sentence.prep)
        
        df.select <- dfm_select(ngram.dfm,pattern = sentence.prep, valuetype = "regex")
        
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
df.select2 <- mutate(df.select2,score = 0.4^(5-ngram.type)*(frequency/max(frequency)),
                     last.word = str_extract(ngram, "(\\S+)$"))
df.select2 <- filter(df.select2,frequency !=max(frequency))

answer <- filter(df.select2,score == max(score))[1,5]
print(answer)

#use stupid back off model
#alpha = 0.4
#based it on 5-gram model
#then use kneser mayer as comparison

