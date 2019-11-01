library(dplyr)
library(quanteda)

#input string of words
sentence <- "last year"
#count number of words in sentence
num.words <- sapply(strsplit(sentence, " "),length)
#add in underscore
sentence.under <- gsub(" ", "_", sentence)
#get n-1gram - end of sentence till underscore
ngram.one.smaller <- gsub( "_{1}.[^_]*$","",sentence.under)

dict1 <- dictionary(list(search = paste(sentence.under,"*",sep="")))
dict2 <- dictionary(paste(ngram.one.smaller,"*",sep=""))



#use stupid back off model
#alpha = 0.4
#based it on 5-gram model
#then use kneser mayer as comparison
#need to calculate unigram total, not total of bigram

#set up small test sample
df <-   trigram.dfm %>%
        dfm_subset(bigram.dfm[1:300]) %>%
        dfm_select(trigram.dfm, dict1,valuetype = "glob")
         # #step, summarize bigram features into count
         # convert(to = "data.frame") %>%
         # select(-document) %>%
         # summarise_all(sum) %>%
         # select(contains(sentence.under)) %>%
         # t()

df <- data.frame(bigram = row.names(df), df)

# df1 <-  unigram.dfm %>%
#         dfm_subset(unigram.dfm[1:300]) %>%
#         dfm_lookup(dict2)
#         #step, summarize bigram features into count
#         convert(to = "data.frame")# %>%
       # select(-document) # %>%
       # summarise_all(sum) # %>%
       # select(contains(ngram.one.smaller)) #%>%
       # t()

#df1 <- data.frame(unigram = row.names(df1), df1)

#
# df <- df %>%
#         rename(frequency = df) %>%
#         mutate(total = sum(frequency)) %>%
#         mutate(percent = frequency/total) %>%
#         arrange(desc(percent))
#
# result <- df[1,1]
#need to take out word
