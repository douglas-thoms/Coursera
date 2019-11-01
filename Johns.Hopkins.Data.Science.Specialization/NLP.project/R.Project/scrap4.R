library(dplyr)
library(quanteda)



#determine string length
#input string of words
sentence <- "let us"
#count number of words in sentence
num.words <- sapply(strsplit(sentence, " "),length)
#if over 4, truncuate to last 4 words

#add in underscore, add wildcard * to last work
sentence.prep <- paste(gsub(" ", "_", sentence),"*",sep = "")


#check if ngram is observed
#combine all n-grams into one dfm
#search for n-1gram with *wildcard, if observed, get frequency
#else, drop last word, if observed, get frequency, continue
#calculate frequency of n-1gram, calculate frequency of all derivatives
#apply stupid back off


#get n-1gram - end of sentence till underscore
#ngram.one.smaller <- gsub( "_{1}.[^_]*$","*",sentence.under)

df.select <- dfm_select(df,pattern = ngram.one.smaller, valuetype = "glob")


#use stupid back off model
#alpha = 0.4
#based it on 5-gram model
#then use kneser mayer as comparison
#need to calculate unigram total, not total of bigram

#set up small test sample
df <-   trigram.dfm %>%
        dfm_subset(trigram.dfm[1:300]) %>%
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
