library(dplyr)
library(quanteda)

#use stupid back off model
#alpha = 0.4
#need to calculate unigram total, not total of bigram

#set up small test sample
df <-   bigram.dfm %>%
        dfm_subset(bigram.dfm[1:300]) %>%
        #step, summarize bigram features into count
        convert(to = "data.frame") %>%
        select(-document) %>%
        summarise_all(sum) %>%
        select(matches("^last_*.")) %>%
        t()
        
df <- data.frame(bigram = row.names(df), df)

df <- df %>%
        rename(frequency = df) %>%
        mutate(total = sum(frequency)) %>%
        mutate(percent = frequency/total) %>%
        arrange(desc(percent))

result <- df[1,1]
#need to take out word    
        


        
        
