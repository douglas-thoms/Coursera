library(ggplot2)
library(dplyr)
library(quanteda)

#subset for source
#dfm <- dfm_subset(words.dfm, source == "en_US.news.txt")

#find top features
topfeatures <- topfeatures(words.dfm,n = 40)

dfm <- words.dfm
#create word cloud - all three sources
textplot_wordcloud(dfm,max_words = 120)

#test.dfm <- dfm_trim(dfm, min_termfreq = 2)

#get frequency of words
features <- textstat_frequency(dfm) 

#see how distribution of features in bar chart - what number of features
#appear once, twice, etc

distribution.features <- features %>%
            aggregate(by = list(features$frequency), FUN = length) %>%
            rename(frequency.of.word = Group.1) %>%
            rename(number.features = frequency) %>%
            select(frequency.of.word,number.features)

a <- ggplot(distribution.features, aes(x = number.features, y = frequency.of.word)) +
        geom_point() + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot(a)           

#as can see, majority are one time words and often not real words

head(distribution.features, n = 50)



# Sort by reverse frequency order
features$feature <- with(features, reorder(feature, -frequency))

b <- ggplot(features, aes(x = feature, y = frequency)) +
        geom_point() + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot(b)

#to create percentage graph
#first create textstate_frequency df, then reverse with  lowest rank at top
#calculate total words and percentage rows based on that row and lower

features <- features %>% 
        arrange(rank) %>%
        mutate(cusum.words = cumsum(frequency)) %>%
        mutate(feature.counter = 1) %>%
        mutate(cusum.feature = cumsum(feature.counter)) %>%
        mutate(total.words = sum(frequency)) %>%
        mutate(total.words.per = (cusum.words/total.words)*100) %>%
        arrange(-total.words.per)
#row_number())

c <- ggplot(features,aes(x=total.words.per,y=cusum.feature)) +
        geom_point() +
        scale_x_reverse(name = "Total words(%)")

plot(c)


#create denodram