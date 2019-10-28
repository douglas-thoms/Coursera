library(ggplot2)
library(dplyr)
library(quanteda)

#subset for source
#dfm <- dfm_subset(words.dfm, source == "en_US.news.txt")
dfm <- words.dfm
#create word cloud
textplot_wordcloud(dfm,max_words = 120)
#create two bar charts


test.dfm <- dfm_trim(dfm, min_termfreq = 2)

features.dfm <- textstat_frequency(dfm)

# Sort by reverse frequency order
features.dfm$feature <- with(features.dfm, reorder(feature, -frequency))

a <- ggplot(features.dfm, aes(x = feature, y = frequency)) +
        geom_point() + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot(a)

#to create percentage graph
#first create textstate_frequency df, then reverse with  lowest rank at top
#calculate total words and percentage rows based on that row and lower

features.dfm <- features.dfm %>% 
        arrange(rank) %>%
        mutate(cusum.words = cumsum(frequency)) %>%
        mutate(feature.counter = 1) %>%
        mutate(cusum.feature = cumsum(feature.counter)) %>%
        mutate(total.words = sum(frequency)) %>%
        mutate(total.words.per = (cusum.words/total.words)*100) %>%
        arrange(-total.words.per)
#row_number())

b <- ggplot(features.dfm,aes(x=total.words.per,y=cusum.feature)) +
        geom_point() +
        scale_x_reverse(name = "Total words(%)")

plot(b)


#create denodram