library(dplyr)
library(ggplot2)

test.corpus <- corpus_sample(total.corpus,size = 1000)



test.dfm <- dfm(test.corpus,
                tolower = TRUE, stem = FALSE, remove_punct = TRUE,
                remove = stopwords("english"))

test.dfm <- dfm_trim(test.dfm, min_termfreq = 1)

features_dfm_inaug <- textstat_frequency(test.dfm)

# Sort by reverse frequency order
features_dfm_inaug$feature <- with(features_dfm_inaug, reorder(feature, -frequency))

a <- ggplot(features_dfm_inaug, aes(x = feature, y = frequency)) +
        geom_point() + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot(a)

#to create percentage graph
#first create textstate_frequency df, then reverse with  lowest rank at top
#calculate total words and percentage rows based on that row and lower

features_dfm_inaug <- features_dfm_inaug %>% 
        arrange(rank) %>%
        mutate(cusum.words = cumsum(frequency)) %>%
        mutate(feature.counter = 1) %>%
        mutate(cusum.feature = cumsum(feature.counter)) %>%
        mutate(total.words = sum(frequency)) %>%
        mutate(total.words.per = (cusum.words/total.words)*100) %>%
        arrange(-total.words.per)
        #row_number())

b <- ggplot(features_dfm_inaug,aes(x=total.words.per,y=cusum.feature)) +
        geom_point() +
        scale_x_reverse(name = "Total words(%)")

plot(b)