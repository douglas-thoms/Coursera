library(ggplot2)
library(dplyr)
library(quanteda)

dfm <- trigram.dfm

#compare against different sources
#subset for source
#dfm <- dfm_subset(words.dfm, source == "en_US.news.txt")

#find top features
topfeatures <- topfeatures(dfm,n = 40)

dfm <- dfm

textplot_wordcloud(dfm,max_words = 120)

dfm.trunct <- dfm_trim(dfm, min_termfreq = 1)

#get frequency of words
features <- textstat_frequency(dfm) 
features.trunct <- textstat_frequency(dfm.trunct)

#see how distribution of features in bar chart - what number of features
#appear once, twice, etc

distribution.features <- features %>%
            aggregate(by = list(features$frequency), FUN = length) %>%
            rename(frequency.of.word = Group.1) %>%
            rename(number.features = frequency) %>%
            select(frequency.of.word,number.features)

a <- ggplot(distribution.features, aes(x = number.features, y = frequency.of.word)) +
        geom_point() + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        coord_cartesian(ylim = c(0,50))

plot(a)             

#as can see, majority are one time words and often not real words

head(distribution.features, n = 50)



# Sort by reverse frequency order
# features.trunct$feature <- with(features.trunct, reorder(feature, -frequency))
# 
# b <- ggplot(features.trunct, aes(x = feature, y = frequency)) +
#         geom_point() + 
#         theme(axis.text.x=element_blank()) #make this blank
# 
# plot(b)

#to create percentage graph
#first create textstate_frequency df, then reverse with  lowest rank at top
#calculate total words and percentage rows based on that row and lower

# features.trunct <- features.trunct %>% 
#         arrange(rank) %>%
#         mutate(cusum.words = cumsum(frequency)) %>%
#         mutate(feature.counter = 1) %>%
#         mutate(cusum.feature = cumsum(feature.counter)) %>%
#         mutate(total.words = sum(frequency)) %>%
#         mutate(total.words.per = (cusum.words/total.words)*100) %>%
#         arrange(-total.words.per)
# #row_number())
# 
# c <- ggplot(features.trunct,aes(x=total.words.per,y=cusum.feature)) +
#         geom_point() +
#         scale_x_reverse(name = "Total words(%)")
# 
# plot(c)


#create denodram


dfm.trunct2 <- dfm_trim(dfm, min_termfreq = 100)

# hierarchical clustering - get distances on normalized dfm
tstat_dist <- textstat_dist(dfm.trunct2, margin = "features")
# hiarchical clustering the distance object
pres_cluster <- hclust(as.dist(tstat_dist))
# label with document names
#pres_cluster$labels <- features(test.dfm)
# plot as a dendrogram
plot(pres_cluster, xlab = "", sub = "",
     main = "Euclidean Distance on Normalized Token Frequency")