test.corpus <- corpus_sample(total.corpus,size = 100)



test.dfm <- dfm(test.corpus,
              tolower = TRUE, stem = FALSE, remove_punct = TRUE,
              remove = stopwords("english"))

test.dfm <- dfm_trim(test.dfm, min_termfreq = 5)

# hierarchical clustering - get distances on normalized dfm
tstat_dist <- textstat_dist(test.dfm, margin = "features")
# hiarchical clustering the distance object
pres_cluster <- hclust(as.dist(tstat_dist))
# label with document names
#pres_cluster$labels <- features(test.dfm)
# plot as a dendrogram
plot(pres_cluster, xlab = "", sub = "",
     main = "Euclidean Distance on Normalized Token Frequency")