#put in for loop so all options are calculated and added into one df

for (i in iter.counter:0){
        
        print(i)
        if(i>1){
                print("other ngram")
                
                #demote ngram down one level
                ngram.search <- gsub( "^[^_]*_","",ngram.search)
                ngram.search <- paste("^",ngram.search,sep="")
                
                ngram.root.search <- gsub( "^[^_]*_","",ngram.root.search)
                ngram.root.search <- paste("^",ngram.root.search,sep="")
                print(paste("ngram.root.search is",ngram.root.search))
                
                sentence.length <- wordcount(sentence)
                sentence <- word(sentence,start=2,end=sentence.length)
                
                candidates.df <- retrieve.candidates(ngram.search, ngram.root.search)
                
                #get length of ngram
                candidates.df$ngram.length <- sapply(candidates.df$name,wordcount,sep = "_")
                
                root.frequency <- candidates.df %>%
                        arrange(ngram.length) %>%
                        filter(ngram.length == min(ngram.length))
                
                #use stupid back off model
                #based it on 5-gram model
                #alpha = 0.4
                #if at least final bigram in model, use following calculation
                
                candidates.df <- candidates.df %>%
                        filter(ngram.length != min(ngram.length)) %>%
                        mutate(score = 0.4^(5-ngram.length)*(frequency/root.frequency[1,2])) %>%
                
                output.df <- full_join(output.df,candidates.df)

                print(paste("candidates.df observations are",length(candidates.df$name)))
                
        }else if(i==0){
                print("looking for unigram")
                
                #get total corpus size
                corpus.size <- sum(length(unigram$name),
                                   length(bigram$name),
                                   length(trigram$name),
                                   length(quadgram$name),
                                   length(pentagram$name))
                
                candidates.df <- unigram
                
                candidates.df <- retrieve.candidates(ngram.search, ngram.root.search)
                
                candidates.df <- candidates.df %>%
                        filter(ngram.length != min(ngram.length)) %>%
                        mutate(score = frequency/corpus.size) %>%
                        filter(score == max(score))
                        
                output.df <- full_join(output.df,candidates.df)
                print(paste("candidates.df observations are",length(candidates.df$name)))
                
        }
}