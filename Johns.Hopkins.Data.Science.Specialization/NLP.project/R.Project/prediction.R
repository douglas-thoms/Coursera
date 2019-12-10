##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
##
##  File name:  prediction.R
##  Date:       09DEC2019
##
##  Step 5 in process
##  Input phrase and outputs prediction
##  Set up as a function to output predictions and specificially work
##  with benchmark.R to quantify accuracy
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------

#Input is phrase used to predict ouput
prediction.function <-function(x){

        library(data.table)
        library(ngram)
        library(stringr)
        library(purrr)
        library(dplyr)
        library(quanteda)
        
        entry <- x
        
        #clean phrase and then recombine
        entry.cleaned <- x %>%
                tokens(remove_punct = TRUE,
                       remove_numbers = TRUE,
                       remove_symbols = TRUE,
                       remove_url = TRUE,
                       remove_twitter = TRUE
                ) %>%
                tokens_tolower()
        entry.cleaned <- paste(entry.cleaned[[1]],collapse=" ")
        print(entry.cleaned)
        
        #determine string length
        entry.cleaned.length <- wordcount(entry.cleaned)
        
        #if over 4, truncuate to last 4 words
        if (entry.cleaned.length >4) {
                entry.cleaned <- word(entry.cleaned, start = entry.cleaned.length-3, end = entry.cleaned.length)
                entry.cleaned.length <- 4
        }
        
        #changes blank spaces to hyphen to match look up tables
        entry.cleaned <- gsub(" ", "_", entry.cleaned)
        
        #create loop to break entry sentence into ngram to unigram
        output <- NULL
        
        for(i in 1:entry.cleaned.length) {
                #generate ngrams from blank to pentagram
                output <- c(output, word(entry.cleaned, start = i, end = entry.cleaned.length, sep = "_"))
        }
        
        output <- c(output,NA)
        
        #search for entries in score table that match phrase
        predictions <- NULL
        for(i in 1:entry.cleaned.length+1){
        
            df <- score.table[.(output[i]), nomatch = 0L]
            predictions <- rbind(predictions,df)
        
        }
        
        
        #get last word of entries and add as new column
        predictions <- predictions %>%
                        mutate(new.word = map_chr(name,word, start = -1, sep = "_")) %>%
                        group_by(new.word) %>%
                        summarise(score = sum(score)) %>%
                        ungroup %>%
                        arrange(desc(score)) %>%
                        mutate(predicted.sentence = paste(entry,new.word))
        #return just last word
        return(predictions$new.word)
        
        

}