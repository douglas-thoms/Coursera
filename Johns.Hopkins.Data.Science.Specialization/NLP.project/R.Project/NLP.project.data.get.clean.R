##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
##
##  File name:  NLP.project.data.get.clean
##  Date:       15OCT2019
##
##  Script to get and clean data
##  
##
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------

##----------------------------------------------------------------------------
## Functions
##----------------------------------------------------------------------------

#read lines from a text file, sampling them using rbinom
get.lines <- function(df,type.info) {
        con <- file(as.character(df[type.info,2]),'rb')
        x <- 0
        #tmp2 <- as.character()
        
        for(i in 1:df[type.info,1]){
                tmp <- readLines(con, 1, encoding = "UTF-8", skipNul = TRUE)
                
                if(rbinom(1,1,sample.rate) & length(tmp)){
                        x <- x + 1
                        if(x == 1) tmp2 <- tmp else tmp2 <- c(tmp2,tmp)
                        
                }
                
        }
        close(con)
        return(as.character(tmp2))
}

#creates corpus from large matrix, adds URL and source name is docvars
create.corpus <- function(input,text_name,file,URL){
        
        output <- corpus(input, docnames = rep(text_name,length(input)))
        docvars(output, "source") <- as.character(file)
        docvars(output, "URL") <- as.character(URL)
        return(output)
}

#create matrix of statistics
get.stats <- function(dfm){
        return(data.frame(num.features = nfeat(dfm),
                          sparsity = sparsity(dfm),
                          num.docs = ndoc(dfm)))
               
}

##----------------------------------------------------------------------------
## Libraries, data and system
##----------------------------------------------------------------------------

session.info.list <- sessionInfo()

library(quanteda)
library(dplyr)
library(ggplot2)
library(lexicon)
library(stringr)
library(ngram)

data(profanity_zac_anger)
data(grady_augmented)


#for reproducibility, same randomness
set.seed(3353)


home.directory <- getwd()
data.directory <- paste(home.directory,"data", sep = "/")
training.data.file.path <- paste(data.directory,"Coursera-SwiftKey.zip",                                                          
                           sep = "/")

dir.create(data.directory, showWarnings = FALSE)

##----------------------------------------------------------------------------
## Acquiredata and Clean
##---------------------------------------------------------------------------

#download profanity list

training.data.loc <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"


#download and unzip file if not present
if(!file.exists(training.data.file.path)){
download.file(training.data.loc, training.data.file.path)

training.data.date <- date()


unzip(training.data.file.path, exdir = data.directory)

}

#determine sample - assume normal distribution
# 95% confidence interval
# Sample Size Calculation:
#         Sample Size = (Distribution of 50%) / ((Margin of Error% / Confidence Level Score)Squared)
# Finite Population Correction:
#         True Sample = (Sample Size X Population) / (Sample Size + Population – 1)


#create data frame of 3 different files info
input.info.df <- data.frame(
        num.lines = c(1010274, 899289, 2360149),
        path = c('.//data//final//en_US//en_US.news.txt',
                 './/data//final//en_US//en_US.blogs.txt',
                 './/data//final//en_US//en_US.twitter.txt'),
        names = c('news','blogs','twitter')
)


#determine sample rate to use
news.sample <- (0.5 * (1-0.5))/((.05/2.576)^2)
news.sample.size <- (news.sample * input.info.df[1,1])/(news.sample + input.info.df[1,1] - 1)

blogs.sample <- (0.5 * (1-0.5))/((.05/2.576)^2)
blogs.sample.size <- (blogs.sample * input.info.df[2,1])/(blogs.sample + input.info.df[2,1] - 1)

twitter.sample <- (0.5 * (1-0.5))/((.05/2.576)^2)
twitter.sample.size <- (twitter.sample * input.info.df[3,1])/(twitter.sample + input.info.df[3,1] - 1)

sample.rate = 2000/input.info.df[2,1]*10
#use sample of 2000

#use function to read lines from text file
news<- get.lines(input.info.df,1)
blogs<- get.lines(input.info.df,2)
twitter<- get.lines(input.info.df,3)

#create corpus from matrices

news.corpus <- create.corpus(news,"news.sample","en_US.news.txt",
                             "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip")
blogs.corpus <- create.corpus(blogs,"blogs.sample", "en_US.blogs.txt",
                              "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip")
twitter.corpus <- create.corpus(twitter, "twitter.sampletex", "en_US.twitter.txt",
                                "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip")

#create one corpus
total.corpus <- corpus(news.corpus) + corpus(blogs.corpus) + corpus(twitter.corpus)



##----------------------------------------------------------------------------
## Clean Data/Tokenization/Ngrams
##----------------------------------------------------------------------------


#create dictionary of words to exclude
#add profane from lexicon
dict.profane <- dictionary(list(profanity = profanity_zac_anger))

#create regex expression to exclude
dict.regex <- dictionary(list(at.mark = "[@!#%&()*+./<=>_]",
                        number = "[0-9]-[0-9]"
))

#add names - US and stuff
#See if these others work later
dict.english <- dictionary(list(grady = grady_augmented
                                ))

#use lexicon grady_augmented and profane

#make do with worstemming for now, if need lemitization use later

#create tokens
#remove profane, stopwords and non-words
total.tokens <- total.corpus %>%
                tokens(remove_punct = TRUE, 
                     remove_numbers = TRUE) %>%
                #tokens_select(stopwords('english'),selection='remove') %>%
                tokens_select(dict.regex, selection = 'remove', valuetype = "regex") %>%
                tokens_select(dict.profane, selection = 'remove', valuetype = "fixed")

#see frequency and types of words
dfm <- dfm(total.tokens)
frequency.words <- textstat_frequency(dfm)
n.feat.non.word <- nfeat(dfm)

#remove non-english words
total.tokens <- tokens_select(total.tokens,dict.english, selection = 'keep', valuetype = "fixed")

#see frequency and types of words        
english.dfm <- dfm(total.tokens) 
frequency.eng.words <- textstat_frequency(english.dfm)
n.feat.english.word <- nfeat(english.dfm)


#tokens("New York City is located in the United States.") %>%
#tokens_compound(pattern = phrase(c("New York City", "United States")))

#reduce features but uncapitalizing and word stem
total.tokens <- total.tokens %>%
                #tokens_wordstem() %>%
                tokens_tolower()

num.tokens <- sum(ntoken(total.tokens))

#see frequency and type of words
words.dfm <- dfm(total.tokens)
frequency.word.stem <- textstat_frequency(words.dfm)
n.feat.words <- nfeat(words.dfm)

bigram.dfm <- dfm(tokens_ngrams(total.tokens,2))
frequency.bi <- textstat_frequency(bigram.dfm)
n.feat.bigram.word <- nfeat(bigram.dfm)

trigram.dfm <- dfm(tokens_ngrams(total.tokens,3))        
frequency.tri <- textstat_frequency(trigram.dfm)
n.feat.trigram.word <- nfeat(trigram.dfm)

ngram.dfm <- dfm(tokens_ngrams(total.tokens,1:5))


#head(kwic(total.tokens, "love", window = 3))


##----------------------------------------------------------------------------
## Explore Data, creat N-grams
##----------------------------------------------------------------------------



#find raw number of feature



#partition testing set - 20%

##----------------------------------------------------------------------------
## Model
##----------------------------------------------------------------------------
#Create very small test set initially - only a couple of lines

#determine string length
#input string of words
sentence <- "You're the reason why I smile everyday. Can you follow me please? It would mean the"
sentence.length <- wordcount(sentence)
#if over 4, truncuate to last 4 words
if (sentence.length >4) sentence <- word(sentence, start = sentence.length-4, end = sentence.length)

sentence.w.under <- gsub(" ", "_", sentence)

#add in underscore, add wildcard * to last work
sentence.prep <- paste("(","^",sentence.w.under,"_",")","|",
                       "(","^",sentence.w.under,"$",")",sep = "")

#regex is (^tell_me_.*)|(^tell_me$)
#check if ngram is observed

#search for n-1gram with *wildcard, if observed, move on
#if not drop first word in 4gram
#while there are less than 2 features bigram will be shortened
df.select <- dfm_select(ngram.dfm,pattern = sentence.prep, valuetype = "regex")
while (nfeat(df.select) < 2){
        sentence.w.under <- gsub( "^[^_]*_","",sentence.w.under)
        sentence.prep <- paste("(","^",sentence.w.under,"_",")","|",
                               "(","^",sentence.w.under,"$",")",sep = "")
        #print(sentence)
        #print(sentence.prep)
        
        df.select <- dfm_select(ngram.dfm,pattern = sentence.prep, valuetype = "regex")
        
        if (grepl("_",sentence.w.under) == FALSE) {
                
                print(paste("Error: \'", sentence.w.under, "\' not in vocabulary"))
                break 
        }
        
}



#else calculate frequency of all groups
#apply stupid back off
#Need to insert missing part, add regex - something like remove first
#n words based on ngram

df.select2 <- df.select %>%
        convert(to = "data.frame") %>%
        select(-document) %>%
        summarise_all(sum) %>%
        t() 


df.select2 <- data.frame(ngram = row.names(df.select2), df.select2)
df.select2 <- transform(df.select2, ngram = gsub("_"," ",ngram))

temp <- strsplit(df.select2$ngram,split=" ")
df.select2$ngram.type <- sapply(temp,length)

df.select2 <- rename(df.select2,frequency = df.select2)
df.select2 <- mutate(df.select2,score = 0.4^(5-ngram.type)*(frequency/max(frequency)))
#last.word = str_extract(ngram, "(\\S+)$"))



root.ngram <- filter(df.select2,ngram.type == min(ngram.type))[1,3]

df.select2 <- mutate(df.select2,last.word = word(ngram,start=min(ngram.type+1),end = ngram.type))
#"(\\s[a-z]*){2}$"
answer <- df.select2 %>%
        filter(frequency !=max(frequency)) %>%
        filter(score == max(score))
print(answer[1,5])

#use stupid back off model
#alpha = 0.4
#based it on 5-gram model
#then use kneser mayer as comparison


