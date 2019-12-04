##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
##
##  File name:  dfm.supplement.R
##  Date:       14Nov19
##
##  Step 2a in process
##  Supplemental in process
##  Input script is download.enter.corpus.R
##  Create supplemntal ngrams to be combined with swiftkey ngrams
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------

##----------------------------------------------------------------------------
## Create corpus
##----------------------------------------------------------------------------
library(dplyr)
library(quanteda)
library(readtext)
library(lexicon)
library(stringr)
library(ngram)

#load profane dataset
data(profanity_zac_anger)
#turn into dictionary
dict.profane <- dictionary(list(profanity = profanity_zac_anger))

#set up directory path
home.directory <- getwd()
data.directory <- paste(home.directory,"data", sep = "/")

#standford movies - multiple text files
data.dir <- paste(data.directory,"aclImdb/train",                                                          
                 sep = "/")
#read text of movie reviews
stanford.movies <- readtext(paste0(data.dir, "/*/*.txt"), encoding = "UTF-8")


#gutenberg partial - multiple text files
data.dir <- paste(data.directory,"Gutenberg/txt",                                                          
                  sep = "/")

#read partial text of guttenberg
partial.gutenberg <- readtext(paste0(data.dir, "/*.txt"), encoding = "UTF-8")

#change into corpus and combine into each
supp.corpus <- corpus(stanford.movies) + corpus(partial.gutenberg)

#tokenize and clean
toks <- supp.corpus %>%
        tokens(
                remove_punct = TRUE,
                remove_numbers = TRUE,
                remove_symbols = TRUE,
                remove_url = TRUE,
                remove_twitter = TRUE) %>%
        tokens_select(dict.profane, selection = 'remove', valuetype = "fixed") 

#create ngram
supp.dfm <- dfm(toks, ngrams = 3)

#remove lower frequency terms
supp.dfm.trim <- dfm_trim(supp.dfm, min_termfreq = 4)

#sum columns and create a vector totalling each factors frequency
vector <- sort(colSums(supp.dfm.trim),decreasing = TRUE)

#create a dataframe with ngram name and frequency
frequency.df <- data.frame(vector,stringsAsFactors = FALSE)
frequency.df$name <- rownames(frequency.df)
frequency.df <- frequency.df %>%
                select(name, everything()) %>%
                rename(frequency = vector)

#count number of words to feature
#frequency.df$ngram.length <- sapply(frequency.df$name,wordcount,sep = "_")

#create RDS file to input later
saveRDS(frequency.df,"data/trigram.supp.rds")

#input to test if same as expected
test <- readRDS("data/trigram.supp.rds")