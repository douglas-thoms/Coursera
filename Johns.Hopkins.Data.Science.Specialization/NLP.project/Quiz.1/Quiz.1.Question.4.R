# #input and sample news

library(stringr)

twitter.lines <- 2360149


twitter.con <- file('C:/Users/dthoms/Documents/Training/Coursera/Johns.Hopkins.Data.Science.Specialization/NLP.project/R.Project/data/final/en_US/en_US.twitter.txt','rb')
twitter.love <- 0
twitter.hate <- 0
for(i in 1:twitter.lines){
        tmp <- readLines(twitter.con, 1, encoding = "UTF-8", skipNul = TRUE)
        if (length(tmp)){
                if (grepl("love",tmp)) twitter.love <- twitter.love + 1 
                if (grepl("hate",tmp)) twitter.hate <- twitter.hate + 1 
        }
}
close(twitter.con)
answer = twitter.love/twitter.hate