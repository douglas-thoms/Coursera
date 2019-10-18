# #input and sample news

library(stringr)

twitter.lines <- 2360149


twitter.con <- file('C:/Users/dthoms/Documents/Training/Coursera/Johns.Hopkins.Data.Science.Specialization/NLP.project/R.Project/data/final/en_US/en_US.twitter.txt','rb')
kickbox <- 0
for(i in 1:twitter.lines){
        tmp <- readLines(twitter.con, 1, encoding = "UTF-8", skipNul = TRUE)
        if (length(tmp)){
                if(grepl("^A computer once beat me at chess, but it was no match for me at kickboxing$", tmp)) {
                        kickbox <- kickbox + 1      
                } 
        }
        
}
close(twitter.con)