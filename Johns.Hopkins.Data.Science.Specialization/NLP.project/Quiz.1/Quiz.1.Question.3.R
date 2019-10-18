# #input and sample news

library(stringr)

news.lines <- 1010274
blogs.lines <-  899289 
twitter.lines <- 2360149

news.con <- file('C:/Users/dthoms/Documents/Training/Coursera/Johns.Hopkins.Data.Science.Specialization/NLP.project/R.Project/data/final/en_US/en_US.news.txt','rb')
news.max <- 0
for(i in 1:news.lines){
        tmp <- readLines(news.con, 1, encoding = "UTF-8", skipNul = TRUE)
        if (length(tmp)){
                y <- str_length(tmp) 
                if(y > news.max) news.max <- y
        }
        
}
close(news.con)

blogs.con <- file('C:/Users/dthoms/Documents/Training/Coursera/Johns.Hopkins.Data.Science.Specialization/NLP.project/R.Project/data/final/en_US/en_US.blogs.txt','rb')
blogs.max <- 0
for(i in 1:blogs.lines){
        tmp <- readLines(blogs.con, 1, encoding = "UTF-8", skipNul = TRUE)
        if (length(tmp)){
                y <- str_length(tmp) 
                if(y > blogs.max) blogs.max <- y
        }
        
        
}
close(blogs.con)

twitter.con <- file('C:/Users/dthoms/Documents/Training/Coursera/Johns.Hopkins.Data.Science.Specialization/NLP.project/R.Project/data/final/en_US/en_US.twitter.txt','rb')
twitter.max <- 0
for(i in 1:twitter.lines){
        tmp <- readLines(twitter.con, 1, encoding = "UTF-8", skipNul = TRUE)
        if (length(tmp)){
                y <- str_length(tmp) 
                if(y > twitter.max) twitter.max <- y
        }
        
}
close(twitter.con)