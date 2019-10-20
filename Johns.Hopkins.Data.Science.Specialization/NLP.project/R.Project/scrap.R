sample.rate <- 0.01

input.info.df <- data.frame(
num.lines = c(1010274, 899289, 2360149),
path = c('./data/final/en_US/en_US.news.txt',
              './data/final/en_US//en_US.blogs.txt',
              './data/final/en_US/en_US.twitter.txt'),
names = c('news','blogs','twitter'),
row.names = c('news','blogs','twitter'),
stringsAsFactors = FALSE
)


get.lines <- function(df,type.info) {
        con <- file(df[type.info,2],'rb')
        x <- 0
        tmp2 <- as.character()
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

x<- get.lines(input.info.df,2)