library(dplyr)

x <- c("fish" = 5, "tree" = 6)
y <- c("fish" = 4, "cheers" =7)

vector <- y

if (!exists("frequency.df"))
{
        frequency.df <- data.frame(vector)
        frequency.df$names <- rownames(frequency.df)
        frequency.df <- frequency.df %>%
                select(names, everything())
} else {
        temp <- data.frame(vector)
        temp$names <- rownames(temp)
        temp <- temp %>%
                select(names, everything())
        frequency.df <- full_join(frequency.df,temp, by = "names")
        frequency.df[is.na(frequency.df)] <- 0
}