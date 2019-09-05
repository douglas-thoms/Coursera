download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv", 
              destfile = "C:/Users/dthoms/Documents/Training/Coursera/Johns.Hopkins.Data.Science.Specialization/Machine.Learning/Quiz/Desktop/gaData.csv")

library(lubridate) # For year() function below

dat = read.csv("~/Training/Coursera/Johns.Hopkins.Data.Science.Specialization/Machine.Learning/Quiz/Desktop/gaData.csv")

training = dat[year(dat$date) < 2012,]

testing = dat[(year(dat$date)) > 2011,]

tstrain = ts(training$visitsTumblr)

library(forecast)

model.obj <- bats(tstrain)

forecast <- forecast(model.obj,level = 95, h=dim(testing)[1])

#need to sum the number of times testing values is greater than low and less than
#high on forecast 

#use ifelse on a new column

comp.matrix <- data.frame(forecast$lower,
                           forecast$upper, visitsTumblr = testing$visitsTumblr)

comp.matrix$visitsTumblr <- as.numeric(comp.matrix$visitsTumblr)



library(dplyr)

comp.matrix <- comp.matrix %>%
        rename(lower = X95.) %>%
        rename(upper = X95..1) %>%
        mutate(within_95 = ifelse((visitsTumblr > lower) & (visitsTumblr < upper), 1,0))

percent <- sum(comp.matrix$within_95)/length(comp.matrix$within_95)