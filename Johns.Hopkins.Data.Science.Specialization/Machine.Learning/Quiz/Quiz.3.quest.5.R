
rm(list=ls())
library(ElemStatLearn)


data(vowel.train)
data(vowel.test)


vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)

library(randomForest)
library(caret)

model <- randomForest(y ~ ., data = vowel.train)
#[NOTE: Use randomForest() specifically, not caret, as there's been some issues reported with that approach. 11/6/2016]

x <- varImp(model)


 
 x$name <- rownames(x)
 
 y <- x[order(-x$Overall),c(1,2)]