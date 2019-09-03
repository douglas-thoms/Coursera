library(ElemStatLearn)

data(vowel.train)
vowel.train$y <- as.factor(vowel.train$y)
data(vowel.test)
vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)

library(caret)

# Set the variable y to be a factor variable in both the training and test set. 
# Then set the seed to 33833. Fit (1) a random forest predictor relating 
# the factor variable y to the remaining variables and (2) a boosted predictor 
# using the "gbm" method. Fit these both with the 
# train() command in the caret package.

fit1 <- train(y ~., method="rf", data = vowel.train)
pred1 <- predict(fit1,vowel.test)
result1 <- confusionMatrix(pred1,vowel.test$y)


fit2 <- train(y ~., method="gbm", data = vowel.train)
pred2 <- predict(fit2,vowel.test)
result2 <- confusionMatrix(pred2,vowel.test$y)

a <- qplot(pred1,pred2,colour=y,data=vowel.test)
print(a)

#create dataframe of pred1 = pred 2 = vowel.test$y -> how many compared to all
#of vowel.test$y
