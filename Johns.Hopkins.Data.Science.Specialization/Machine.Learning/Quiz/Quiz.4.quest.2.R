library(caret)

library(gbm)

set.seed(3433)

library(AppliedPredictiveModeling)

data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)

inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]

training = adData[ inTrain,]
training$diagnosis <- as.factor(training$diagnosis)

testing = adData[-inTrain,]
testing$diagnosis <- as.factor(testing$diagnosis)

set.seed(62433)

fit_rf <- train(diagnosis ~., method="rf", data = training)
pred_rf <- predict(fit_rf, newdata = testing)
result_rf <- confusionMatrix(pred_rf, testing$diagnosis)
result_per_rf <- result_rf$overall

fit_gbm <- train(diagnosis ~., method="gbm", data = training, verbose=FALSE)
pred_gbm <- predict(fit_gbm, newdata = testing)
result_gbm <- confusionMatrix(pred_gbm, testing$diagnosis)
result_per_gbm <- result_gbm$overall

fit_lda <- train(diagnosis ~., method="lda", data = training)
pred_lda <- predict(fit_lda, newdata = testing)
result_lda <- confusionMatrix(pred_lda, testing$diagnosis)
result_per_lda <- result_lda$overall

a <- qplot(pred_rf,pred_gbm, colour=diagnosis,data=testing)

print(a)

predDF <- data.frame(pred_rf, pred_gbm, pred_lda, testing$diagnosis)
fit_comb <- train(testing.diagnosis ~., method = "rf", data= predDF)
pred_comb <- predict(fit_comb,predDF)
result_comb <- confusionMatrix(pred_comb, testing$diagnosis)
result_per_comb <- result_comb$overall