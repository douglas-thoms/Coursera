library(AppliedPredictiveModeling)
library(dplyr)
data(segmentationOriginal)
library(caret)
library(rpart)
library(rattle)
set.seed(125)

#Create test training datasets
training <- filter(segmentationOriginal, Case == "Train")
#testing <- filter(segmentationOriginal, Case == "Test")
TotalIntench2 = c(23000,50000,57000)
FiberWidthCh1 = c(10,10,8)


testing <- data.frame(TotalIntench2, FiberWidthCh1)

fit <- train(Class ~., data = training, method = "rpart")

plot(fit$finalModel, uniform = TRUE, main = "Classification Tree")
text(fit$finalModel, use.n = TRUE, all = TRUE, cex =.8)
fancyRpartPlot(fit$finalModel)
prediction <- predict(fit,newdata = testing)




#predicted value is class
#fancyRpartPlot