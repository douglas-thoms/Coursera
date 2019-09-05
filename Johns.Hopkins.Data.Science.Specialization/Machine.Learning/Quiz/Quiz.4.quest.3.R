set.seed(3523)

library(AppliedPredictiveModeling)
library(caret)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]

set.seed(233)

library(elasticnet)
library(glmnet)

fit_lasso <- train(CompressiveStrength~., data=training, method = "lasso")
a <- plot.enet(fit_lasso$finalModel, xvar="penalty", use.color = TRUE)
print(a)

