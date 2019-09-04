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

# lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = lambda)
# lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test,])
# mean((lasso.pred-ytest)^2)