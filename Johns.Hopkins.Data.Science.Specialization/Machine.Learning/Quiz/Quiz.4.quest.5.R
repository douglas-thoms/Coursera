set.seed(3523)

library(AppliedPredictiveModeling)

data(concrete)

library(caret)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[inTrain,]

testing = concrete[-inTrain,]

set.seed(325)

library(e1071)

mod.obj <- svm(CompressiveStrength ~., data = training)

pred <- predict(mod.obj,testing)

library(Metrics)

RMSE <- rmse(testing$CompressiveStrength,pred)