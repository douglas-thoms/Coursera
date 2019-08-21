library(caret)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)

adData = data.frame(predictors, diagnosis)
trainIndex = createDataPartition(diagnosis, p = 0.5, list = FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]
