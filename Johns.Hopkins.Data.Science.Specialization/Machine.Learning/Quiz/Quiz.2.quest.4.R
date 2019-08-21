library(dplyr)
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

trainingIL <- training[grep("^IL.*",names(training))]



procTrain <- preProcess(trainingIL, method="pca", thresh = 0.9)
procTrain