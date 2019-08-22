library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

trainingIL <- training[,grep("^IL|diagnosis", names(training))]
testingIL <- testing[c(1,grep("^IL.*",names(testing)))]

#Non-PCA model





#PCA Model
#determine PCA to use to reach .8
pre.proc <- preProcess(trainingIL[,-13], method="pca", thres = 0.8)

#calculate PCA data set
trainPC <- predict(pre.proc,trainingIL[,-13])
testPC <- predict(pre.proc,testingIL[,-13])

#apply to model

model <- train(diagnosis ~ ., data = trainingIL, method = "glm")
results <- confusionMatrix(predict(model,testingIL),testingIL$diagnosis)


modelFitPC <- train(diagnosis ~ ., method = "glm", data=trainPC)
resultsPC <- confusionMatrix(testingIL$diagnosis,predict(modelFitPC,testPC))





#predictionsPCA <- predict(modelFit,procTest)

#resultsPCA <- confusionMatrix(procTest$diagnosis,predict(modelFit,procTest))