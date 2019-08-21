library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

trainingIL <- training[,grep("^IL|diagnosis", names(training))]
testingIL2 <- testing[c(1,grep("^IL.*",names(testing)))]

#Non-PCA model





#PCA Model
#determine PCA to use to reach .8
procTrain <- preProcess(trainingIL[,-13], method="pca", thres = 0.8)

#calculate PCA data set
trainPC <- predict(procTrain,trainingIL[,-13])
testPC <- predict(procTrain,testingIL[,-13])

#apply to model

model <- train(diagnosis ~ ., data = trainingIL, method = "glm")
predict_model <- predict(model, newdata = testingIL)
results <- confusionMatrix(predict(model,testingIL),testingIL$diagnosis)


modelFitPC <- train(diagnosis ~ ., method = "glm", data=trainPC)
resultsPC <- confusionMatrix(testingIL$diagnosis,predict(modelFitPC,testPC))

#procTest <- preProcess(trainingIL[,-1], method="pca", pcaComp = 7)




#predictionsPCA <- predict(modelFit,procTest)

#resultsPCA <- confusionMatrix(procTest$diagnosis,predict(modelFit,procTest))