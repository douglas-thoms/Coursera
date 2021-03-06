##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
##
##  File name:  Week.4.Assignment.R
##  Date:       08SEP2019
##
##  Assignment for Machine Practical Learning
##  
##
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------

##----------------------------------------------------------------------------
## Background
##----------------------------------------------------------------------------

# Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible 
# to collect a large amount of data about personal activity relatively inexpensively. 
# These type of devices are part of the quantified self movement – a group of 
# enthusiasts who take measurements about themselves regularly to improve their health, 
# to find patterns in their behavior, or because they are tech geeks. One thing 
# that people regularly do is quantify how much of a particular activity they do, 
# but they rarely quantify how well they do it. In this project, your goal will be 
# to use data from accelerometers on the belt, forearm, arm, and dumbell of 
# 6 participants. They were asked to perform barbell lifts correctly and incorrectly 
# in 5 different ways. 

# More information is available from the website here: 
# http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har 
# (see the section on the Weight Lifting Exercise Dataset).


# The goal of your project is to predict the manner in which they did the exercise. 
# -This is the "classe" variable in the training set. 
# -You may use any of the other variables to predict with.

# You should create a report describing how: 
# -you built your model
# -how you used cross validation
# -what you think the expected out of sample error is and why you made the 
# choices you did

# You will also use your prediction model to predict 20 different test cases.

# Peer Review Portion
# Your submission for the Peer Review portion should consist of: 
# -a link to a Github repo with your R markdown 
# and compiled HTML file describing your analysis. 
# -constrain the text of the writeup to < 2000 words and the number of figures 
# to be less than 5. 
# -It will make it easier for the graders if you submit a repo 
# with a gh-pages branch so the HTML page can be viewed online 


# Course Project Prediction Quiz Portion
# -Apply your machine learning algorithm to the 20 test cases available in the 
# test data above and submit your predictions in appropriate format to the 
# Course Project Prediction Quiz for automated grading.

##----------------------------------------------------------------------------
## Library
##----------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(caret)
library(parallel)
library(doParallel)
library(earth)

##----------------------------------------------------------------------------
## Question
##----------------------------------------------------------------------------

#Predict classe using any of the other variables - how well they exercise?

##----------------------------------------------------------------------------
## Input Data
##----------------------------------------------------------------------------


# Weight Lifting Exercises Dataset
# 
# On-body sensing schema
# 
# The approach we propose for the Weight Lifting Exercises dataset is to investigate 
# "how (well)" an activity was performed by the wearer. The "how (well)" investigation 
# has only received little attention so far, even though it potentially provides 
# useful information for a large variety of applications,such as sports training.
# 
# In this work (see the paper) we first define quality of execution and investigate 
# three aspects that pertain to qualitative activity recognition: the problem of 
# specifying correct execution, the automatic and robust detection of execution mistakes, 
# and how to provide feedback on the quality of execution to the user. We tried 
# out an on-body sensing approach (dataset here), but also an "ambient sensing approach" 
# (by using Microsoft Kinect - dataset still unavailable)
# 
# Six young health participants were asked to perform one set of 10 repetitions 
# of the Unilateral Dumbbell Biceps Curl in five different fashions: exactly 
# according to the specification (Class A), throwing the elbows to the front (Class B), 
# lifting the dumbbell only halfway (Class C), lowering the dumbbell only halfway (Class D) 
# and throwing the hips to the front (Class E).
# 
# Class A corresponds to the specified execution of the exercise, while the other 
# 4 classes correspond to common mistakes. Participants were supervised by an 
# experienced weight lifter to make sure the execution complied to the manner 
# they were supposed to simulate. The exercises were performed by six male participants 
# aged between 20-28 years, with little weight lifting experience. We made sure that 
# all participants could easily simulate the mistakes in a safe and controlled manner 
# by using a relatively light dumbbell (1.25kg).

# The training data for this project are available here:
#         https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

# The test data are available here:
#         https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

# The data for this project come from this source: 
#         http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har. 
# If you use the document you create for this class for any purpose please cite them 
# as they have been very generous in allowing their data to be used for this kind of assignment.

if(!file.exists("training.csv")){
        download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", 
                      destfile = "C:/Users/Douglas/Documents/Coursera/Johns.Hopkins.Data.Science.Specialization/Machine.Learning/Assignment/training.csv")
}

#read sources and put in NA in blank
training = read.csv("C:/Users/Douglas/Documents/Coursera/Johns.Hopkins.Data.Science.Specialization/Machine.Learning/Assignment/training.csv",
                    na.strings=c("","NA"))

if(!file.exists("testing.csv")){
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", 
              destfile = "C:/Users/Douglas/Documents/Coursera/Johns.Hopkins.Data.Science.Specialization/Machine.Learning/Assignment/testing.csv")
}

#read sources and put in NA in blank
testing = read.csv("C:/Users/Douglas/Documents/Coursera/Johns.Hopkins.Data.Science.Specialization/Machine.Learning/Assignment/testing.csv",
                   na.strings=c("","NA"))

##----------------------------------------------------------------------------
# Exploratory analysis
##----------------------------------------------------------------------------

#Ideally the training and data sets variablility and other qualities be similar

dim(training)
head(select(training,1:10))
dim(testing)
head(select(testing,1:10))

#different set of columns betweeing training and testing

#function to see which columns have all NA
take.NA = function(x){
                 if(sum(is.na(x)) == length(x)){
                        x="TRUE"
                 } else{
                        x="non NA values present"
                 }
        }

#remove all values that are completely NA
testing.non.zero.values <- apply(testing, 2, take.NA)

#remove all NA columns
testing.proc <- testing[,testing.non.zero.values != TRUE]

#create vector to select to remove vectors in training that were removed from testing
training.remove.vectors <- names(testing)[testing.non.zero.values == TRUE]

#remove columns
training.proc.raw <- training[, !colnames(training) %in% training.remove.vectors]

#check for NA, empty spaces in observation
empty.cells <- complete.cases(training.proc.raw)[FALSE]

#remove variables with unlikely relation like training window, etc
training.proc.raw <- select(training.proc.raw, -X, -user_name,-raw_timestamp_part_1,
          -raw_timestamp_part_2, -cvtd_timestamp)

training.proc.num_window <- training.proc.raw[training.proc.raw$new_window == "no", ]                          

#feature plot

#no codebook, need to determine meaning of num_window
#research paper suggests classe corresponds to repetitions
#if classes also corresponds to num_window that means num_window equals repetition
#and new_window equals sliding window

#create table of frequency of classe per num_window
#using code below to create table and count number of num_window that
#have only one type of classe e.  All num_window have exclusively one classe
table.classe.training.proc <- table(training.proc.num_window$num_window, training.proc.num_window$classe)
table.classe.training.proc <- data.frame(table.classe.training.proc)
table.classe.training.proc <- table.classe.training.proc[table.classe.training.proc$Freq > 0, ] 
classe.per.num_window <- count(table.classe.training.proc, Var1)
#length of vector is 857, same as all observations
freq.1.classe.num_window <- length(classe.per.num_window[classe.per.num_window$n == 1,]$Var1)

#num_window will be treated as a repetition 


##----------------------------------------------------------------------------
## Features
##----------------------------------------------------------------------------

set.seed(3553)


#check distribution of casse

classe.dist <- aggregate(Freq~Var2, data = table.classe.training.proc, sum)
plot1.obj <- ggplot(data = classe.dist, aes(x = Var2, y = Freq, fill = Var2)) +
        geom_bar(stat = "identity") + 
        labs(title = "Distribution of \'classe\' Variables", x = "", y = "") +
        theme(legend.position = "none")
plot(plot1.obj)



ave.mean.test <- select(training.proc.num_window, -num_window, -new_window, -classe)

ave.mean.test <- t(rbind(summarise_each(ave.mean.test, mean), summarise_each(ave.mean.test, sd)))
ave.mean.test <- data.frame(ave.mean.test)
names(ave.mean.test) <- c("mean", "SD")

plot2.obj <- ggplot(data = ave.mean.test, aes(x = mean, y = SD)) +
        geom_jitter() +
        labs(title = "Comparison of mean vs standard distirbution", 
             x = "Mean", y = "Standard Deviation") +
        theme(legend.position = "none")
plot(plot2.obj)



#suggests no feature preprocessing needed as no very large outliers


#feature plot - week 2 3:08 plotting predictors

##----------------------------------------------------------------------------
## Algorithms and Evaluation
##----------------------------------------------------------------------------

#test three different types

#random forest algorithm
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

fitControl <- trainControl(method = "cv",
                           number = 5,
                           allowParallel = TRUE)

fit.rf.obj <- train(classe~., method="rf", data = training.proc.num_window[c(-1,-2)],
                    trControl = fitControl)

fit.gbm.obj <- train(classe~., method="gbm", data = training.proc.num_window[c(-1,-2)],
                     trControl = fitControl)

fit.lda.obj <- train(classe~., method="lda", data = training.proc.num_window[c(-1,-2)],
                     trControl = fitControl)


stopCluster(cluster)
registerDoSEQ()

#Accuracy calcs
predict.rf <- predict(fit.rf.obj, newdata = training, type = "raw")
rf.train.accuracy <- confusionMatrix(predict.rf, training$classe)

predict.gbm <- predict(fit.gbm.obj, newdata = training, type = "raw")
gbm.train.accuracy <- confusionMatrix(predict.gbm, training$classe)

predict.lda <- predict(fit.lda.obj, newdata = training, type = "raw")
lda.train.accuracy <- confusionMatrix(predict.lda, training$classe)

fit.in.sample.list <- list(rf.train.accuracy, gbm.train.accuracy, lda.train.accuracy)
fit.name.vct <- c("Random Forest","GBM","LDA")

fit.all.in.sample <- data.frame()

for(i in 1:3){
        tmp <- data.frame(t(fit.in.sample.list[[i]]$overall[c(1,2)]))
        tmp <- tmp%>%
                mutate(Model = fit.name.vct[i]) %>%
                mutate(In.Sample.Error = 1 - Accuracy) %>%
                select(Model, In.Sample.Error, Accuracy, Kappa)

        if (i==1) fit.all.in.sample <- tmp else fit.all.in.sample <-
                        rbind(fit.all.in.sample, tmp)
}

fit.rf.resample <- fit.rf.obj$resample
fit.rf.conf <- confusionMatrix.train(fit.rf.obj)

fit.gbm.resample <- fit.gbm.obj$resample
fit.gbm.conf <- confusionMatrix.train(fit.gbm.obj)

fit.lda.resample <- fit.lda.obj$resample
fit.lda.conf <- confusionMatrix.train(fit.lda.obj)

fit.resample.list <- list(fit.rf.resample, fit.gbm.resample, fit.lda.resample)

fit.all.resample <- data.frame()

for(i in 1:3){
        tmp <- fit.resample.list[[i]] %>%
                select(-Resample) %>%
                summarise(Mean.Accuracy = mean(Accuracy), Mean.Kappa = mean(Kappa)) %>%
                mutate(Model = fit.name.vct[i]) %>%
                mutate(Out.of.Sample.Error = 1 - Mean.Accuracy) %>%
                select(Model, Out.of.Sample.Error, Mean.Accuracy, Mean.Kappa)

        if (i==1) fit.all.resample <- tmp else fit.all.resample <-
                                                    rbind(fit.all.resample, tmp)
}

plot3.obj <- plot(fit.rf.obj, main = "Random Forest: Accuracy by Predictor Count")
print(plot3.obj)

Var.Imp <- varImp(fit.rf.obj, scale = TRUE)

##----------------------------------------------------------------------------
## Parameters
##----------------------------------------------------------------------------



# Aiming for
#-interpretable
#-simple
#-accurate
        


#par(mfrow = c(1,2))    
#plot(fit.rf.obj, main = "Accuracy by Predictor Count")
#plot(fit.rf.obj$finalModel, main = "Variable Importance Plot: Random Forest")

#par(mfrow = c(1,2))
#plot(fit.gbm.obj, main = "Accuracy by Predictor Count")
#plot(fit.gbm.obj$finalModel, main = "Variable Importance Plot: Gradient Boosting Machines")

#par(mfrow = c(1, 2))
#plot(fit.lda.obj, main = "Accuracy by Predictor Count")
#plot(fit.lda.obj$finalModel, main = "Variable Importance Plot: Linear Discriminant Analysis")