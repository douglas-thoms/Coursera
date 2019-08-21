library(ggplot2)
library(GGally)
library(Hmisc)
library(dplyr)
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

#add index to combined data frame
concrete <- concrete %>%
        mutate(index = as.numeric(rownames(concrete)))


cut.variable <- cut2(concrete$FlyAsh,g=7)
print(table(cut.variable))

p1 <- qplot(concrete$index,concrete$CompressiveStrength, colour = cut.variable)

plot(p1)

names <- colnames(concrete)
names <- names[-length(names)]

p2 <- featurePlot(x = training[, names], y = training$CompressiveStrength, plot = "pairs")

plot(p2)

p3 <- ggpairs(concrete)

plot(p3)