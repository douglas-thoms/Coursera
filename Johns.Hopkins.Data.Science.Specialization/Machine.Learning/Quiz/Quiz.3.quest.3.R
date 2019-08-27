library(pgmm)
library(dplyr)
library(caret)
library(rattle)
set.seed(125)
data(olive)
olive = olive[,-1]

fit <- train(Area ~., data = olive, method = "rpart")

newdata = as.data.frame(t(colMeans(olive)))

plot(fit$finalModel, uniform = TRUE, main = "Classification Tree")
text(fit$finalModel, use.n = TRUE, all = TRUE, cex =.8)
fancyRpartPlot(fit$finalModel)

x <- predict(fit, newdata = newdata)