library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
set.seed(125)
#inTrain <- createDataPartition(y=segmentationOriginal$Case, p=0.75, list = FALSE)
training <- segmentationOriginal[which(segmentationOriginal$Case == "Train"),]
testing <- segmentationOriginal[which(segmentationOriginal$Case == "Test"),]
modelFit <- train(Class ~., data= training, method="rpart")
modelFit$finalModel
#test <- colnames(testing)
#df <- read.table(text = "", col.names = test)

#a. PS 
#b. WS 
#c. PS
#d. Not possible to predict 

#q2 The bias is larger and the variance is smaller. Under leave one out cross validation K is equal to the sample size.