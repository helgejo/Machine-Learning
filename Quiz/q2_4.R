library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
testData = testing[,grep("^IL_", colnames(testing))]
testData$diagnosis <- testing[,1]
ILdata <-  training[,grep("^IL_", colnames(training))]
ILdata$diagnosis <- training[,1]
preP<-preProcess(ILdata[,-13], method = "pca", thresh = 0.8)
trainPC <- predict(preP, ILdata[,-13])
modelFit <- train(ILdata$diagnosis ~ .,method="glm",data=trainPC)

testPC <- predict(preP,testData[,-13])
# compare results
confusionMatrix(testData$diagnosis, predict(modelFit,testPC))