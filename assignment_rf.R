library(ISLR); library(ggplot2); library(caret); library(Hmisc); library(randomForest)
set.seed(96)

#import data
training <- read.csv("pml-training.csv", na.strings = c("NA", "#DIV/0!", ""))

#Data description
# summary(training)
# describe(training)
# head(training)
# sapply(training, class)
# str(training)

#Remove first uneccessary columns of username and timestamps
trainfilt <- training[,-c(1:6)]

# make all into type numeric 
for(i in c(2:ncol(trainfilt)-1)){
        trainfilt[,i] = as.numeric(as.character(trainfilt[,i]))
}

# remove columns with almost all NA values
trainfilt <- trainfilt[colnames(trainfilt[colSums(is.na(trainfilt))==0])]

# Tested remove Near zero variance predictors
#nzv <- nearZeroVar(trainfilt)
#trainfilt <- trainfilt[, -nzv]

# Split training data into train and test to do cross-validation
inTrain <- createDataPartition(trainfilt$classe, p = 0.75, list = FALSE)
trainfilt.train <- trainfilt[inTrain,]
trainfilt.test <- trainfilt[-inTrain,]

#Cross-validation
# define training control
train_control <- trainControl(method="cv", number=3)
# train the model 
model_cvRF <- train(classe~., data=trainfilt.train, trControl=train_control, method="rf")
# make predictions
predictions_cvRF <- predict(model_cvRF, newdata=trainfilt.test)
# summarize results
confusionMatrix(predictions_cvRF, trainfilt.test$classe)

importance <- varImp(model_cvRF, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

# RandomForest prediction
model <- randomForest(classe ~., data= trainfilt.train)
predictionsTrain <- predict(model, newdata=trainfilt.train)
confusionMatrix(predictionsTrain,trainfilt.train$classe)

predictionsTe <- predict(model, newdata=trainfilt.test)
confusionMatrix(predictionsTe,trainfilt.test$classe)