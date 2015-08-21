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

#Build model with cross-validation
# define training control
train_control <- trainControl(method="cv", number=3)
# train the model 
model_cvRF <- train(classe~., data=trainfilt.train, trControl=train_control, method="rf")
# make predictions
predictions_cvRF <- predict(model_cvRF, newdata=trainfilt.test)
# summarize results
confusionMatrix(predictions_cvRF, trainfilt.test$classe)

# Check which variables are most important
        #importance <- varImp(model_cvRF, scale=FALSE)
# summarize importance
        #print(importance)
# plot importance
        #plot(importance)

# plot relationships between the predictors and outcome
#featurePlot(x=trainfilt[,c("num_window", "roll_belt", "pitch_forearm")], y = trainfilt$classe,plot="pairs")

# RandomForest prediction
        #model <- randomForest(classe ~., data= trainfilt.train)
        #predictionsTrain <- predict(model, newdata=trainfilt.train)
        #confusionMatrix(predictionsTrain,trainfilt.train$classe)
        #predictionsTe <- predict(model, newdata=trainfilt.test)
        #confusionMatrix(predictionsTe,trainfilt.test$classe)

# Predcting on the test set
testing <- read.csv("pml-testing.csv", na.strings = c("NA", "#DIV/0!", ""))
#Remove first uneccessary columns of username and timestamps
testfilt <- testing[,-c(1:6)]
# make all into type numeric 
for(i in c(2:ncol(testfilt)-1)){
        testfilt[,i] = as.numeric(as.character(testfilt[,i]))
}
# remove columns with almost all NA values
testfilt <- testfilt[colnames(testfilt[colSums(is.na(testfilt))==0])]

answers = rep("A", 20)

for(i in 1:20){
        dat <- testing[which(testing$problem_id == i),]
        predictions_test <- predict(model_cvRF, newdata=dat)
        answers[i] <- as.character(predictions_test)
}

pml_write_files(answers)
