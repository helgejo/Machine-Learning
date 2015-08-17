library(ISLR); library(ggplot2); library(caret); library(Hmisc)
set.seed(96)
training <- read.csv("pml-training.csv", na.strings = c("NA", "#DIV/0!", ""))
testing <- read.csv("pml-testing.csv", na.strings = c("NA", "#DIV/0!", ""))
#testing <- testing[which(testing$problem_id == 1),]
#featurePlot(x=training, y=training$classe, plot="pairs")
#featurePlot(x=training[,c(11:15)], y = training$classe,plot="pairs")


#Remove first uneccessary columns of username and timestamps --> 154 columns
trainfilt <- training[,-c(1:6)]

# make all into type numeric 
for(i in c(2:ncol(trainfilt)-1)){
        trainfilt[,i] = as.numeric(as.character(trainfilt[,i]))
}

# remove columns with almost all NA values
trainfilt <- trainfilt[colnames(trainfilt[colSums(is.na(trainfilt))==0])]

# Impute and standardize
#preObj <- preProcess(training[,-154],method="knnImpute")
#trainfilt <- predict(preObj,training[,-154])$classe

# Tested remove Near zero variance predictors
#nzv <- nearZeroVar(trainfilt)
#trainfilt <- trainfilt[, -nzv]

# Split training data into train and test to do cross-validation
inTrain <- createDataPartition(trainfilt$classe, p = 0.75, list = FALSE)
trainfilt.train <- trainfilt[inTrain]
trainfilt.test <- trainfilt[-inTrain]

preP<-preProcess(ILdata[,-13], method = "pca", thresh = 0.8)
trainPC <- predict(preP, ILdata[,-13])
modelFit <- train(ILdata$diagnosis ~ .,method="glm",data=trainPC)

testPC <- predict(preP,testData[,-13])
# compare results
confusionMatrix(testData$diagnosis, predict(modelFit,testPC))

# create preprocess object
#preProcValues <- preProcess(trainfilt[,-79], method = c("center", "scale"))
#preProc <- preProcess(trainfilt[,-79],method="pca")

# calculate PCs for training data
#trainPC <- predict(preProc,trainfilt[,-79])



# run model on outcome and principle components
modelFit <- train(trainfilt$classe ~ .,method="glm",data=trainPC)

# calculate PCs for test data
testPC <- predict(preProc,testing[,-160])

# compare results
confusionMatrix(testing$classe,predict(modelFit,testPC))