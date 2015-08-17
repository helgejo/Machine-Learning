library(ISLR); library(ggplot2); library(caret)
set.seed(96)
training <- read.csv("pml-training.csv", na.strings = c("NA", "#DIV/0!", ""))
testing <- read.csv("pml-testing.csv")

#featurePlot(x=training, y=training$classe, plot="pairs")
#featurePlot(x=training[,c(11:15)], y = training$classe,plot="pairs")


#Remove first columns of username and timestamps --> 154 columns
trainfilt <- training[,-c(1:6)]
trainfilt<- apply(trainfilt,c(1:153),as.numeric) 

# Impute and standardize
preObj <- preProcess(training[,-154],method="knnImpute")
trainfilt <- predict(preObj,training[,-154])$classe

#Create dummy variables 
#dummies <- dummyVars(classe ~ new_window, data = trainfilt)
#traindum <- predict(dummies, newdata = trainfilt)

#Remove only NA columns --> not needed
#trainfilt <- trainfilt[,colSums(is.na(trainfilt))<nrow(trainfilt)]

# Remove Near zero variance predictors --> 95 columns
nzv <- nearZeroVar(trainfilt)
trainfilt <- trainfilt[, -nzv]
#dim(trainnzv)

#Remove highly correlated variables --> matrix not symmetric?
highlyCor <- findCorrelation(trainfilt[,-95], cutoff = .75)
trainfilt <- trainfilt[,-na.omit(highlyCor)]

# create preprocess object
#preProcValues <- preProcess(trainfilt[,-79], method = c("center", "scale"))
preProc <- preProcess(trainfilt[,-79],method="pca")

# calculate PCs for training data
trainPC <- predict(preProc,trainfilt[,-79])

# run model on outcome and principle components
modelFit <- train(trainfilt$classe ~ .,method="glm",data=trainPC)

# calculate PCs for test data
testPC <- predict(preProc,testing[,-160])

# compare results
confusionMatrix(testing$classe,predict(modelFit,testPC))