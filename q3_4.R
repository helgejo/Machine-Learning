library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
missClass = function(values,prediction){
        sum(((prediction > 0.5)*1) != values)/length(values)
}
set.seed(13234)

modelFit <- glm(chd ~ age + alcohol + obesity + tobacco + typea + ldl, data= trainSA, method="glm", family="binomial")

trying <- predict(modelFit, type = "response")
trying.miss <- missClass(trainSA$chd, trying)
# output = 0.2727273

testingSet <- predict(modelFit, newdata = testSA, type = "response")
testingSet.miss <- missClass(testSA$chd, testingSet)
# output = 0.3116883
