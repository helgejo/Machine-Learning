library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 
set.seed(33833)
tree <- train(as.factor(vowel.train$y) ~., data = vowel.train, method = "rf")
varImp(tree)

#Overall
#x.2  100.000
#x.1   98.486
#x.5   41.348
#x.6   25.948
#x.8   20.694
#x.9    9.126
#x.4    8.532
#x.3    5.792
#x.7    1.945
#x.10   0.000