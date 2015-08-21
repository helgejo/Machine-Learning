library(pgmm)
library(tree)
data(olive)
olive = olive[,-1]
set.seed(125)

treeOlive <- tree(Area ~., data = olive)

newdata = as.data.frame(t(colMeans(olive)))

predict(treeOlive, newdata)

## output 2.875 