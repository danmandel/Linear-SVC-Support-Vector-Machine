#Correlated predictors
library(caret);library(kernlab);data(spam)
inTrainingSet <- createDataPartition(y=spam$type,
                                     p=.75,list=FALSE)
trainingSet <- spam[inTrainingSet,]
testingSet <- spam[-inTrainingSet,]

M <- abs(cor(trainingSet[,-58]))
diag(M) <- 0
which(M > .8,arr.ind=T)
