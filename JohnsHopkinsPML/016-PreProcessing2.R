#Correlated predictors
library(caret);library(kernlab);data(spam)
inTrainingSet <- createDataPartition(y=spam$type,
                                     p=.75,list=FALSE)
trainingSet <- spam[inTrainingSet,]
testingSet <- spam[-inTrainingSet,]

M <- abs(cor(trainingSet[,-58]))
diag(M) <- 0
which(M > .8,arr.ind=T)

names(spam)[c(34,32)]
plot(spam[,34],spam[,32])

#rotate the plot
X <- .71*trainingSet$num415 + .71*trainingSet$num857
Y <- .71*trainingSet$num415 - .71*trainingSet$num857
plot(X,Y)

smallSpam <- spam[,c(34,32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1],prComp$x[,2])
