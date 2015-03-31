library(caret);library(kernlab);data(spam)
inTrainingSet <- createDataPartition(y=spam$type,
                                     p=.75,list=FALSE)
trainingSet <- spam[inTrainingSet,]
testingSet <- spam[-inTrainingSet,]
modelFit <- train(type ~., data=trainingSet, method = "glm")

args(train.default)
args(trainControl)

set.seed(1235)
modelFit2 <- train(type ~.,data=trainingSet,method="glm")
modelFit2
