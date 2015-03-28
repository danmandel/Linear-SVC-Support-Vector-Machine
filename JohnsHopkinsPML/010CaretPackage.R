library(caret)
library(kernlab)
data(spam)

#Data splitting
inTrainingSet <- createDataPartition(y = spam$type,
                                     p=0.75,list=FALSE)
TrainingSet <- spam[inTrainingSet,]
TestingSet <- spam[-inTrainingSet,]
dim(TrainingSet)

#Fitting a model
set.seed(32343)
modelFit <- train(type ~., data=TrainingSet,method="glm")
modelFit

#Final model
modelFit <- train(type ~., data=TrainingSet,method="glm")
modelFit$finalModel

#Prediction
prediction <- predict(modelFit,newdata=TestingSet)
prediction

confusionMatrix(prediction,TestingSet$type)
