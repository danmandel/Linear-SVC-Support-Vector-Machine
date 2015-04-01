library(ISLR);library(ggplot2);library(caret);
data(Wage)
summary(Wage)

inTrainingSet <- createDataPartition(y=Wage$wage,
                                     p=.7,list=FALSE)
trainingSet <- Wage[inTrainingSet,]
testingSet <- Wage[-inTrainingSet,]
dim(trainingSet);dim(testingSet)

featurePlot(x=trainingSet[,c("age","education","jobclass")],
            y=trainingSet$wage,
            plot="pairs")

qplot(age,wage,color=jobclass,data=trainingSet)

