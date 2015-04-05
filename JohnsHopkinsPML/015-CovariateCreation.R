library(kernlab);data(spam)

#transforming tidy covariates
spam$capitalAvesq <- spam$capitalAve^2

#dummy variables
library(ISLR);library(caret);data(Wage);
inTrainingSet <- createDataPartition(y=Wage$wage,
                                     p=.7,list=FALSE)
trainingSet <- Wage[inTrainingSet,]; testingSet <- Wage[-inTrainingSet,]
table(trainingSet$jobclass)
dummies <- dummyVars(wage ~ jobclass, data=trainingSet)
head(predict(dummies,newdata=trainingSet))

#removing zero covariates
nsv <- nearZeroVar(trainingSet,saveMetrics=TRUE)
nsv

#spline basis
library(splines)
bsBasis <- bs(trainingSet$age,df=3)
bsBasis

#fitting curves with splines
lml <- lm(wage ~ bsBasis,data=trainingSet)
plot(trainingSet$age,trainingSet$wage,pch=19,cex=.5)
points(trainingSet$age,predict(lml,newdata=trainingSet),col="red",pch=19,cex=.5)

#splines on test set
predict(bsBasis,age=testingSet$age)

