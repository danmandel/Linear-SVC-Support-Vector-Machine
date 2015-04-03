library(caret);library(kernlab);data(spam)
inTrainingSet <- createDataPartition(y=spam$type,
                                     p=.75,list=FALSE)
trainingSet <- spam[inTrainingSet,]
testingSet <- spam[-inTrainingSet,]
hist(trainingSet$capitalAve,main="",xlab="ave. capital run length")

mean(trainingSet$capitalAve)
sd(trainingSet$capitalAve)

#standardizing
trainCapAve <- trainingSet$capitalAve
trainCapAveS <- (trainCapAve - mean(trainCapAve))/sd(trainCapAve)
mean(trainCapAveS)
sd(trainCapAveS)

#standardizing test set
testCapAve <- testingSet$capitalAve
testCapAveS <- (testCapAve - mean(trainCapAve))/sd(trainCapAve)
mean(testCapAveS)
sd(testCapAveS)

preOBJ <- preProcess(trainingSet[-58],method=c("center","scale"))
trainCapAveS <- predict(preObj,training[set,-58])$capitalAve
mean(trainCapAveS)
