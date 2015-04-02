library(ISLR);library(ggplot2);library(caret);library(Hmisc);
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

qq <- qplot(age,wage,color=education,data=trainingSet)
qq +geom_smooth(method='lm',formula=y~x)

cutWage <- cut2(trainingSet$wage,g=3)
table(cutWage)

p1 <- qplot(cutWage,age,data=trainingSet,fill=cutWage,
            geom=c("boxplot"))
p1

p2 <- qplot(cutWage,age,data=trainingSet,fill=cutWage,
            geom=c("boxplot","jitter"))
p2

t1 <- table(cutWage,trainingSet$jobclass)
t1

prop.table(t1,1)
qplot(wage,color=education,data=trainingSet,geom="density")



