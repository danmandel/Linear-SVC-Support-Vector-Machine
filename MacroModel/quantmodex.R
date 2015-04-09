library(quantmod)
library(caret)
library(e1071)
date="2014-01-01"
date1='2014::'

getSymbols('SPY',from=date,src = 'yahoo')
getSymbols('^VIX',from=date,src='yahoo')
getFX('EUR/USD',from=date,src = 'google')
getSymbols('CPIAUCSL',from=date,src='FRED')
getSymbols('DGS10',from=date,src='FRED')

DGS10 = DGS10['2010::']
#dataSPY = SPY['2014::']
#dataVIX = VIX['2015::']
todayclose = EURUSD['2010::']
prevclose <- lag(todayclose,1) # now the value for jan 2 is the price it was on jan 1
nextclose <- lag(todayclose,-1)
nextday = ifelse(nextclose>todayclose,1,ifelse(nextclose<todayclose,-1,0))
dataset <- na.locf(merge(prevclose,todayclose,DGS10['2014::'],nextday)) #Last One Carried Forward and merged
colnames(dataset) = c("prevclose","close","DGS110","nextday")
#dataset[is.na(dataset)] <- 999


index = 1:nrow(dataset)
inTrainingSet = createDataPartition(index,p=.75,
                                    list=FALSE)
training_orig = dataset[inTrainingSet,]
testing_orig = dataset[-inTrainingSet,]

training = as.data.frame(dataset[inTrainingSet,])
rownames(training) = NULL
training$nextday = as.factor(training$nextday)

testing = as.data.frame(dataset[-inTrainingSet,])
rownames(testing) = NULL
testing$nextday = as.factor(testing$nextday)

svm.model <- svm(nextday~.,data=training,cost=10^1,gamma=10^-2)
svm.pred = predict(svm.model,testing[,-ncol(testing)]) # leave out last column which contains result

table(pred=svm.pred, true=testing[,ncol(testing)])
classAgreement(table(pred=svm.pred, true=testing[,ncol(testing)]))

  
