library(quantmod)
library(caret)
library(e1071)
#getSymbols('SPY',src = 'yahoo')
getSymbols('^VIX',src='yahoo')
getFX('EUR/USD',src = 'yahoo')
#getSymbols('CPIAUCSL',src='FRED')
getSymbols('DGS10',src='FRED')

#dataSPY = SPY['2014::']
#dataVIX = VIX['2015::']
todayclose = EURUSD['2015::']
prevclose <- lag(todayclose,1) # now the value for jan 2 is the price it was on jan 1
nextclose <- lag(todayclose,-1)
nextday = ifelse(nextclose>todayclose,1,ifelse(nextclose<todayclose,-1,0))
dataset <- merge(prevclose,todayclose,DGS10['2015::'],nextday)
colnames(dataset) = c("prevclose","close","DGS110","nextday")
dataset[is.na(dataset)] <- 999



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



  
