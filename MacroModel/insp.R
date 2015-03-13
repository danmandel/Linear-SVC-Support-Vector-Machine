library(quantmod)
library(e1071)
library(caret)
getSymbols("SPY")
data = SPY

#Add features

addFeatures = function(data)
{
    
  require(PerformanceAnalytics)
  close = Cl(data)
    
  returns = na.trim(ROC(close, type="discrete"))
    
  # n-day returns
  res = merge(na.trim(lag(returns, 1)),
              na.trim(lag(ROC(close, type="discrete", n=2), 1)),
              na.trim(lag(ROC(close, type="discrete", n=3), 1)),
              na.trim(lag(ROC(close, type="discrete", n=5), 1)),
              na.trim(lag(ROC(close, type="discrete", n=10), 1)),
              na.trim(lag(ROC(close, type="discrete", n=20), 1)),
              na.trim(lag(ROC(close, type="discrete", n=50), 1)),
              na.trim(lag(ROC(close, type="discrete", n=100), 1)),
              na.trim(lag(ROC(close, type="discrete", n=150), 1)),
              na.trim(lag(ROC(close, type="discrete", n=200), 1)),
              all = FALSE)
    
  # other features
  res = merge(res,
              xts(na.trim(lag(rollmean(returns, k=21, align="right"),1))),
              xts(na.trim(lag(rollmedian(returns, k=21, align="right"),1))),
              xts(na.trim(lag(apply.rolling(returns, width=21, FUN=sd),1))),
              xts(na.trim(lag(apply.rolling(returns, width=21, FUN=mad),1))),
              xts(na.trim(lag(apply.rolling(returns, width=21, align="right", FUN=skewness),1))),
              xts(na.trim(lag(apply.rolling(returns, width=21, align="right", FUN=kurtosis),1))),
              all = FALSE)
    
  # add volume
    res = merge(res, xts(na.trim(lag(Vo(data),2))), all=FALSE)
    
  # add result column
  
    nextday = ifelse(returns >= 0, 1, -1) # 1 if next day higher, 0 otherwise
    res = merge(res, nextday, all=FALSE)
    
    colnames(res) = c("ROC.1", "ROC.2", "ROC.3", "ROC.5", "ROC.10", "ROC.20", "ROC.50", "ROC.100", "ROC.150", "ROC.200",
                      "MEAN", "MEDIAN", "SD", "MAD", "SKEW", "KURTOSIS",
                      "VOLUME", "NEXTDAY")
    return(res)
}

#Generate feature list
dataset = addFeatures(data)

#Splitting data into training, testing, [cross validation]
index = 1:nrow(dataset)
inTraining = createDataPartition(index, p=0.75, list=FALSE)
training_orig = dataset[inTraining, ]
testing_orig = dataset[-inTraining, ]

training = as.data.frame(dataset[inTraining, ])
rownames(training) = NULL
training$NEXTDAY = as.factor(training$NEXTDAY)

testing = as.data.frame(dataset[-inTraining, ])
rownames(testing) = NULL
testing$NEXTDAY = as.factor(testing$NEXTDAY)

#revisit this
#levels(training$NEXTDAY) <- list(down="0", up="1")
#levels(testing$NEXTDAY) <- list(down="0", up="1")

#Train model
svm.model <- svm(NEXTDAY~., data = training, cost = 10^1, gamma=10^-2)

svm.pred = predict(svm.model, testing[,-ncol(testing)]) # leave out last column which contains the actual result

#table(pred=svm.pred, true=testing[,ncol(testing)])
#classAgreement(table(pred=svm.pred, true=testing[,ncol(testing)]))
