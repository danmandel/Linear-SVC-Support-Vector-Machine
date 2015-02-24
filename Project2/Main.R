library(e1071)
library(quantmod)
library(PerformanceAnalytics)

getSymbols("GERN",src = "yahoo", from="1990-01-01")

addFewerFeatures = function(data)
{
close = Cl(data)
returns = na.trim(ROC(close,type='discrete'))

#n-day returns
res = merge(na.trim(lag(returns,1)),
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
xts(na.trim(lag(rollmean(returns,k=21,aligh="right"),1))),
xts(na.trim(lag(rollmedian(returns,k=21,align="right"),1))),
xts(na.trim(lag(apply.rolling(returns, width=21, FUN=sd),1))),
xts(na.trim(lag(apply.rolling(returns, width=21, FUN=mad),1))),

xts(na.trim(lag(apply.rolling(returns,width=21,align="right",FUN=skewness,1)))),

xts(na.trim(lag(apply.rolling(returns, width=21, 
                              align="right", FUN=kurtosis),1))),all = FALSE)
#add volume
res = merge(res, xts(na.trim(lag(Vo(data),2))), all=FALSE)

# add result column
nextday = ifelse(returns >= 0, 1, -1) 
res = merge(res, nextday, all=FALSE)
res = merge(res, returns, all=FALSE)

res <- na.omit(res)

colnames(res) = c("ROC1","ROC2", "ROC3", "ROC5", "ROC10", 
"ROC20", "ROC50", "ROC100", "ROC150", "ROC200",
"MEAN", "MEDIAN", "SD", "MAD", "SKEW", "KURTOSIS","VOLUME1", 
"output", "returns")

return(res)
}

data <- addFewerFeatures(GLD)
daily <- data$returns
data$returns <- NULL

learningPeriod <- 200
result <- c()

for (i in (learningPeriod+1):(length(data[,1])-2)){
  efTrain <- data[(i-learningPeriod):i,]
  r1 <- svm(factor(output) ~., data = efTrain, cost=100, gamma=0.1)
  r1.pred <- predict(r1, data[i+1, 1:17])
  r1.pred <- data.frame(r1.pred)
  
  if (as.numeric(as.character(r1.pred[1,])) == data$output[i+1]){
    result <- rbind(result,abs(daily[i+1,1])-0.0001) # we won
  } else {
    result <- rbind(result,-abs(daily[i+1,1])-0.0001) # we lost
  }
  if (i %% 200 == 0){
    charts.PerformanceSummary(result,ylog=TRUE)
  }
}
