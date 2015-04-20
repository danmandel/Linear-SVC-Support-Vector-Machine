library(quantmod)
library(caret)
library(e1071)
date="2014-01-01"
date1='2015::'


fredtickerlist <- c('SP500','VIXCLS','EXUSEU','CPIAUCSL','DGS10','UNRATE')
getSymbols(fredtickerlist,from=date,src='FRED')

#googletickerlist <- c('SPY')


#total = merge(fredtickerlist[1],fredtickerlist[2])

#mergeshit2 <- function(n){
 # for (i in 1:6){
  #  i=i[date1]
  #}
#}


#SP500=SP500[date1]
DGS10 = DGS10[date1]
CPIAUCSL=CPIAUCSL[date1]
UNRATE = UNRATE[date1] 

todayclose = EXUSEU
prevclose <- lag(todayclose,1) # now the value for jan 2 is the price it was on jan 1
nextclose <- lag(todayclose,-1)
#nextday = ifelse(nextclose>todayclose,1,ifelse(nextclose<todayclose,-1,0))
nextday = ifelse(nextclose>=todayclose,1,-1)
dataset <- na.trim(na.locf(merge(prevclose,todayclose,SP500,CPIAUCSL,VIXCLS,DGS10,nextday))) #Last One Carried Forward and merged
colnames(dataset) = c("prevclose","close","SPYclose","CPIAUCSL","VIXclose","DGS10","nextday")
#dataset[is.na(dataset)] <- 999


index = 1:nrow(dataset)
inTrainingSet = createDataPartition(index,p=.75,
                                    list=FALSE)
training_orig = dataset[inTrainingSet,]
testing_orig = dataset[-inTrainingSet,]

training = as.data.frame(dataset[inTrainingSet,])
#rownames(training) = NULL
training$nextday = as.factor(training$nextday)

testing = as.data.frame(dataset[-inTrainingSet,])
#rownames(testing) = NULL
testing$nextday = as.factor(testing$nextday)

svm.model <- svm(nextday~.,data=training,cost=10^1,gamma=10^-2)
svm.pred = predict(svm.model,testing[,-ncol(testing)]) # leave out last column which contains result

table(pred=svm.pred, true=testing[,ncol(testing)])
classAgreement(table(pred=svm.pred, true=testing[,ncol(testing)]))
