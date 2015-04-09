library(quantmod)
library(YieldCurve)
library(caret)
#getSymbols('SPY',src = 'yahoo')
getSymbols('^VIX',src='yahoo')
getFX('EUR/USD',src = 'yahoo')
getSymbols('CPIAUCSL',src='FRED')
getSymbols('DGS10',src='FRED')

dataSPY = SPY['2014::']
dataVIX = VIX['2015::']
todayclose = EURUSD['2015::']
prevclose <- lag(todayclose,1) # now the value for jan 2 is the price it was on jan 1
nextclose <- lag(todayclose,-1)
check <- merge(prevclose,todayclose,nextclose,nextday,nextday3)
colnames(check) = c("prevclose","close","nextclose","nextday","nextday3")

nextday = ifelse(nextclose>=todayclose,1,-1)
nextday3 = ifelse(nextclose>todayclose,1,ifelse(nextclose<todayclose,-1,0))

nextday3 = {
 if (nextclose[i]>todayclose[i]){
   nextday2[i] <- 1
 } else if (nextclose[i]==todayclose[i]){
   nextday2[i] <- 0
 }
  else if (nextclose[i]<todayclose[i]){
    nextday2[i] <- -1
  }
}


total <- merge(DGS10,EURUSD,VIX)
curonly <- EURUSD['2015::']
nextday = ifelse 
total2015 <- total['2015::']

index = 1:nrow(total2015)
inTrainingSet = createDataPartition(index,p=.75,
                                    list=FALSE)
training_orig = total2015[inTrainingSet,]
testing_orig = total2015[-inTrainingSet,]

training = as.data.frame(total2015[inTrainingSet,])
rownames(training) = NULL



#mm <- specifyModel(Next(OpCl(dataSPY)) ~ OpCl(dataSPY) + Cl(dataVIX))
#modelData(mm)
#candleChart(SPY,theme='white.mono', type='candles') 
#addSMI(n=13,slow=25,fast=2,signal=9,ma.type="EMA")
#buildData(formula, na.rm = TRUE, return.class = "zoo")
