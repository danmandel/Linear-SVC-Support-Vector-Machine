library(quantmod)
getSymbols('SPY',src = 'yahoo')
getSymbols('^VIX',src='yahoo')
getFX('EUR/USD',src = 'yahoo')

dataSPY = SPY['2014::']
dataVIX = VIX['2015::']

both <- merge(dataSPY,dataVIX)
mm <- specifyModel(Next(OpCl(dataSPY)) ~ OpCl(dataSPY) + Cl(dataVIX))
modelData(mm)

candleChart(SPY,theme='white.mono', type='candles') 
#addSMI(n=13,slow=25,fast=2,signal=9,ma.type="EMA")

#buildData(formula, na.rm = TRUE, return.class = "zoo")

