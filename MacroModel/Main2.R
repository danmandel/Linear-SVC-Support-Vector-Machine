library(quantmod)
library(WDI)
getSymbols("DEXJPUS",src = "FRED")
getFX("USD/EUR")
data = (DEXJPUS['2015::'])
noweek = last(data, '-1 weeks') # all data except last week
last3daysoffirstweek=last(first(data, '2 weeks'), '3 days')

GetFeatures <- function(data){
  features = merge()
  
}

#periodicity
#unclass(periodicity(data))
weekly <-(to.weekly(data))

endpoints(data,on="weeks")

#find the max closing price each week
maxclose<-apply.weekly(data,FUN=function(x) {max(Cl(x))})

#maxclose<-apply.weekly(data,FUN=function(x) {max(data$DEXJPUS)})

allPGDPdata = WDI(indicator='NY.GDP.PCAP.KD', country='all', start=2013, end=2016)
treasurydata = WDI(indicator='FR.INR.RISK', country='all', start=2013, end=2016)
WDIsearch('gdp.*capita.*constant')
WDIsearch('gdp)
