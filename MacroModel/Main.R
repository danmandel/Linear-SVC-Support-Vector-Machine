library(quantmod)
library(WDI)

candleChart(GOOG,theme='black', type='candles')
getSymbols("goog", src = "yahoo")
getSymbols("DEXJPUS",src="FRED") # Japan / U.S. since 1971ish 
#head(DEXJPUS)

movavg <- avg(Cl(DEXJPUS))
chartSeries(DEXJPUS)
candleChart(DEXJPUS,subset='1940-12::2015')

quandl
#also check bea.gov and worldbank for api
#install.packages('WDI')

#WDIsearch('gdp')[1:10,]
WDI

allPGDPdata = WDI(indicator='NY.GDP.PCAP.KD', country='all', start=1960, end=2016)
WDIsearch('gdp.*capita.*constant')
WDIsearch('gdp)
