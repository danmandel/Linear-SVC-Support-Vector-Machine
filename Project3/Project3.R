library(quantmod)
getSymbols("AMZN")

#barChart(AMZN, theme='white.mono',bar.type='hlc')
barChart(AMZN)

getSymbols("^GSPC")

Nasdaq100_Symbols <- c("AAPL", "ADBE", "ADI", "ADP", "ADSK", "AKAM", "ALTR", "ALXN", 
                       "AMAT", "AMGN", "AMZN", "ATVI", "AVGO", "BBBY", "BIDU", "BIIB", 
                       "BRCM", "CA", "CELG", "CERN", "CHKP", "CHRW", "CHTR", "CMCSA", 
                       "COST", "CSCO", "CTRX", "CTSH", "CTXS", "DISCA", "DISCK", "DISH", 
                       "DLTR", "DTV", "EBAY", "EQIX", "ESRX", "EXPD", "EXPE", "FAST", 
                       "FB", "FFIV", "FISV", "FOXA", "GILD", "GMCR", "GOOG", "GOOGL", 
                       "GRMN", "HSIC", "ILMN", "INTC", "INTU", "ISRG", "KLAC", "KRFT", 
                       "LBTYA", "LLTC", "LMCA", "LMCK", "LVNTA", "MAR", "MAT", "MDLZ", 
                       "MNST", "MSFT", "MU", "MXIM", "MYL", "NFLX", "NTAP", "NVDA", 
                       "NXPI", "ORLY", "PAYX", "PCAR", "PCLN", "QCOM", "QVCA", "REGN", 
                       "ROST", "SBAC", "SBUX", "SIAL", "SIRI", "SNDK", "SPLS", "SRCL", 
                       "STX", "SYMC", "TRIP", "TSCO", "TSLA", "TXN", "VIAB", "VIP", 
                       "VOD", "VRSK", "VRTX", "WDC", "WFM", "WYNN", "XLNX", "YHOO")
getSymbols(Nasdaq100_Symbols)

nasdaq100 <- data.frame(as.xts(merge(AAPL, ADBE, ADI, ADP, ADSK, AKAM, 
                                     ALTR, ALXN,AMAT, AMGN, AMZN, ATVI, AVGO, BBBY, BIDU, BIIB, 
                                     BRCM, CA, CELG, CERN, CHKP, CHRW, CHTR, CMCSA, 
                                     COST, CSCO, CTRX, CTSH, CTXS, DISCA, DISCK, DISH, 
                                     DLTR, DTV, EBAY, EQIX, ESRX, EXPD, EXPE, FAST, 
                                     FB, FFIV, FISV, FOXA, GILD, GMCR, GOOG, GOOGL, 
                                     GRMN, HSIC, ILMN, INTC, INTU, ISRG, KLAC, KRFT, 
                                     LBTYA, LLTC, LMCA, LMCK, LVNTA, MAR, MAT, MDLZ, 
                                     MNST, MSFT, MU, MXIM, MYL, NFLX, NTAP, NVDA, 
                                     NXPI, ORLY, PAYX, PCAR, PCLN, QCOM, QVCA, REGN, 
                                     ROST, SBAC, SBUX, SIAL, SIRI, SNDK, SPLS, SRCL, 
                                     STX, SYMC, TRIP, TSCO, TSLA, TXN, VIAB, VIP, 
                                     VOD, VRSK, VRTX, WDC, WFM, WYNN, XLNX, YHOO)))

outcomeSymbol <- 'FISV.Volume'
library(xts)
#For training
nasdaq100 <- xts(nasdaq100,order.by=as.Date(rownames(nasdaq100)))
nasdaq100 <- as.data.frame(merge(nasdaq100, lm1=lag(nasdaq100[,outcomeSymbol],-1)))
nasdaq100$outcome <- ifelse(nasdaq100[,paste0(outcomeSymbol,'.1')] > nasdaq100[,outcomeSymbol], 1, 0)
nasdaq100 <- nasdaq100[,!names(nasdaq100) %in% c(paste0(outcomeSymbol,'.1'))]

GetDiffDays <- function(objDF,days=c(10), offLimitsSymbols=c('outcome'), roundByScaler=3) {
  # needs to be sorted by date in decreasing order
  ind <- sapply(objDF, is.numeric)
  for (sym in names(objDF)[ind]) {
    if (!sym %in% offLimitsSymbols) {
      print(paste('*********', sym))
      objDF[,sym] <- round(scale(objDF[,sym]),roundByScaler)
      
      print(paste('theColName', sym))
      for (day in days) {
        objDF[paste0(sym,'_',day)] <- c(diff(objDF[,sym],lag = day),rep(x=0,day)) * -1
      }
    }
  }
  return (objDF)
}

nasdaq100 <- GetDiffDays(nasdaq100, days=c(1,2,3,4,5,10,20), offLimitsSymbols=c('outcome'), roundByScaler=2)

#drop the most recent row because outcome is not yet known
nasdaq100 <- nasdaq100[2:nrow(nasdaq100),]
