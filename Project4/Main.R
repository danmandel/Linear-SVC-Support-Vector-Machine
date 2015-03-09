library(caret)
library(ggplot)
library(e1071)
library(rpart)

getSymbols("UUP")
daily <- dailyReturn(UUP)
daily <- na.omit(daily)

t <- 10
k <- length(daily[,1])
