library(RCurl)
library(caret)
library(psych)

urlfile <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data'
x <- getURL(urlfile, ssl.verifypeer = FALSE)
adults <- read.csv(textConnection(x),header=F)
head(adults,2)

names(adults)=c('Age','Workclass','FinalWeight','Education','EducationNumber',
                'MaritalStatus','Occupation','Relationship','Race',
                'Sex','CapitalGain','CapitalLoss','HoursWeek',
                'NativeCountry','Income')

adults$Income <- ifelse(adults$Income==' <=50K',0,1)

dmy <- dummyVars(" ~ .", data = adults)
adultsTrsf <- data.frame(predict(dmy,newdata = adults))

cor.prob <- function (X, dfr = nrow(X) - 2) {
  R <- cor(X, use="pairwise.complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr/(1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R[row(R) == col(R)] <- NA
  R
}

flattenSquareMatrix <- function(m) {
  if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.") 
  if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
  ut <- upper.tri(m)
  data.frame(i = rownames(m)[row(m)[ut]],
             j = rownames(m)[col(m)[ut]],
             cor=t(m)[ut],
             p=m[ut])
}

corMasterList <- flattenSquareMatrix (cor.prob(adultsTrsf))
print(head(corMasterList,10))

corList <- corMasterList[order(-abs(corMasterList$cor)),]
print(head(corList,10))

selectedSub <- subset(corList, (abs(cor) > 0.2 & j == 'Income'))
print(selectedSub)

#bestSub <- as.character(selectedSub$i)
#bestSub <- as.character(selectedSub$i[c(1,3,5,6,8,9)]) 

#pairs.panels(adultsTrsf[c(bestSub, 'Income')])
