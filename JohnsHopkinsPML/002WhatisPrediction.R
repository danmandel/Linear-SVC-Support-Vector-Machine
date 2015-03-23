library(kernlab)
data(spam)
head(spam)


plot(density(spam$your[spam$type=="nonspam"]),
     col="blue", main="",xlab="Frequency of 'your'")
#blue line shows that relatively few nonspam emails have more than two 'yours'


lines(density(spam$your[spam$type=="spam"]),col="red")
#density = # of times the frequency appears

abline(v=.5,col="black")
#cutoff where message is spam if <.5 of the words are 'your'

prediction <- ifelse(spam$your > 0.5, "spam", "nonspam")
table(prediction,spam$type)/length(spam$type)
