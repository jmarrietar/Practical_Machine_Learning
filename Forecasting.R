#GOOGLE DATA
library(quantmod)
from.dat<-as.Date("01/01/08",format="%m/%d/%y")
to.dat<-as.Date("12/31/13",format="%m/%d/%y")

getSymbols("GOOG",src="google",from=from.dat,to=to.dat)

head(GOOG)

#Summarize monthly and store as time series. 
mGoog<-to.monthly(GOOG)
googOpen<-Op(mGoog)
tsl<-ts(googOpen,frequency=12)
plot(tsl,xlab="Years+1",ylab="GOOG")

#Decompose a time series into parts
plot(decompose(ts1),xlab="Years+1")

#Training and test sets 
ts1Train<-window(ts1,start=1,end=5)
ts1Test<-window(ts1,star=5,end=(7-0.01))
ts1Train

#Simple moving average
plot(ts1Train)
lines(ma(ts1Train,order=3),col="red")

#Exponential smoothing
ets1<-ets(ts1Train,model="MMM")
fcast<-forecast(ets1)
plot(fcast)
lines(ts1Test,col="red")

#Get the accuracy
accuracy(fcast,ts1Test)


