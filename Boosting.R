library(ISLR)
data(Wage)
library(ggplot2)
library(caret)

Wage<-subset(Wage,select=-c(logwage))

inTrain<-createDataPartition(y=Wage$wage,p=0.7,list=FALSE)

training<-Wage[inTrain,]
testing<-Wage[-inTrain,]

#Fit the model. #gbm<- Boosting with Trees
modFit<-train(wage~.,method="gbm",data=training,verbose=FALSE)
print(modFit)

#Plot the results 
qplot(predict(modFit,testing),wage,data=testing)

