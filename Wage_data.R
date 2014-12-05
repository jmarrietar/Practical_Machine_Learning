#------------PLOTING PREDICTORS----------------------

library(ISLR)
library(ggplot2)
library(caret)

data(Wage)
summary(Wage)

#GET TRAINING/TEST SETS

inTrain<-createDataPartition(y=Wage$wage,p=0.7,list=FALSE)

training<-Wage[inTrain,]
testing<-Wage[-inTrain,]

dim(training)
dim(testing)

#Feature Plot 

featurePlot(x=training[,c("age","education","jobclass")],y=training$wage,plot="pairs")


#QPLOT with colors and regression smoothers. 

qq<-qplot(age,wage,colour=jobclass,data=training)

qq+geom_smooth(method='lm',formula=y~x)


#MAKING FACTORSS!  CUT2 Function 

