#------------PLOTING PREDICTORS----------------------

library(ISLR)
library(ggplot2)
library(caret)
library(Hmisc)

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


######################################################
#MAKING FACTORSS!  CUT2 Function 
cutWage<-cut2(training$wage,g=3)
table(cutWage)

p1<-qplot(cutWage,age,data=training,fill=cutWage,geom=c("boxplot"))
p1

#TABLESSS
t1<-table(cutWage,training$jobclass)
t1

#proportion

prop.table(t1,1)


#Things you should be Looking
#IMBALANCE IN OUTCOMES/PREDICTORS. 
#outliers
#group of points not explianed by predictor
#Skewed variables. 

#DENSITY PLOT 

qplot(wage,colour=education,data=training,geom="density")

#############################################################
####################PREPROCESSING############################
#############################################################







