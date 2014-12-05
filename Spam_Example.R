#The Caret Package. 

#Functionality
#*Some preprocessing(cleaning)
#  -PreProcess
#*Data splitting
#  -createDataPartition
#  -CreateResample
#  -CreateTimeSlices
#*Training/testing functions
#  -train
#  -predict
#*Model comparison
#  -confudionMatrix


###########SPAM EXAMPLE################# 

#Data Splitting
library(caret);
library(kernlab)
data(spam)

inTrain<-createDataPartition(y=spam$type,p=0.75,list=FALSE)
training<-spam[inTrain,]
testing<-spam[-inTrain,]

dim(training)

#Fit a Model. 

set.seed(32343)
install.packages('e1071', dependencies=TRUE)
modelFit<-train(type ~.,data=training,method="glm")
modelFit

#FinalModel
modelFit$finalModel

#Prediction 
predictions<-predict(modelFit,newdata=testing)
predictions

#Confusion Matrix
confusionMatrix(predictions,testing$type)


###############################################################
###############################################################
#########################################################

#FOLDS
set.seed(32323)
folds<-createFolds(y=spam$type,k=10,list=T,returnTrain=T)
sapply(folds,length)

folds[[1]][1:10]

#RESAMPLING 
set.seed(32323)
folds<-createResample(y=spam$type,times=10,list=T)
sapply(folds,length)

#Time Slices. 


