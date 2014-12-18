#Question1 
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

training<-subset(segmentationOriginal,Case=="Train")
testing<-subset(segmentationOriginal,Case=="Test")

set.seed(125)

modFit<-train(Class~.,method="rpart",data=training)


#Prettier Plots
library(rattle)
library(rpart.plot)
fancyRpartPlot(modFit$finalModel)

#Question3
library(pgmm)
data(olive)
olive = olive[,-1]
modFit<-train(Area~.,method="rpart",data=olive)
newdata = as.data.frame(t(colMeans(olive)))

predict(modFit,newdata[,-1])

#Question4 FIT A LOGISTIC REGRESSION
library(ElemStatLearn)
data(SAheart.data)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)


modglm=train(chd~age+alcohol+obesity+tobacco+typea+ldl,data=trainSA,method="glm",family="binomial")

pglmTrain=predict(modglm,trainSA)
pglmTest=predict(modglm,testSA)


missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

missClass(trainSA$chd,pglmTrain)
missClass(testSA$chd,pglmTest)

#Question5

library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 
str(vowel.train)
vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)
set.seed(33833)

modFit<-train(y~.,data=vowel.train,method="rf",prox=TRUE)
modFit

gbmImp <- varImp(modFit,value=nsubsets)
gbmImp



