#Question 1
#Load the vowel.train and vowel.test data sets:
library(ElemStatLearn)
library(caret)
data(vowel.train)
data(vowel.test) 

vowel.train$y<-as.factor(vowel.train$y)
vowel.test$y<-as.factor(vowel.test$y)
str(vowel.train)
str(vowel.test)

# Fit (1) a random forest predictor relating the factor variable y to the remaining variables 
set.seed(33833)
modFit<-train(y~.,data=vowel.train,method="rf",prox=TRUE)
modFit

#a boosted predictor using the "gbm" method. 
#Fit the model. #gbm<- Boosting with Trees
modFit2<-train(y~.,method="gbm",data=vowel.train,verbose=FALSE)
print(modFit2)

#What are the accuracies for the two approaches on the test data set?

#Predict New Values Random forest
pred1<-predict(modFit,vowel.test)
vowel.test$predRight<-pred1==vowel.test$y
Tabla1<-table(pred1,vowel.test$y)

# load Caret package for computing Confusion matrix
confusionMatrix(Tabla1)

#Predict New Values Boosting with trees
pred2<-predict(modFit2,vowel.test)

Tabla2<-table(pred2,vowel.test$y)
confusionMatrix(Tabla2)


#Question 2 
#Load the Alzheimer's data
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]


set.seed(62433)
#Build two different Models. 
mod1<-train(diagnosis~.,method="glm",data=training)
mod2<-train(diagnosis~.,method="rf",data=training,trControl=trainControl(method="cv"),number=3)
modlda=train(diagnosis~.,data=training,method="lda")

#Predict on the testing set
pred1<-predict(mod1,testing)
pred2<-predict(mod2,testing)
pred3<-predict(modlda,testing)


#Fit a model that combines predictors
predDF<-data.frame(pred1,pred2,pred3,diagnosis=testing$diagnosis)
combModFit<-train(diagnosis~.,method="rf",data=predDF,trControl=trainControl(method="cv"),number=3)
combPred<-predict(combModFit,predDF)

#Accuracy
Tabla3<-table(combPred,testing$diagnosis)
confusionMatrix(Tabla3)

#Question 3
set.seed(3523)
library(AppliedPredictiveModeling)
library(caret)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[inTrain,]
testing = concrete[-inTrain,]

set.seed(233)

lassoFit <- train( training$CompressiveStrength ~ ., method="lasso", data=training)
lassoPred <- predict(lassoFit,testing)
xx<-lassoFit$finalModel
plot(xx$penalty,xx$actions,pch=19)
plot.enet(xx, xvar="penalty", use.color=T)

#Question 5
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(325)

library(e1071)

#svm
svm.model<-svm(CompressiveStrength~.,data=training)
svm<-predict(svm.model,testing[,-9])

colnames(testing)

#Evaluate on testing
sqrt((sum((svm-testing[,9])^2))/256)

