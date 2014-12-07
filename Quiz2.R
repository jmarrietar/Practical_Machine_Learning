#Question 1 

library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

#Question 2 
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

plot(training$CompressiveStrength,pch=19)
plot(testing$CompressiveStrength,pch=19)
plot(concrete$CompressiveStrength,pch=19)
#Lets make some factors 

plot(concrete$FlyAsh,pch=19)

cutFlyAsh<-cut2(concrete$FlyAsh,g=3)
table(cutFlyAsh)

cutFlyAshT<-cut2(training$FlyAsh,g=3)
table(cutFlyAshT)

qplot(concrete$CompressiveStrength, concrete$FlyAsh,data=concrete,colour=cutFlyAsh)
qplot(seq_along(concrete$CompressiveStrength), concrete$CompressiveStrength,data=concrete,colour=cutFlyAsh)
qplot(seq_along(training$CompressiveStrength), training$CompressiveStrength,data=training,colour=cutFlyAshT)
p1


#Question 3
hist(concrete$Superplasticizer,pch=19)
hist(log(concrete$Superplasticizer+1),pch=19)

#Question 4 
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]


#PCA with preprocess function  
TPCA<-training[,c(58:69)]
TPCA$row.names<-NULL

#Find all the predictor variables in the training set that begin with IL.
#Perform principal components on these variables with the preProcess() function from the caret package. 
#Calculate the number of principal components needed to capture 80% of the variance. How many are there?
preProc<-preProcess(TPCA,method="pca",thresh=0.8)
preProc$rotation

#Question 5

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

#Create a training data set consisting of only the predictors with variable names beginning with IL and the diagnosis.
trainingP<-training[,c(1,58:69)]
testingP<-testing[,c(1,58:69)]

#Build two predictive models, one using the predictors as they are.
modelFit1<-train(trainingP$diagnosis~.,method="glm",data=trainingP[-1])

Mtarix1<-confusionMatrix(testingP$diagnosis,predict(modelFit1,testingP[,-1]))

#one using PCA with principal components explaining 80% of the variance in the predictors.

#Se define el preprocesamiento
preProc<-preProcess(trainingP[,-1],method="pca",thresh=0.8)
#Se aplica el preprocesamiento a los datos de entrenamiento y se les halla componentes principales
trainPC<-predict(preProc,trainingP[,-1])
#Se entrena el Clasificador
modelFit<-train(training$diagnosis~.,method="glm",data=trainPC)

#Aqui transformamos la bd de prueba en una de componentes principales
testPC<-predict(preProc,testingP[,-1])
#Matriz de confusion, se prueba el clasificador en los datos de prueba de CP predict((modelFit,testPC))
Matrix2<-confusionMatrix(testingP$diagnosis,predict(modelFit,testPC))


