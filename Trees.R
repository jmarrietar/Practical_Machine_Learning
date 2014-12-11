data(iris)
library(ggplot2)
names(iris)
library(caret)
#Create Training and test Sets
inTrain<-createDataPartition(y=iris$Species,p=0.7,list=FALSE)

training<-iris[inTrain,]
testing<-iris[-inTrain,]

dim(training)
dim(testing)

qplot(Petal.Width,Sepal.Width,colour=Species,data=training)

modFit<-train(Species~.,method="rpart",data=training)

print(modFit$finalModel)

#Plot Tree

plot(modFit$finalModel,uniform=TRUE,main="Classification Trea")
text(modFit$finalModel,use.n=T,all=T,cex=0.5)

#Prettier Plots
library(rattle)
library(rpart.plot)
fancyRpartPlot(modFit$finalModel)

#Predicting new Values
predict(modFit,newdata=testing)

