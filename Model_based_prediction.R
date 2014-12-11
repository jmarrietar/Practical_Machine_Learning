data(iris)
library(ggplot2)
names(iris)

table(iris$Species)

inTrain<-createDataPartition(y=iris$Species,p=0.7,list=F)

training<-iris[inTrain,]
testing<-iris[-inTrain,]

#Build Predictions (lda and Naive bayes)
modlda=train(Species~.,data=training,method="lda")
modnb=train(Species~.,data=training,method="nb")

plda=predict(modlda,testing)
pnb=predict(modnb,testing)

table(plda,pnb)



