library(caret)
setwd("C:/Users/Alfonso/Desktop/JOM/Practical_Machine_Learning")
training<-read.csv("pml-training.csv",header=T)
testing<-read.csv("pml-testing.csv",header=T)


#####################################
##########      TIPS       ##########
#####################################

#I didn't treat this as a time series. 
#Random Forest gives very accurate result in this case. 
#You do need to do some initial cleanup and also run the preProcess for rf to run efficiently. 

#Try Boosting


#(For a tidy data set of this course project, the columns is about 50~60.)
#Take a smaller size of training set (subset of training data). 
#(I did the course project with only 1000~2000 records in training set and got approximately 90% precision.)
#Set ntree parameter when using randomForest(). (ntree around 100 is enough.)

#Get Rid of X as one of the predictors. 

