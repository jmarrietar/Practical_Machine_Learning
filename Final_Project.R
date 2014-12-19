library(caret)
#setwd("C:/Users/Alfonso/Desktop/JOM/Practical_Machine_Learning")
setwd("C:/Users/JosePortatil/Dropbox/Data Science/Practical_Machine_Learning")
training<-read.csv("pml-training.csv",header=T,na.strings = "")
testing<-read.csv("pml-testing.csv",header=T,na.strings = "")


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

####################################################

colnames(training)
summary(training)

###################################
#######  CLEANING PART ############
###################################

#***** Clean Treaning Data. 

#FIRST PHASE 

#Drop Not relevant Columns. 
training$X<-NULL
training$user_name<-NULL 
training$raw_timestamp_part_1<-NULL
training$raw_timestamp_part_2<-NULL
training$cvtd_timestamp<-NULL
training$new_window<-NULL
training$num_window<-NULL
training$problem_id<-NULL

#Phase 2 
#Convert COlumn Variables to Numeric [First convert to Character then to Numeric]
training[, -c(153)] <- sapply(training[,-c(153)], as.character)
training[, -c(153)] <- sapply(training[,-c(153)], as.numeric)

#Drop columns with lots of NA values 
delete_columns<-which(colSums(is.na(training))>19000)
training<-training[,-c(delete_columns)]

###CLEAN TESTING DATA 

#FIRST PHASE 

#Drop Not relevant Columns. 
testing$X<-NULL
testing$user_name<-NULL 
testing$raw_timestamp_part_1<-NULL
testing$raw_timestamp_part_2<-NULL
testing$cvtd_timestamp<-NULL
testing$new_window<-NULL
testing$num_window<-NULL
testing$problem_id<-NULL

#Convert COlumn Variables to Numeric [First convert to Character then to Numeric]
testing[, -c(153)] <- sapply(testing[,-c(153)], as.character)
testing[, -c(153)] <- sapply(testing[,-c(153)], as.numeric)

#Drop columns with lots of NA values 
testing<-testing[,-c(delete_columns)]

###############################################
###### CREATE A MODEL & CROSS VALIDATE ########
###############################################
#SAMPLE DATA SET TO DECREASE TRAINING TIME 

# take a random sample of size 1000 from a dataset mydata 
# sample without replacement
training_sample <- training[sample(1:nrow(training), 2000,
                          replace=FALSE),]
#A testing set 
test_sample<- training[sample(1:nrow(training), 2000,
                                   replace=FALSE),]

# define training control
train_control <- trainControl(method="cv", number=10)
# train the model 
model <- train(classe~., data=training_sample, trControl=train_control, method="rf")
# make predictions on testing set and make confusion Matrix
predictions <- predict(model, test_sample[,1:52])
# summarize results
confusionMatrix(predictions, test_sample$classe)


###############################################
############ PREDICTION ##################
##############################################

Final_predictions <- predict(model, testing)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}


pml_write_files(Final_predictions)
