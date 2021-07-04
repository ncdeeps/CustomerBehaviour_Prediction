library(dplyr)
library(rpart)
library(caret)
library(rpart.plot)
library(data.table)
library(e1071)
library(caTools)
library(Ecdat)

#Reading the bank data file
MyBank  <- read.csv(file="bank-additional-full.csv",header=TRUE, sep=";" )
data.frame(MyBank)

#removing the Duration column
MyBank$duration <- NULL

#Spliting the dataset to .7 of train and .3 of test
data1= sample.split(MyBank,SplitRatio = 0.7)
Train =subset(MyBank,data1==TRUE)
Test =subset(MyBank,data1==FALSE)

#BUiding model for Decision tree
Result <- rpart(y~.,data = Train, method = 'class')

#Ploting the decision tree
rpart.plot(Result,extra = 100)

#Make a prediction
predict_Test <-predict(Result, Test, type = 'class')
predict_Test
predict_Train <-predict(Result, Train, type = 'class')
predict_Train


#Confusion matrix
cm <- confusionMatrix(table(Test$y,predict_Test),positive = "yes")
cm
cm1 <- confusionMatrix(table(Train$y,predict_Train),positive = "yes")
cm1

