library(dplyr)
library(caret)
require(randomForest)
library(party)
library(e1071)
library(data.table)
library(readr)

#Import the data
MyData  <- read.csv(file="bank-additional-full.csv",header=TRUE, sep=";" )
setnames(MyData, "y", "Result")
shuffle_index <- sample(1:nrow(MyData))
MyData <- MyData[shuffle_index,]
head(MyData)
tail(MyData)
glimpse(MyData)

#Clean the dataset
#no NA in data set

#Create train/test set

set.seed(12345)
row.number = sample(1:nrow(MyData), 0.7*nrow(MyData))
data_train = MyData[row.number,]
data_test = MyData[-row.number,]
data_train = select(data_train,-11)
dim(data_train)
head(data_train)
data_test = select(data_test,-11)
dim(data_test)
head(data_test)


#Build the model

Result <- randomForest(factor(Result)~.,data = data_train, method = 'class')
Result
#importance(Result)
#varImpPlot(Result)
tree <- getTree(Result, 1, labelVar=TRUE)
tree
plot(Result)
Result.legend <- if (is.null(Result$test$err.rate)) {colnames(Result$err.rate)}
                  {colnames(Result$test$err.rate)}
legend("top", cex =0.5, legend=Result.legend, lty=c(1,2,3), col=c(1,2,3), horiz=T)

#Plotting tree

x <- ctree(factor(Result)~., data=data_train)
plot(x, type="simple")

# 5.1 Prediction in Test set

predict_test <-predict(Result, data_test, type = 'class')
head(predict_test)
confusionmatrix_test <- table((predict_test),data_test$Result)
confusionMatrix(confusionmatrix_test,positive="yes") 
mean(predict_test == data_test$Result)
mean(predict_test != data_test$Result)

# 5.2 Prediction in Train set
predict_train <-predict(Result, data_train, type = 'class')
head(predict_train)
confusionmatrix_train <- table((predict_train),data_train$Result)
confusionMatrix(confusionmatrix_train,positive="yes") 
mean(predict_train == data_train$Result)
mean(predict_train != data_train$Result)


