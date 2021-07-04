library(caret)
library(readr)
library(tidyverse)
library(e1071)
library(scorecard)

# Reading the CSV file and seperating the file using the semi colan seperator.
bank_df <- read.csv("bank-additional-full.csv",header = TRUE, sep = ";")

summary(bank_df)
View(bank_df)

# Removing the NA values from the dataset if present
is.na(bank_df) <- sapply(bank_df,is.infinite)
na.omit(bank_df)

#Converting the categorical values to numeric values.
bank_df$y <- ifelse(bank_df$y == "yes",1,0)

bank_df$y <- as.factor(bank_df$y)

bank_df$y

# Changing the column names of the dependent and independent variables.
bank_df$result <- bank_df[,21]
bank_df <- bank_df[,-21]

bank_df$prev_camp <- bank_df[,14]
bank_df <- bank_df[,-14]

bank_df$curr_camp <- bank_df[,12]
bank_df <- bank_df[,-12]

View(bank_df)
ncol(bank_df)

# Creating a partion of data for further analysis using CARET package
set.seed(12345)

partioning_data <- createDataPartition(bank_df$result, p=0.7, list=FALSE)

train_bank <- bank_df[partioning_data,]
test_bank <- bank_df[-partioning_data,]

train_bank_stratified <- split_df(train_bank,ratio = 0.35)

train <- train_bank_stratified$train
test <- train_bank_stratified$test

dim(train)
dim(test)

train <- as.data.frame(train)
test <-  as.data.frame(test)

# Training the traing data in the method called repeated cross validation 

train_method <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(12345)

svm_training <- train(result ~ ., 
                      data = train, method ="svmLinear",
                      trControl=train_method,preProcess=c("center","scale"),
                      tuneLength = 10)

svm_training

# Predicting the accuracy and the kappa score of the model using the training data 
svm_testing <- predict(svm_training, newdata = test)

svm_testing

# Confusion Matrix, for the user to understand the model's capability to predict accurately.

confusionMatrix(positive = "1",table(svm_testing,test$result))

# SVM model tuning: in process

grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))

set.seed(12345)

svm_Linear_Grid <- train(result ~., data = train, method = "svmLinear",
                         trControl=train_method,
                         preProcess = c("center", "scale"),
                         tuneGrid = grid,
                         tuneLength = 10)


svm_Linear_Grid

plot(svm_Linear_Grid)

test_pred_grid <- predict(svm_Linear_Grid, newdata = test)

test_pred_grid

confusionMatrix(test_pred_grid, test$result, positive = "1")
