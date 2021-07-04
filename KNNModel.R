library(scorecard)
library(dplyr)
library(class)
library(e1071)
library(caret)
#Read the Dataset
bank <- read.csv("bank_additional_full.csv",header = TRUE,sep = ";")
bank <- bank %>% mutate(Result = ifelse(y == "no",0,1))

bank_new <- model.matrix(y~.,bank)
# View(bank)
View(bank_new)

#check the class of each columns
sapply(bank, class)       
sapply(bank_new, class)
typeof(bank_new) #datatype check

bank_new <- as.data.frame(bank_new)
typeof(bank_new)

bank_list <- split_df(bank_new, ratio = 0.70, seed = 12345)

#KNN MODEL
create_train_test <-
  function(data, size = 0.8, train = TRUE) {
    n_row = nrow(data)
    total_row = size * n_row
    train_sample <- 1:total_row
    if (train == TRUE) {
      return (data[train_sample, ])
    }
    else {
      return (data[-train_sample, ])
    }
  }
#partitioning the sets as train and test (70 and 30 percentile)
bank_train <- create_train_test(bank_new, 0.7, train = TRUE)
bank_test <- create_train_test(bank_new, 0.7, train = FALSE)


typeof(bank_train)

# View(bank_train)
View(bank_test)

library(ggplot2)

#CONFUSION MATRIX#
yha_2 <- knn(bank_train, bank_test, bank_train$Result, k = 3, prob = TRUE)
cm_3 <- confusionMatrix(table(yha_2,bank_test$Result),positive = '1') 
cm_3


yha_10 <- knn(bank_train, bank_test, bank_train$Result, k = 11, prob = TRUE)
cm_11 = confusionMatrix(table(yha_10,bank_test$Result),positive = '1')
cm_11


yha_20 <- knn(bank_train, bank_test, bank_train$Result, k = 21, prob = TRUE)
cm_21 = confusionMatrix(table(yha_20,bank_test$Result),positive = '1')
cm_21


yha_30 <- knn(bank_train, bank_test, bank_train$Result, k = 31, prob = TRUE)
cm_31 <- confusionMatrix(table(yha_30,bank_test$Result),positive = '1')
cm_31

yha_50 <- knn(bank_train, bank_test, bank_train$Result, k = 51, prob = TRUE)
cm_51 <- confusionMatrix(table(yha_50,bank_test$Result),positive = '1')
cm_51


yha_100 <- knn(bank_train, bank_test, bank_train$Result, k = 101, prob = TRUE)
cm_101 <- confusionMatrix(table(yha_100,bank_test$Result),positive = '1')
cm_101


yha_150 <- knn(bank_train, bank_test, bank_train$Result, k = 151, prob = TRUE)
cm_151 <- confusionMatrix(table(yha_150,bank_test$Result),positive = '1')
cm_151


#Training Data with K value 2
for(i in 1:(ncol(bank_test)-1)) {
  plot(bank_test$Result,bank_test[[paste0('x',i)]],ylab='Predictors',xlab='Result',pch=20,col='blue',main=paste('Training Data with K value 2'))
}
points(bank_test$Result,yha_2,col='red',xlab = 'result',ylab = 'test')


#Training Data with K value 10
for(i in 1:(ncol(bank_test)-1)) {
  plot(bank_test$Result,bank_test[[paste0('x',i)]],ylab='Predictors',xlab='Result',pch=20,col='blue',main=paste('Training Data with K value 10'))
}
points(bank_test$Result,yha_10,col='red',xlab = 'result',ylab = 'test')






