library(tidyverse)
# TO READ THE DATA AND CONVERT TO A DATAFRAME
bankData=read.table("bank-additional-full.csv",header=TRUE,sep=";")

#TO UNDERSTAND THE NATURE OF THE DATA
bankData %>% class
bankData %>% names
bankData %>% nrow
bankData %>% ncol
bankData %>% summary
bankData %>% glimpse
bankData %>% tail(5)
bankData %>% head(5)
##########
#RESULT::#
##########
#1. There are 41188 rows with 21 columns. The first twenty column constitiutes the predictor variables.
#2. The last column namely the variable 'y' constitute the response.


#TO CHECK IF THERE ARE NA VALUES IN ANY ROWS
bankData %>% filter_all(any_vars(is.na(.)))



#TO SPLIT DATA INTO TRAIN (70% of the data) AND TEST (30% of the data)#
#######################################################################

# library(caret) # createDataPartition

#70% of the sample bank data
smpSize <- floor(0.70 * nrow(bankData))

#Set the seed to make partition reproducible
set.seed(12345)
trainIndex <- sample(seq_len(nrow(bankData)), size = smpSize)

trainBankData <- bankData[trainIndex, ]
testBankData <- bankData[-trainIndex, ]
dim(trainBankData)

#########################################################
#DATA MODEL USING GENERALISED LINEAR LOGISTIC REGRESSION#
#########################################################

#Using ALL the input predictors:
logFit <- glm(y ~ . - duration, data = trainBankData, family = binomial())
summary(logFit)
logFit %>% coef()
##########
#RESULT::#
##########
#1. From the summary p values, it can be seen that not all predictors are useful.
#2. Only Predictors with p values closer to zero arre considered further for analysis. These include:
#job, education, default, contact, month, day_of_week, duration, campaign, pdays, previous, poutcomeemp.var.rate, 
#cons.price.idx, cons.conf.idx, euribor3m, nr.employed



#################ALL CUSTOMERS#####################
#Using only some significant predictors as deduced form the previous result:
#logFitFinal <- glm(y ~ . -duration -age, data = trainBankData, family = binomial)
logFitFinal <- glm(y ~ . -duration, data = trainBankData, family = binomial)
summary(logFitFinal)


#logFitFinal <- glm(y ~ job + education + default + contact + month + day_of_week + duration + campaign + 
#                     pdays + previous + poutcome + emp.var.rate + cons.price.idx + cons.conf.idx + euribor3m + 
#                     nr.employed, data = trainBankData, family = binomial)
#summary(logFitFinal)
##########
#RESULT::#
##########
#1. From the summary p values, it can be seen that all predictors used for the model have significant p value.


####################################################
#PREDICTION AND ACCURACY OF MODEL FOR TRAINING DATA#
####################################################
#Using the logFitFinal Model to find the probabilities that the train response belong to yes or no classes:
contrasts(bankData$y)
logPredTrain <- predict(logFitFinal,type = "response")
logClassificationTrainData <- rep("no",length(trainBankData$y))
logClassificationTrainData[logPredTrain > 0.5] = "yes"

#Confusion Matrix to understand the Accuracy of the Genralized Linear Logistic Regression Model Accuracy:
confusionMatrixTrain <- table(logClassificationTrainData, trainBankData$y)
confusionMatrixTrain

#sensitivity(confusionMatrixTrain)
##########
#RESULT::#
##########
#logClassificationTrainData   no    yes
#                     no     25253  2483
#                     yes    363    732

#Accuracy and Error:
 (25253 + 732)/length(trainBankData$y)


accuracyTrain <- mean(logClassificationTrainData == trainBankData$y) *100
accuracyTrain
errorTrain <- mean(logClassificationTrainData != trainBankData$y) *100
errorTrain
########
#RESULT#
########
#This gives 90.1 % accuracy in predicting.



######################################################
#PREDICTION AND ACCURACY OF MODEL FOR "TESTING DATA" #
######################################################
#Using the logFitFinal Model to find the probabilities that the test response belong to yes or no classes:
contrasts(testBankData$y)
logPredTest <- predict(logFitFinal, testBankData, type = "response")
logClassificationTestData <- rep("no",length(testBankData$y))
logClassificationTestData[logPredTest > 0.5] = "yes"

#Confusion Matrix to understand the Accuracy of the Genralized Linear Logistic Regression Model Accuracy:
confusionMatrixTest <- table(logClassificationTestData, testBankData$y)
confusionMatrixTest
##########
#RESULT::#
##########
#   logPred    no    yes
#       no    10766  1103
#       yes   166    322

#Accuracy and Error:
 (10766 + 322)/length(testBankData$y)

accuracyTest <- mean(logClassificationTestData == testBankData$y)
accuracyTest
errorTest <- mean(logClassificationTestData != testBankData$y)
errorTest
########
#RESULT#
########
#This gives 89.72% accuracy in predicting.


##HIGHER DECISION BOUNDARY
##########################
logClassificationTestDataHigh <- rep("no",length(testBankData$y))
logClassificationTestDataHigh[logPredTest > 0.7] = "yes"

#Confusion Matrix to understand the Accuracy of the Genralized Linear Logistic Regression Model Accuracy:
confusionMatrixTestPt6 <- table(logClassificationTestDataHigh, testBankData$y)
confusionMatrixTestPt6
mean(logClassificationTestDataHigh == testBankData$y)
mean(logClassificationTestDataHigh != testBankData$y)

##LOWER DECISION BOUNDARY
##########################
logClassificationTestDataLow <- rep("no",length(testBankData$y))
logClassificationTestDataLow[logPredTest > 0.1] = "yes"

#Confusion Matrix to understand the Accuracy of the Genralized Linear Logistic Regression Model Accuracy:
confusionMatrixTestPt6 <- table(logClassificationTestDataLow, testBankData$y)
confusionMatrixTestPt6
mean(logClassificationTestDataLow == testBankData$y)
mean(logClassificationTestDataLow != testBankData$y)

#Confusion Matrix Function with Input Decision Boundary
confusionMatrixF <- function(decision){
  logData <- rep("no",length(testBankData$y))
  logData[logPredTest > decision] = "yes"
  
  #Confusion Matrix to understand the Accuracy of the Genralized Linear Logistic Regression Model Accuracy:
  confusionMatrixFound <- table(logData, testBankData$y)
  acc <- mean(logData == testBankData$y)
  err <- mean(logData != testBankData$y)
  paste("Accuracy:",round(acc,4), " Error:", round(err,4))
}

#Tuning Logistic Regression
confusionMatrixF(0.1)
confusionMatrixF(0.2)
confusionMatrixF(0.5)
confusionMatrixF(0.7)
confusionMatrixF(0.8)


#LOGISTIC REGRESSION TUNING PARAMETERS PLOT
logisticParameter <- data.frame(decisionBoundary =c(0.7,0.5,0.2,0.1),
                                errorRate = c(10.74,10.28,15.52,20.27),
                                accuracy =c(89.26,89.72,86.48,79.73),
                                sensitivity =c(10.39,22.6,58.25,65.96),
                                specificity =c(99.54,98.47,90.16,81.52))
logisticParameter %>% gather(key, value, errorRate,accuracy, sensitivity,specificity) %>% 
  ggplot(aes(x=decisionBoundary,y=value,colour = key))+geom_line() +
  scale_color_manual(values=c("green", "red", "blue", "brown"))+
  xlab("Decision Boundary") +
  ylab("Percentage") +
  scale_x_continuous(breaks = seq(0,1,0.2))+
  scale_y_continuous(breaks = seq(0,100,10))

