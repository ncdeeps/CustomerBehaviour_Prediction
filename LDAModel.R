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
smpSize <- floor(0.7* nrow(bankData))

#Set the seed to make partition reproducible
set.seed(12345)
trainIndex <- sample(seq_len(nrow(bankData)), size = smpSize)

trainBankData <- bankData[trainIndex, ]
testBankData <- bankData[-trainIndex, ]


###############################################
#DATA MODEL USING LINEAR DISCRIMINANT ANALYSIS#
###############################################
library(MASS)

ldaFit <- lda( y ~ . -duration, data =trainBankData)
ldaFit

#######################################################
#PREDICTION AND ACCURACY OF MODEL FOR "TRAININF DATA" #
#######################################################
ldaTrainPred <- predict(ldaFit,trainBankData)
names(ldaTrainPred)

#Confusion Matrix to understand the Accuracy of the Genralized Linear Logistic Regression Model Accuracy:
lda_ConfusionMatrixTrain <- table(ldaTrainPred$class,trainBankData$y)
lda_ConfusionMatrixTrain

########
#RESULT#
########
#      no    yes
#no  24499   2030
#yes  1117   1185

#Accuracy and Error:
ldaAccuracyTrain <- (24499 + 1185)/length(trainBankData$y)
ldaAccuracyTrain

accuracyTrain <- mean(ldaTrainPred$class == trainBankData$y)
accuracyTrain
errorTrain <- mean(ldaTrainPred$class != trainBankData$y)
errorTrain
########
#RESULT#
########
#This gives 89.1 % accuracy in predicting.


######################################################
#PREDICTION AND ACCURACY OF MODEL FOR "TESTING DATA" #
######################################################
ldaTestPred <- predict(ldaFit,testBankData)
names(ldaTestPred)

#Confusion Matrix to understand the Accuracy of the Genralized Linear Logistic Regression Model Accuracy:
lda_ConfusionMatrixTest <- table(ldaTestPred$class,testBankData$y)
lda_ConfusionMatrixTest

########
#RESULT#
########
#      no    yes
#no   10463  903
#yes   469   522

#Accuracy and Error:
ldaAccuracyTest <- (10463 + 522)/length(testBankData$y)
ldaAccuracyTest

accuracyTest <- mean(ldaTestPred$class == testBankData$y)
accuracyTest
errorTest <- mean(ldaTestPred$class != testBankData$y)
errorTest
########
#RESULT#
########
#This gives 88.9 % accuracy in predicting.











