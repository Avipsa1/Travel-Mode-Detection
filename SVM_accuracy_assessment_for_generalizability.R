library(tidyverse)
library(caret)
library(e1071)
library(neuralnet)
library(randomForest)
library(kernlab)
library(mlnoost)
library(caTools)
library(dplyr)
library(fakeR)
library(pROC)
setwd("C:/Users/aroy29/Dropbox (ASU)/INTERACT Travel Mode Detection/Data/")



window_size = c(5,10)
svm_model <- list()
true_labels <- list()
predicted_labels <- list()
prediction_probs <- list()
train.features <- list()
test.features <- list()
validation.features <- list()
conf.Mat <- list()
rocCurve <- list()
for(i in 1:6)
{
  acc_features = read.csv(paste("Acc_calculated_features_window_size_",window_size[i],".csv",sep=""))
  gps_features = read.csv(paste("GPS_calculated_features_window_size_",window_size[i],".csv",sep=""))
  gis_features = read.csv(paste("GIS_calculated_features_window_size_",window_size[i],".csv",sep=""))
  input.features = data.frame(cbind(acc_features[,-c(1)],gps_features[,-c(1,26)],gis_features[,-c(1,44)]))
  print("Input Features created..")
  print(nrow(input.features))
  n = nrow(input.features)
  train.idx = sample(seq_len(n), floor(0.7*n))
  train.features[[i]] = input.features[train.idx,] 
  train.features[[i]][is.na(train.features[[i]])] <- 0
  validation.features[[i]] = simulate_dataset(input.features,use.levels = TRUE,level3.noise=TRUE, n = floor(n/2))
  print("Train, test and validation sets created..")
  true_labels[[i]] = validation.features[[i]]$Mode
  train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
  svm_model[[i]] <- train(Mode ~., data = train.features[[i]], method = "svmRadial",trControl = train.control)
  print(paste("Fitted SVM model for window size =",window_size[i]))
  prediction_probs[[i]] <- predict(svm_model[[i]], validation.features[[i]], type = "prob")
  predicted_labels[[i]] <- predict(svm_model[[i]], validation.features[[i]])
  print("Predictions complete!")
  rocCurve[[i]]   <- multiclass.roc(true_labels[[i]],as.numeric(predicted_labels[[i]]))
  print("ROC curve created!")
  conf.Mat[[i]] <- table(true_labels[[i]],predicted_labels[[i]])
  print("Confusion Matrix created!")
}


