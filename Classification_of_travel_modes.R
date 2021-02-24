#Classify using multiple models
library(tidyverse)
library(caret)
library(dplyr)
library(ggplot2)
library(gridExtra)
setwd("C:/Users/aroy29/Dropbox (ASU)/INTERACT Travel Mode Detection/Data/")

#Function to split input features into Acc/GPS/GIS feature set combinations with specific window sizes
createFeatureSets <- function(window_size,featureSet)
{
  print("Reading raw features from file..")
  #Read raw features from CSV files
  acc_features = read.csv(paste("Acc_calculated_features_window_size_",window_size,"_July28.csv",sep=""))
  gps_features = read.csv(paste("GPS_calculated_features_window_size_",window_size,"_July28.csv",sep=""))
  gis_features = read.csv(paste("GIS_calculated_features_window_size_",window_size,"_July28.csv",sep=""))
  
  #Create feature sets
  print(paste("Creating feature subsets for window size..",window_size))
  if(featureSet=='GPS_only')
  {
    #1. GPS_only
    input.features = gps_features[,-c(1)]
    print("GPS only features created!")
  }
 else if(featureSet=='GPS_Acc')
 {
  #2. GPS_Acc
  input.features = cbind(acc_features[,-c(1)],gps_features[,-c(1,26)])
  print("GPS & Accelerometer features created!")
 }
  else if(featureSet=='GPS_Acc_GIS')
  {
  #3. GPS_Acc_GIS
  input.features = cbind(acc_features[,-c(1)],gps_features[,-c(1,26)],gis_features[,-c(1,44)])
  print("GPS, Accelerometer and GIS features created!")
  }
  else if(featureSet=='GPS_GIS')
  {
    #4. GPS_GIS
    input.features = cbind(gps_features[,-c(1)],gis_features[,-c(1,44)])
    print("GPS and GIS features created!")
  }
  else if(featureSet=='Acc_only')
  {
    #5. Acc_only
    input.features = acc_features[,-c(1)]
    print("Accelerometer features created!")
  }
  else
  {
  #6. Acc_GIS
  input.features = cbind(acc_features[,-c(1)],gis_features[,-c(1,44)])
  print("Accelerometer Features created!")
  }
  print('Feature subset creation complete..')
  #print(head(input.features))
  return(input.features)
  
}

#Function to fit multiple models using input features with a specific window size
fit_models <- function(clf,featureSet,window_size)
{
  set.seed(123)
  data <- createFeatureSets(window_size,featureSet)
  data[is.na(data)] <- 0
  print(paste("No. of rows in original data=",nrow(data)))
  print("Generating Training sample:")
  x = createDataPartition(data$Mode,p=0.7, list = FALSE, times = 1) #Create a 70:30 split
  train_data <- data[x,]
  print(paste("No. of rows in training set =",nrow(train_data)))
  print(paste("Train classes:",table(train_data$Mode)))
  test_data <- data[x,]
  print(paste("Test classes:",table(test_data$Mode)))
  write.csv(train_data,paste(featureSet,"ws",window_size,"train_July28.csv",sep="_"))
  write.csv(test_data,paste(featureSet,"ws",window_size,"test_July28.csv",sep="_"))
  
  #Set up a 10 fold cross validation for training all models
  train.control <- trainControl(method = "repeatedcv", 
                                number = 10, repeats = 3)
  if(clf=='svmLW')
  {
    print("Fitting Random forest model begins..")
    #Random Forest classifier
    model <- train(Mode ~., data = train_data, method = "svmLinearWeights2",
                   ntree = 100,metric="ROC",
                    trControl = train.control)
    print("SVM with class weights fitted!")
  }
  else if(clf=='svmR')
  {
   # Build smote model
    
    train.control$sampling <- "smote"
    model <- train(Mode ~ .,
                       data = train_data,
                       method = "svmRadial",
                       verbose = FALSE,
                       metric = "ROC",
                       trControl = train.control)
    #support vector machine  -with a Radial basis function
    #model <- train(Mode ~., data = train_data, method = "svmRadial",
    #              trControl = train.control)
    print("SVM radial-basis model fitted..")
  }
  else if(clf=='svmL')
  {
    #Multinomial Logit
    model <- train(Mode ~., data = train_data, method = "svmLinear2",
                    trControl = train.control)
    print("SVM-with linear kernel fitted..")
  }
  else if(clf=='svmP')
  {
    #Neural Net
    model <- train(Mode ~., data = train_data, method = "svmPoly",
                    trControl = train.control)
    print("SVM with polynomial kernel fitted..")
  }
  else if(clf=='svmRW')
  {
    #Gradient Boosting
    model <- train(Mode ~., data = train_data, method = "svmRadialWeights",
                    trControl = train.control)
  
    print("SVM with radial weights fitted..")
    }
  filename = paste("C:/Users/aroy29/Dropbox (ASU)/INTERACT Travel Mode Detection/Results/model outputs/",clf,"_model_results_",featureSet,"_window_size_",window_size,"_june30.rda",sep="")
  print("Writing model results to file...")
  save(model,file=filename)
  print("Completed!")
  return(model)
}

#Use Classifier/Feature set combinations to generate outputs for:

#Support Vector Machines
window_size = c(3,5,7,10)
svm_model1 <- list() #GPS only
svm_model2 <- list()#Acc only
svm_model3 <- list() #GPS and Acc
ecdf1 <- list()
ecdf2 <- list()
ecdf3 <- list()

for(i in 1:length(window_size))
{
  svm_model1[[i]] <- fit_models('svmL','GPS_only',window_size[i])
  svm_model2[[i]] <- fit_models('svmL','Acc_only',window_size[i])
  svm_model3[[i]] <- fit_models('svmL','GPS_Acc',window_size[i])
  ecdf1[[i]] = ecdf(svm_model1[[i]]$resample$Accuracy)
  ecdf2[[i]] = ecdf(svm_model2[[i]]$resample$Accuracy)
  ecdf3[[i]] = ecdf(svm_model3[[i]]$resample$Accuracy)
  svm_model1[[i]]$resample$window_size = window_size[i]
  svm_model1[[i]]$resample$feature_set = 'GPS_only'
  svm_model2[[i]]$resample$window_size = window_size[i]
  svm_model2[[i]]$resample$feature_set = 'Acc_only'
  svm_model3[[i]]$resample$window_size = window_size[i]
  svm_model3[[i]]$resample$feature_set = 'GPS_Acc'
}

#Check 10-fol CV accuracy of SVM models for varying sizes 
par(mfrow=c(2,2))
for(i in 1:length(window_size))
{
  plot(ecdf1[[i]],do.points= FALSE, verticals = TRUE, col = "red", lwd = 4,
       xlab = "10-fold Cross-validation Accuracy", ylab = "% Training data", 
       main = paste("Window size  =", window_size[i]), xlim = c(0,1), cex = 1.5, cex.axis=1.5,
       cex.lab=1.5,cex.main=1.5)
  plot(ecdf2[[i]], verticals=TRUE, do.points=FALSE, add=TRUE, col='blue',lwd=4)
  plot(ecdf3[[i]], verticals=TRUE, do.points=FALSE, add=TRUE, col='green',lwd=4)
  
  legend("topleft",legend = c("GPS Only","Acc only","GPS + Acc"), #,"GPS + Acc + GIS"
         col = c("red","blue","green"), lty = c(1,1,1), cex = 1.05, lwd = c(4,4,4))
}

par(mfrow=c(1,1))
par(mar = c(7,7,6,6), bg = "#f4f4f4")
plot(ecdf1[[1]],do.points= FALSE, verticals = TRUE, col = "#A359BD", lwd = 4,
     xlab = "10-fold Cross-validation Accuracy", ylab = "% Training data", 
     main = paste("Window size = 3s"), xlim = c(0,1), cex = 1.5, cex.axis=1.5,
     cex.lab=1.5,cex.main=1.5)
plot(ecdf2[[1]], verticals=TRUE, do.points=FALSE, add=TRUE, col="#329E96",lwd=4)
plot(ecdf3[[1]], verticals=TRUE, do.points=FALSE, add=TRUE, col="#FFC300",lwd=4)

legend("topleft",legend = c("GPS Only","Acc only","GPS + Acc"), #,"GPS + Acc + GIS"
       col = c("#A359BD","#329E96","#FFC300"), lty = c(1,1,1), cex = 1.05, lwd = c(4,4,4))

#Generate boxplots for 10-fold CV accuracy for different feature sets

plot_accuracy = data.frame("Accuracy"=c(),"Kappa"=c(),"Resample"=c(),
                           "window_size"= c(), "feature_set"=c())
for(i in 1:length(window_size))
{
  df1 = svm_model1[[i]]$resample
  df2 = svm_model2[[i]]$resample
  df3 = svm_model3[[i]]$resample
  df = rbind(df1,df2,df3)
  plot_accuracy = rbind(plot_accuracy,df)
}

plot_accuracy$window_size = factor(plot_accuracy$window_size, levels = unique(plot_accuracy$window_size))
plot_accuracy$feature_set = factor(plot_accuracy$feature_set, levels = c("GPS_only","Acc_only","GPS_Acc"))
str(plot_accuracy)
write.csv(plot_accuracy,"../Results/10-foldCV-accuracy-SVM.csv")

mean_accu = aggregate(Accuracy~feature_set+window_size,plot_accuracy,mean)
median_accu = aggregate(Accuracy~feature_set+window_size,plot_accuracy,median)
max_accu = aggregate(Accuracy~feature_set+window_size,plot_accuracy,max)
min_accu = aggregate(Accuracy~feature_set+window_size,plot_accuracy,min)
sd_accu = aggregate(Accuracy~feature_set+window_size,plot_accuracy,sd)
iqr_accu = aggregate(Accuracy~feature_set+window_size,plot_accuracy,IQR)
conf_int_accu = aggregate(Accuracy~feature_set+window_size,plot_accuracy,function(x) t.test(x)$conf.int)

accuracy_summary = data.frame(feature_set = mean_accu$feature_set,
                              window_size = mean_accu$window_size,
                              mean_accuracy = mean_accu$Accuracy,
                              median_accuracy = median_accu$Accuracy,
                              max_accuracy = max_accu$Accuracy,
                              min_accuracy = min_accu$Accuracy,
                              sd_accuracy = sd_accu$Accuracy,
                              iqr_accuracy = iqr_accu$Accuracy,
                              '5%CI' = conf_int_accu$Accuracy[,1],
                              '95%CI' = conf_int_accu$Accuracy[,2])
accuracy_summary
'''
   feature_set window_size mean_accuracy median_accuracy max_accuracy min_accuracy sd_accuracy iqr_accuracy
1     GPS_only           3     0.6369243       0.6295732    0.7435897    0.5714286  0.04683574   0.05853659
2     Acc_only           3     0.5527433       0.5500000    0.6000000    0.4761905  0.02814199   0.03841463
3      GPS_Acc           3     0.6857283       0.6913919    0.8000000    0.5238095  0.06442460   0.08885017
4     GPS_only           5     0.6522187       0.6666667    0.7826087    0.5384615  0.05777550   0.06221154
5     Acc_only           5     0.5513201       0.5600000    0.6086957    0.5200000  0.02081297   0.01833333
6      GPS_Acc           5     0.6478703       0.6400000    0.8333333    0.5416667  0.06666572   0.09173913
7     GPS_only           7     0.6316260       0.6213450    0.8125000    0.4444444  0.08679659   0.07843137
8     Acc_only           7     0.5489630       0.5555556    0.5882353    0.5000000  0.02576473   0.03135212
9      GPS_Acc           7     0.6602891       0.6568627    0.7777778    0.4705882  0.08612501   0.09722222
10    GPS_only          10     0.6243284       0.6153846    0.8333333    0.3636364  0.10307255   0.10897436
11    Acc_only          10     0.5613126       0.5549451    0.8000000    0.3846154  0.08563066   0.10737179
12     GPS_Acc          10     0.6645504       0.6666667    0.9166667    0.4615385  0.11092392   0.16098485
       X5.CI    X95.CI
1  0.6194355 0.6544131
2  0.5422349 0.5632517
3  0.6616718 0.7097849
4  0.6306450 0.6737924
5  0.5435485 0.5590918
6  0.6229769 0.6727637
7  0.5992156 0.6640363
8  0.5393423 0.5585837
9  0.6281295 0.6924487
10 0.5858405 0.6628164
11 0.5293376 0.5932876
12 0.6231308 0.7059701
'''
write.csv(accuracy_summary,"../Results/SVM_10-fold_CV-accuracy-summary-statistics.csv")

#Print variable importance
varImp(svm_model3[[4]])

'''
              Bicycle   Bus     Motor.Vehicle Sky.Train   Walk
sd_Acc          95.897 100.00       95.897    95.897    100.00
cv_Acc          95.897 100.00       95.897    95.897    100.00
sd_speed        65.562 100.00       58.974    58.974    100.00
cv_speed        61.026 100.00       85.375    61.026    100.00
skw_speed       45.641  92.31       73.986    45.641      92.31
krt_speed       16.923  86.15       66.673    10.611      86.15
iqr_Acc         84.379  84.62       78.462    78.462      84.62
sd_height       47.456  76.92       22.198    34.911      76.92
sd_rel.angle    44.615  63.08       74.585    44.615      63.08
cv_height       19.053  61.54       9.744     34.911      61.54
sd_net_disp      9.744  33.85       60.919     9.744      33.85
iqr_net_disp    15.897  33.85       59.840    15.897      33.85
sum_rel.angle   58.974  58.97       58.974    58.974      10.77
mean_rel.angle  58.974  58.97       58.974    58.974      10.77
sum_speed       54.872  54.87       58.402    54.872      48.52
mean_speed      54.872  54.87       58.402    54.872      48.52
sum_net_disp    49.744  49.74       49.744    49.744      49.23
mean_net_disp   49.744  49.74       49.744    49.744      49.23
ent_Acc         43.590  43.59       43.590    43.590      15.38
krt_Acc         35.385  36.92       35.385    35.779      36.92
'''

read_test_data <- function(featureSet,window_size)
{
  test <- read.csv(paste(featureSet,"ws",window_size,"test_July28.csv",sep="_"))
  return(test)
}

window_size = c(3,5,7,10)

test_data1 <- list()
test_data2 <- list()
test_data3 <- list()
predicted1 <- list()
predicted2 <- list()
predicted3 <- list()
conf_mat1 <- list()
conf_mat2 <- list()
conf_mat3 <- list()

for(i in 1:length(window_size))
{
  test_data1[[i]] <- read_test_data('GPS_only',window_size[i])
  print(head(test_data1))
  test_data2[[i]] <- read_test_data('Acc_only',window_size[i])
  print(head(test_data2))
  test_data3[[i]] <- read_test_data('GPS_Acc',window_size[i])
  print(head(test_data3))
  predicted1[[i]] <- predict(svm_model1[[i]],test_data1[[i]][,names(test_data1[[i]])!="Mode"])
  predicted2[[i]] <- predict(svm_model2[[i]],test_data2[[i]][,names(test_data2[[i]])!="Mode"])
  predicted3[[i]] <- predict(svm_model3[[i]],test_data3[[i]][,names(test_data3[[i]])!="Mode"])
  conf_mat1[[i]] <- confusionMatrix(predicted1[[i]],test_data1[[i]]$Mode)
  conf_mat2[[i]] <- confusionMatrix(predicted2[[i]],test_data2[[i]]$Mode)
  conf_mat3[[i]] <- confusionMatrix(predicted3[[i]],test_data3[[i]]$Mode)
}

gps_only_test_acc <- data.frame(rbind(conf_mat1[[1]]$overall,
                                      conf_mat1[[2]]$overall,
                                      conf_mat1[[3]]$overall,
                                      conf_mat1[[4]]$overall))
gps_only_test_acc$window_size <- c(3,5,7,10)
gps_only_test_acc$feature_set <- 'GPS Only'

acc_only_test_acc <- data.frame(rbind(conf_mat2[[1]]$overall,
                                      conf_mat2[[2]]$overall,
                                      conf_mat2[[3]]$overall,
                                      conf_mat2[[4]]$overall))
acc_only_test_acc$window_size <- c(3,5,7,10)
acc_only_test_acc$feature_set <- 'Acc only'

gps_acc_test_acc <- data.frame(rbind(conf_mat3[[1]]$overall,
                                     conf_mat3[[2]]$overall,
                                     conf_mat3[[3]]$overall,
                                     conf_mat3[[4]]$overall))
gps_acc_test_acc$window_size <- c(3,5,7,10)
gps_acc_test_acc$feature_set <- 'GPS + Acc'

test_acc <- rbind(gps_only_test_acc,acc_only_test_acc,gps_acc_test_acc)
test_acc[,c('AccuracyLower','Accuracy','AccuracyUpper','window_size','feature_set')]
'''
   AccuracyLower  Accuracy AccuracyUpper window_size feature_set
1      0.6421671 0.6894866     0.7340386           3    GPS Only
2      0.7049846 0.7632653     0.8150555           5    GPS Only
3      0.7267595 0.7942857     0.8515661           7    GPS Only
4      0.7786117 0.8536585     0.9109023          10    GPS Only
5      0.5102849 0.5599022     0.6086466           3    Acc only
6      0.4904875 0.5551020     0.6183672           5    Acc only
7      0.4945670 0.5714286     0.6458288           7    Acc only
8      0.5259498 0.6178862     0.7040059          10    Acc only
9      0.7031716 0.7481663     0.7895284           3   GPS + Acc
10     0.7576229 0.8122449     0.8591378           5   GPS + Acc
11     0.8557725 0.9085714     0.9468343           7   GPS + Acc
12     0.8456170 0.9105691     0.9545076          10   GPS + Acc
'''
write.csv(test_acc[,-c(2,7)],"../Results/Test_accuracy_by_window_size_July28.csv")

My_Theme = function(base_size = 12) {
  bg_color = "#f4f4f4"
  bg_rect = element_rect(fill = bg_color, color = bg_color)
  
  theme_bw(base_size) +
    theme(text = element_text(family = "Open Sans"),
          plot.title = element_text(family = "PT Sans", hjust = 0.5),
          plot.background = bg_rect,
          panel.background = bg_rect,
          legend.background = bg_rect,
          panel.grid.major = element_line(colour = "grey80", size = 0.25),
          panel.grid.minor = element_line(colour = "grey80", size = 0.25),
          legend.key.width = unit(1.5, "line"),
          legend.key = element_blank())
}


ggplot(plot_accuracy,aes(x=feature_set)) + 
  geom_boxplot(aes(y=Accuracy,fill=feature_set),position = "dodge") +
  scale_fill_manual(values= c("GPS_only" = "#A359BD", "Acc_only" = "#329E96","GPS_Acc" = "#FFC300")) + 
  My_Theme(18) + ggtitle("10-fold CV Accuracy of different feature sets") + 
  xlab("") + ylab("Accuracy (%)") + labs(fill = "Feature Sets")


library(ggbeeswarm)
ggplot(plot_accuracy,aes(x=window_size,y=Accuracy)) + 
  geom_quasirandom(aes(col = feature_set),priority='ascending',cex=3.5, size = 5.5)+ 
  xlab("Window Size") +
  ylab("Accuracy(%)") + My_Theme(18) + labs(color = "Feature Sets")+ 
  scale_color_manual(values= c("GPS_only" = "#A359BD", "Acc_only" = "#3f9e4d","GPS_Acc" = "#FFC300")) 


accuracy_summary[,c(1,2,3,7)]

'''
   feature_set window_size mean_accuracy sd_accuracy
3      GPS_Acc           3     0.6857283  0.06442460
6      GPS_Acc           5     0.6478703  0.06666572
9      GPS_Acc           7     0.6602891  0.08612501
12     GPS_Acc          10     0.6645504  0.11092392
'''

#Plot confusion matrix of GPS+Acc for - window size = 3s,5s,7s

df1 = data.frame(conf_mat3[[1]]$table)
g1 <- ggplot(data = df1, aes(x = Reference, y = Prediction)) + 
  geom_tile(aes(fill = Freq),colour = "white") +
  geom_text(aes(label = sprintf("%1.0f",Freq)), vjust = 1) + 
  scale_fill_gradient2(low = "blue", mid = "white",high = "red") + 
  My_Theme(18) + xlab ( "True Values") + 
  ylab ( "Predicted Values") + labs(fill = "No. of obs.") + 
  ggtitle("Confusion Matrix of GPS + Acc (ws = 3s)")

df2 = data.frame(conf_mat3[[2]]$table)
g2 <- ggplot(data = df2, aes(x = Reference, y = Prediction)) + 
  geom_tile(aes(fill = Freq),colour = "white") +
  geom_text(aes(label = sprintf("%1.0f",Freq)), vjust = 1) + 
  scale_fill_gradient2(low = "blue", mid = "white",high = "red") + 
  My_Theme(18) + xlab ( "True Values") + 
  ylab ( "Predicted Values") + labs(fill = "No. of obs.") + 
  ggtitle("Confusion Matrix of GPS + Acc (ws = 5s)")

df3 = data.frame(conf_mat3[[3]]$table)
g3 <- ggplot(data = df3, aes(x = Reference, y = Prediction)) + 
  geom_tile(aes(fill = Freq),colour = "white") +
  geom_text(aes(label = sprintf("%1.0f",Freq)), vjust = 1) + 
  scale_fill_gradient2(low = "purple",high = "red") + 
  My_Theme(18) + xlab ( "True Values") + 
  ylab ( "Predicted Values") + labs(fill = "No. of obs.") + 
  ggtitle("Confusion Matrix of GPS + Acc (ws = 5s)")

df4 = data.frame(conf_mat3[[3]]$table)
g4 <- ggplot(data = df4, aes(x = Reference, y = Prediction)) + 
  geom_tile(aes(fill = Freq),colour = "white") +
  geom_text(aes(label = sprintf("%1.0f",Freq)), vjust = 1) + 
  scale_fill_gradient2(low = "purple",high = "red") + 
  My_Theme(18) + xlab ( "True Values") + 
  ylab ( "Predicted Values") + labs(fill = "No. of obs.") + 
  ggtitle("Confusion Matrix of GPS + Acc (ws = 10s)")

g2
grid.arrange(g1,g2,g3,g4, ncol=2)
g2

#Print in-sample prediction accuracy - window size = 5s
train_data_ws10_gps_acc = read.csv(paste("../Data/GPS_Acc","ws",5,"train_June22.csv",sep="_"))
y_pred <- predict(svm_model3[[2]],train_data_ws10_gps_acc[,names(train_data_ws10_gps_acc)!="Mode"])
confusionMatrix(y_pred,train_data_ws10_gps_acc$Mode)

write.table(confusionMatrix(y_pred,train_data_ws10_gps_acc$Mode),"../Results/Confusion MAtrix - Test- ws - 10s.txt")

#Calculate ROC metrics and AUC for GPS_Acc- window size 5s
library(pROC)
y_pred <- as.ordered(y_pred)
auc <- multiclass.roc(train_data_ws10_gps_acc$Mode, y_pred, 
                      levels = c("Bicycle","Bus","Motor Vehicle","Sky Train","Walk"),
                      boot.n = 100)
rs <- auc[['rocs']]
plot.roc(rs[[1]],cex = 1.5, cex.axis=1.5,cex.lab=1.5,cex.main=1.5, lwd = 3, 
         main=paste("AUC:",round(auc$auc,3)))
sapply(2:5,function(i) lines.roc(rs[[i]], col=i, lwd = 3))
#sapply(2:5,function(x) plot(ci.thresholds(rs[[x]], col = x, alpha = 0.3),add=TRUE))
legend("bottomright",legend = c("Bicycle","Bus","Motor Vehicle","Sky Train","Walk"), col = 1:10, lwd = rep(3,10))


sens.ci <- ci.se(auc, specificities=seq(0, 100, 5))
plot(sens.ci, type="shape", col="lightblue")
plot(sens.ci, type="bars")

# need to re-add roc2 over the shape
plot(roc2, add=TRUE)

# CI of thresholds
plot(ci.thresholds(roc2))

#Visualize confusion matrices
conf_mat3[[3]]