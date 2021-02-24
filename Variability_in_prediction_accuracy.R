library(tidyverse)
library(ggplot2)
library(ggbeeswarm)
#set the working directory from which the files will be read from
setwd("C:/Users/aroy29/Dropbox (ASU)/INTERACT Travel Mode Detection/Results/model outputs/")
#create a list of the files from your target directory
file_list <- list.files(path=".")
file_list
name <- NULL
acc <- NULL



for (i in 1:length(file_list)){
  load(file_list[i])
  name[i] = file_list[i]
  acc[i] = mean(model$results$Accuracy)
}

df = data.frame(model_name = name, mean_accuracy = acc)
df$classifier = substr(df$model_name,1,3)
begin_str = 19
end_str = nchar(as.character(df$model_name))-18
df$feature_set = substr(df$model_name,begin_str,end_str)
print(df)
head(df)

write.csv(df,"../Model_Accuracy_with_different_models_feature_sets.csv")

#Clean the file in excel & read it again for plotting
plot_df <- read.csv("../Model_Accuracy_with_different_models_feature_sets_cleaned.csv")
head(plot_df)
clf_name <- function(x)
{
  if(x=='mln'){name = "Multinomial Logit"}
  else if(x=='rf_'){name = "Random Forest"}
  else if(x=='svm'){name = "Support Vector Machine"}
  else{name = "Gradient Boosting"}
  return(name)
}
plot_df$clf_name = sapply(plot_df$classifier,FUN=clf_name)
head(plot_df)

#Boxplot
ggplot(plot_df) + geom_boxplot(aes(x=clf_name,y = mean_accuracy, fill=feature_set))+xlab("Classifier")+
  ylab("Prediction Accuracy (%)") + theme_bw() + ylim(c(0.89,1.00))

ggplot(plot_df) + geom_boxplot(aes(x=feature_set,y = mean_accuracy))+xlab("Feature Sets")+
  ylab("Prediction Accuracy (%)") + theme_bw() + ylim(c(0.89,1.00))

#Beeswarm
ggplot(plot_df, aes(x=clf_name,y=mean_accuracy, col=feature_set)) + geom_beeswarm(priority='density',cex=2.5, size = 2) +
  xlab("Classifier") + ylab ("Prediction Accuracy (%)") + theme_bw()

ggplot(plot_df, aes(x=feature_set,y=mean_accuracy,col=clf_name)) + geom_beeswarm(priority='density',cex=2.5, size = 2) +
  xlab("Classifier") + ylab ("Prediction Accuracy (%)") + theme_bw()

# The beeswarm plot show Gradient Boosting and Random Forest performs best for all feature set combinations
rf_plot_data = subset(plot_df,classifier=='rf_')
head(rf_plot_data)
ggplot(rf_plot_data) + geom_boxplot(aes(x=feature_set,y = mean_accuracy))+
  xlab("Feature Sets")+ ylab("Prediction Accuracy (%)") + theme_bw() + ylim(c(0.89,1.00))

#Random Forest only
xgb_plot_data = subset(plot_df,classifier=='xgb')
head(xgb_plot_data)
ggplot(xgb_plot_data,aes(x=feature_set,y = mean_accuracy)) + geom_beeswarm(priority='density',cex=2.5, size = 2)+
  xlab("Feature Sets")+ ylab("Prediction Accuracy (%)") + theme_bw() + ylim(c(0.89,1.00))


