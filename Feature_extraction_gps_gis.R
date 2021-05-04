setwd("C:/Users/aroy29/Dropbox (ASU)/INTERACT Travel Mode Detection/Avipsa_Analysis/Travel-Mode-Detection/Data")
library(tidyverse)
library(lubridate)
library(activityCounts)
library(magrittr)
library(zoo)
library(e1071)
library(entropy)
library(progress)
library(caret)

#Read GIS features calculated in arcgis and read the new feature file
merged_df<- read.csv("GPS_GIS_attributes_all_cities.csv")
#Remove all NoTrip data
merged_df <- merged_df[merged_df$mode!='NoTrip',]
merged_df$date <- ymd_hms(merged_df$date)
merged_df$mode <- factor(merged_df$mode,levels = c("Active","Public","Private"))
str(merged_df)
table(merged_df$mode)

merged_df$hour <- hour(merged_df$date)
merged_df$hour[is.na(merged_df$hour)] <- 0

t <- NULL
n <- length(merged_df$hour)
for(i in 1:n) 
{
  x = merged_df$hour[i]
  if((x>=0 & x<4))
  {
    t[i] = 'Night'
  }
  else if(x>=4 & x<10)
  {
    t[i] = 'Morning Peak'
  }
  else if(x>=10 & x<17)
  {
    t[i] = 'Afternoon'
  }
  else if(x>=17 & x<22)
  {
    t[i] = 'Evening Peak'
  }
  else if(x>=22 & x<23)
  {
    t[i] = 'Night'
  }
}


merged_df$timeOfDay <- t


s<-NULL
for(i in 1:n)
{
  if(merged_df$city[i] == 'St. Johns')
  {
    if(month(merged_df$date[i]) %in% c(7,8))
      s[i] <- 'Summer'
    else if(month(merged_df$date[i]) %in% c(12,1,2,3))
      s[i] <- 'Winter'
    else if(month(merged_df$date[i]) %in% c(4,5,6))
      s[i] <- 'Spring'
    else
      s[i] <- 'Fall'
  }
  if(merged_df$city[i] == 'Vancouver')
  {
    if(month(merged_df$date[i]) %in% c(7,8,9))
      s[i] <- 'Summer'
    else if(month(merged_df$date[i]) %in% c(1,2,3))
      s[i] <- 'Winter'
    else if(month(merged_df$date[i]) %in% c(4,5,6))
      s[i] <- 'Spring'
    else
      s[i] <- 'Fall'
  }
  if(merged_df$city[i] == 'Montreal')
  {
    if(month(merged_df$date[i]) %in% c(6,7,8))
      s[i] <- 'Summer'
    else if(month(merged_df$date[i]) %in% c(12,1,2))
      s[i] <- 'Winter'
    else if(month(merged_df$date[i]) %in% c(3,4,5))
      s[i] <- 'Spring'
    else
      s[i] <- 'Fall'
  }
}

merged_df$season <- s
head(merged_df)

write.csv(merged_df,"../Results/GPS_GIS_input_features_SJ_VC_MT.csv")

'''
#Subset dataframes by mode to produce calculated features
active_df <- subset(merged_df,mode=='Active')
public_df <- subset(merged_df,mode=='Private')
private_df <- subset(merged_df,mode=='Public')


#Create a function to calculate features based on a time window from merged data

GenerateFeatures <- function(raw_df,window_size)
{
  # create empty dataframe to store all the new features
  new_features <- NULL
  
  #Comment showing progress
  pb <- progress_bar$new(
    format = "Creating features [:bar] :current/:total, time elapsed :elapsedfull",
    total = 7, clear = F, width= 70)
  pb$message(" --- Generating New Features ---")
  pb$tick(0)
  #-------------------Sum Features------------------
  sum_features <- raw_df %>%
    zoo::rollapply(
      data = .,
      width =  window_size,
      by = window_size,
      FUN = sum
    ) %>%
    as.data.frame() %>%
    rename("sum_net_disp" = dist ,
           "sum_speed" = speed,
           "sum_height" = height,
           "sum_abs.angle" = abs_ngl,
           "sum_rel.angle" = rel_ngl,
           "sum_dist_bus" = dist_bus, 
           "sum_dist_comm" = dist_comm, 
           "sum_dist_green" = dist_green,
           "sum_dist_res" = dist_res, 
           "sum_dist_shore" = dist_shore,
           "sum_dist_sub" = dist_sub,
           "sum_dist_bike" = dist_bike
    )
  pb$tick()
  pb$message("Sum features are created")
  #-------------------------Mean------------------------
  
  mean_features <- raw_df %>%
    zoo::rollapply(
      data = .,
      width = window_size ,
      by = window_size,
      FUN = mean
    ) %>%
    as.data.frame() %>%
    rename("mean_net_disp" = dist ,
           "mean_speed" = speed,
           "mean_height" = height,
           "mean_abs.angle" = abs_ngl,
           "mean_rel.angle" = rel_ngl,
           "mean_dist_bus" = dist_bus, 
           "mean_dist_comm" = dist_comm, 
           "mean_dist_green" = dist_green,
           "mean_dist_res" = dist_res, 
           "mean_dist_shore" = dist_shore,
           "mean_dist_sub" = dist_sub,
           "mean_dist_bike" = dist_bike
    )
  pb$tick()
  pb$message("Mean features are created")
  #-----------------Standard Deviation---------------------
  
  sd_features <- raw_df %>%
    zoo::rollapply(
      data = .,
      width = window_size ,
      by = window_size,
      FUN = sd
    ) %>%
    as.data.frame() %>%
    rename("sd_net_disp" = dist ,
           "sd_speed" = speed,
           "sd_height" = height,
           "sd_abs.angle" = abs_ngl,
           "sd_rel.angle" = rel_ngl,
           "sd_dist_bus" = dist_bus, 
           "sd_dist_comm" = dist_comm, 
           "sd_dist_green" = dist_green,
           "sd_dist_res" = dist_res, 
           "sd_dist_shore" = dist_shore,
           "sd_dist_sub" = dist_sub,
           "sd_dist_bike" = dist_bike)
  pb$tick()
  pb$message("SD features are created")
  #--------------Coefficient of Variation------------
  cv_features <- sd_features / mean_features 
  cv_features %<>%    rename("cv_net_disp" = sd_net_disp ,
                             "cv_speed" = sd_speed,
                             "cv_height" = sd_height,
                             "cv_abs.angle" = sd_abs.angle,
                             "cv_rel.angle" = sd_rel.angle,
                             "cv_dist_bus" = sd_dist_bus, 
                             "cv_dist_comm" = sd_dist_comm, 
                             "cv_dist_green" = sd_dist_green,
                             "cv_dist_res" = sd_dist_res, 
                             "cv_dist_shore" = sd_dist_shore,
                             "cv_dist_sub" = sd_dist_sub,
                             "cv_dist_bike" = sd_dist_bike)
  pb$tick()
  pb$message("CV features are created")
  #-----------------Inter Quartile Range---------------------
  iqr_features <- raw_df %>%
  zoo::rollapply(
    data = .,
    width = window_size ,
    by = window_size,
    FUN = IQR,
    na.rm = TRUE
  ) %>%
  as.data.frame() %>%
  rename("iqr_net_disp" = dist ,
         "iqr_speed" = speed,
         "iqr_height" = height,
         "iqr_abs.angle" = abs_ngl,
         "iqr_rel.angle" = rel_ngl,
         "iqr_dist_bus" = dist_bus, 
         "iqr_dist_comm" = dist_comm, 
         "iqr_dist_green" = dist_green,
         "iqr_dist_res" = dist_res, 
         "iqr_dist_shore" = dist_shore,
         "iqr_dist_sub" = dist_sub,
         "iqr_dist_bike" = dist_bike)
  pb$tick()
  pb$message("IQR features are created")
  #---------------Skewness--------------------

  skw_features <- raw_df %>%
    zoo::rollapply(
      data = .,
      width = window_size ,
      by = window_size,
      FUN = function(x)
        e1071::skewness(x, na.rm = T)
    ) %>%
    as.data.frame() %>%
    rename("skw_net_disp" = dist ,
           "skw_speed" = speed,
           "skw_height" = height,
           "skw_abs.angle" = abs_ngl,
           "skw_rel.angle" = rel_ngl,
           "skw_dist_bus" = dist_bus, 
           "skw_dist_comm" = dist_comm, 
           "skw_dist_green" = dist_green,
           "skw_dist_res" = dist_res, 
           "skw_dist_shore" = dist_shore,
           "skw_dist_sub" = dist_sub,
           "skw_dist_bike" = dist_bike)
  pb$tick()
  pb$message("Skw features are created")
  #----------------Kurtosis----------------------

# degree of the peakedness of the signal probability distribution
  krt_features <- raw_df %>%
    zoo::rollapply(
      data = .,
      width = window_size ,
      by = window_size,
      FUN = function(x)
        e1071::kurtosis(x, na.rm = T)
    ) %>%
    as.data.frame() %>%
    rename("krt_net_disp" = dist ,
           "krt_speed" = speed,
           "krt_height" = height,
           "krt_abs.angle" = abs_ngl,
           "krt_rel.angle" = rel_ngl,
           "krt_dist_bus" = dist_bus, 
           "krt_dist_comm" = dist_comm, 
           "krt_dist_green" = dist_green,
           "krt_dist_res" = dist_res, 
           "krt_dist_shore" = dist_shore,
           "krt_dist_sub" = dist_sub,
           "krt_dist_bike" = dist_bike)
  pb$tick()
  pb$message("Krt features are created")
  
  #Combine all the calculated ffeatures into a single data frame
  new_features <- cbind(sum_features,mean_features,sd_features,
                      cv_features,iqr_features,skw_features,krt_features)
  return(new_features)
}

#Vary window size to check accuracy with varying model parameters

build_feature_matrix <- function(active_df,private_df,public_df)
{
  window_size = c(1,3,5)
  
  for(i in 1:length(window_size))
  {
      active_features <- GenerateFeatures(active_df,window_size[i])
      active_features$mode <- "Active"
      private_features <- GenerateFeatures(private_df,window_size[i])
      private_features$mode <- "Private"
      public_features <- GenerateFeatures(public_df,window_size[i])
      public_features$mode <- "Public"
      
      new_features <- rbind(active_features,private_features,public_features)
    
    
    #### need to find a way to associate mode labels with each roll up time frame
    filename1 = paste("Calculated_features_window_size",window_size[i],".csv",sep="")
    write.csv(new_features,filename1)
    gps_features <- new_features[,c("sum_height","sum_abs.angle","sum_rel.angle","sum_speed","sum_net_disp",
                                    "mean_height","mean_abs.angle","mean_rel.angle","mean_speed","mean_net_disp",
                                    "sd_height","sd_abs.angle","sd_rel.angle","sd_speed","sd_net_disp",
                                    "cv_height","cv_abs.angle","cv_rel.angle","cv_speed","iqr_net_disp",
                                    "skw_height","skw_abs.angle","skw_rel.angle","skw_speed","skw_net_disp",
                                    "krt_height","krt_abs.angle","krt_rel.angle","krt_speed","krt_net_disp","mode")]
    gis_features <- new_features[,c("sum_dist_bus","sum_dist_comm","sum_dist_green","sum_dist_res","sum_dist_bike",
                                    "sum_dist_shore","sum_dist_sub","mean_dist_bus","mean_dist_comm","mean_dist_bike",
                                    "mean_dist_green","mean_dist_res","mean_dist_shore","mean_dist_sub",
                                    "sd_dist_bus","sd_dist_comm","sd_dist_bike","sd_dist_green","sd_dist_res",
                                    "sd_dist_shore","sd_dist_sub","cv_dist_bus","cv_dist_comm","cv_dist_bike",
                                    "cv_dist_green","cv_dist_res","cv_dist_shore","cv_dist_sub",
                                    "iqr_dist_bus","iqr_dist_comm","iqr_dist_green","iqr_dist_res","iqr_dist_bike",
                                    "iqr_dist_shore","iqr_dist_sub","skw_dist_bus","skw_dist_comm",
                                    "skw_dist_green","skw_dist_res","skw_dist_shore","skw_dist_sub","skw_dist_bike",
                                    "krt_dist_bus","krt_dist_comm","krt_dist_green","krt_dist_res",
                                    "krt_dist_shore","krt_dist_sub","krt_dist_bike","mode")]
    
    filename2 = paste("GPS_calculated_features_window_size_",window_size[i],"_March2_2021.csv",sep="")
    filename3 = paste("GIS_calculated_features_window_size_",window_size[i],"_March2_2021.csv",sep="")
    
    write.csv(gps_features,filename2)
    write.csv(gis_features,filename3)
  print("All feature matrices written to file!")
  }
  
}

#Select columns from which to calculate features 
activeFeatures <- active_df[,-c(1:3,9,17)]
activeFeatures[is.na(activeFeatures)] <- 0
privateFeatures <- private_df[,-c(1:3,9,17)]
privateFeatures[is.na(privateFeatures)] <- 0
publicFeatures <- public_df[,-c(1:3,9,17)]
publicFeatures[is.na(publicFeatures)] <- 0

build_feature_matrix(activeFeatures,privateFeatures,publicFeatures)
'''

#Build input feature set from the merged dataframe
inputFeatures <- merged_df[,-c(1,2,3,9,18)]
inputFeatures$timeOfDay <- as.factor(inputFeatures$timeOfDay)
inputFeatures$season <- as.factor(inputFeatures$season)
str(inputFeatures)

write.csv(inputFeatures,"../Results/Input Feature Set All Cities.csv")
#Normalize numeric variables from feature set using min-max equalizer
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

normalizedFeatures <- as.data.frame(lapply(inputFeatures[1:12], min_max_norm))
normalizedFeatures$timeOfDay <- inputFeatures$timeOfDay
normalizedFeatures$season <- inputFeatures$season
normalizedFeatures$city <- as.factor(inputFeatures$city)
normalizedFeatures$mode <- merged_df$mode

str(normalizedFeatures)

#Write the normalized input feature set to file
write.csv(normalizedFeatures, "Normalized_Input_FeatureSet_Mar03_2021.csv ")


data = read.csv("Normalized_Input_FeatureSet_Mar03_2021.csv")

#Create Train-Test split
x = createDataPartition(data$mode,p=0.7, list = FALSE, times = 1) #Create a 70:30 split
train_data <- data[x,]
test_data <- data[-x,]

write.csv(train_data,"GPS_GIS_TrainingData_Mar03_2021.csv")
write.csv(test_data,"GPS_GIS_TestData_Mar03_2021.csv")

#Separate GPS and GIS features
data_gps <- data[,c(2:6,14:17)]
data_gis <- data[,c(7:17)]

#Create Train-Test split for only GPS features
gps_x = createDataPartition(data_gps$mode,p=0.7, list = FALSE, times = 1) #Create a 70:30 split
gps_train_data <- data_gps[gps_x,]
gps_test_data <- data_gps[-gps_x,]

write.csv(gps_train_data,"GPS_TrainingData_Mar03_2021.csv")
write.csv(gps_test_data,"GPS_TestData_Mar03_2021.csv")

#Create Train-Test split for only GIS features
gis_x = createDataPartition(data_gis$mode,p=0.7, list = FALSE, times = 1) #Create a 70:30 split
gis_train_data <- data_gis[gis_x,]
gis_test_data <- data_gis[-gis_x,]

write.csv(gis_train_data,"GIS_TrainingData_Mar03_2021.csv")
write.csv(gis_test_data,"GIS_TestData_Mar03_2021.csv")
