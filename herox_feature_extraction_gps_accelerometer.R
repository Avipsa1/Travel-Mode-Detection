library(tidyverse)
library(lubridate)
library(activityCounts)
library(magrittr)
library(zoo)
library(e1071)
library(entropy)
library(progress)

#Read GIS features calculated in arcgis and read the new feature file
merged_df<- read.csv("GPS_GIS_Acc_attributes_raw.csv")
str(merged_df)
table(merged_df$Mode)
#Remove all NoTrip data
merged_df <- merged_df[merged_df$Mode!='NoTrip',]
merged_df$RecrdTm <- ymd_hms(merged_df$RecrdTm)
merged_df$year <- year(merged_df$RecrdTm)
merged_df$month <- month(merged_df$RecrdTm)
merged_df$Mode <- factor(merged_df$Mode,levels = c("Bus","Bicycle","Walk","Sky Train","Motor Vehicle"))
str(merged_df)
table(merged_df$Mode)
table(merged_df$month)

#Calculate features from acceleration, GPS and GIS
features_df = merged_df[,c("Acc_mag", "dist_bus", "dist_comm", "dist_green", "dist_res", 
                          "dist_shore","dist_sub", "height", "rel_ngl", "speed", 
                          "nt_dsp_","Mode")]


colnames(features_df) <- c("Acc_mag", "dist_bus", "dist_comm", "dist_green", "dist_res", 
                           "dist_shore","dist_sub", "height", "rel.angle", "speed", 
                           "net_disp","Mode")
features_df[is.na(features_df)] <- 0
table(features_df$Mode)

#Subset dataframes by mode to produce calculated features
#notrip_df <- subset(features_df,Mode=='NoTrip')[,-c(12)]
walk_df <- subset(merged_df,Mode=='Walk')[,-c(12)]
motor_df <- subset(merged_df,Mode=='Motor Vehicle')[,-c(12)]
bus_df <- subset(merged_df,Mode=='Bus')[,-c(12)]
bike_df <- subset(merged_df,Mode=='Bicycle')[,-c(12)]
skytrain_df <- subset(merged_df,Mode=='Sky Train')[,-c(12)]


#Calculate trip time by mode
walk_trip_times = mean(aggregate(dt~User,walk_df,mean)$dt)/60
bike_trip_times = mean(aggregate(dt~User,bike_df,sum)$dt)/60
motor_trip_times = mean(aggregate(dt~User,motor_df,sum)$dt)/60
bus_trip_times = mean(aggregate(dt~User,bus_df,sum)$dt)/60
skytrain_trip_times = mean(aggregate(dt~User,skytrain_df,sum)$dt)/60
avg_trip_times = aggregate(dt~User+Mode,merged_df,sum)
duration_by_modes = aggregate(dt~Mode,avg_trip_times,mean)

plot_df = data.frame(Mode = c('Bicycle', 'Bus', 'Motor Vehicle', 'Sky Train', 'Walk'), 
                     Trips = c(274,118,321,73,964),
                     Duration = c('1.54 mins','1.45 mins','1.09 mins','1.33 mins','5.23 mins'))
ggplot(plot_df, aes(x = Mode, y = Trips,fill = Duration)) + 
  geom_bar(stat = 'identity') + 
  My_Theme(24) + xlab("Transportation Mode") + 
  ylab("Number of Trips") + scale_fill_brewer(palette = "Set1") +
  geom_text(aes(label=Duration),position=position_dodge(width=0.9), 
            vjust=-0.25) + labs(fill = "Avg. Trip Duration")

#Using trip detection algorithm calulate trip times by mode
trips = read.csv("./Data/Filtered_trips_with_GPS_Acc_GIS_features.csv")
trips = trips[trips$Mode!='NoTrip',]
trips$start_time = ymd_hms(trips$start_time)
trips$stop_time = ymd_hms(trips$stop_time)
trips$trip_length = trips$stop_time - trips$start_time
avg_trip_times = aggregate(trip_length~Mode,trips,mean)
avg_trip_times

plot_df = data.frame(Mode = c('Bicycle', 'Bus', 'Motor Vehicle', 'Sky Train', 'Walk'), 
                     Trips = c(274,118,321,73,964),
                     Duration = c('37.9 mins','23.7 mins','22.8 mins','21.8 mins','14.4 mins'))
ggplot(plot_df, aes(x = Mode, y = Trips,fill = Duration)) + 
  geom_bar(stat = 'identity') + 
  My_Theme(24) + xlab("Transportation Mode") + 
  ylab("Number of Trips") + scale_fill_brewer(palette = "Set1") +
  geom_text(aes(label=Duration),position=position_dodge(width=0.9), 
            vjust=-0.25) + labs(fill = "Avg. Trip Duration")


modes <- c("Walk","Motor Vehicle","Bus","Bicycle","Sky Train")
counts <- c(nrow(walk_df),nrow(motor_df),
            nrow(bus_df),nrow(bike_df),nrow(skytrain_df))
data.frame(modes,counts)
#Create a function to calculate features based on a time window from merged data

GenerateFeatures <- function(raw_df,window_size)
{
  # create empty dataframe to store all the new features
  new_features <- NULL
  
  #Comment showing progress
  pb <- progress_bar$new(
    format = "Creating features [:bar] :current/:total, time elapsed :elapsedfull",
    total = 18, clear = F, width= 70)
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
    rename("sum_Acc" = Acc_mag,
           "sum_net_disp" = net_disp ,
           "sum_speed" = speed,
           "sum_height" = height,
           "sum_rel.angle" = rel.angle,
           "sum_dist_bus" = dist_bus, 
           "sum_dist_comm" = dist_comm, 
           "sum_dist_green" = dist_green,
           "sum_dist_res" = dist_res, 
           "sum_dist_shore" = dist_shore,
           "sum_dist_sub" = dist_sub
    )
  pb$tick()
  pb$message("Sum features are created")
  #------------------Signal Power------------------
  snp_features <- raw_df %>%
    select(Acc_mag) %>%
    zoo::rollapply(
      data = .,
      width = window_size ,
      by = window_size,
      FUN = function(x)
        sum(x ^ 2)
      
    ) %>%
    as.data.frame() %>%
    rename("snp_Acc" = Acc_mag)
  pb$tick()
  pb$message("Snp features are created")
  #-------------------------Mean------------------------
  
  mean_features <- raw_df %>%
    zoo::rollapply(
      data = .,
      width = window_size ,
      by = window_size,
      FUN = mean
    ) %>%
    as.data.frame() %>%
    rename("mean_Acc" = Acc_mag,
           "mean_net_disp" = net_disp ,
           "mean_speed" = speed,
           "mean_height" = height,
           "mean_rel.angle" = rel.angle,
           "mean_dist_bus" = dist_bus, 
           "mean_dist_comm" = dist_comm, 
           "mean_dist_green" = dist_green,
           "mean_dist_res" = dist_res, 
           "mean_dist_shore" = dist_shore,
           "mean_dist_sub" = dist_sub
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
    rename("sd_Acc" = Acc_mag,
           "sd_net_disp" = net_disp ,
           "sd_speed" = speed,
           "sd_height" = height,
           "sd_rel.angle" = rel.angle,
           "sd_dist_bus" = dist_bus, 
           "sd_dist_comm" = dist_comm, 
           "sd_dist_green" = dist_green,
           "sd_dist_res" = dist_res, 
           "sd_dist_shore" = dist_shore,
           "sd_dist_sub" = dist_sub)
  pb$tick()
  pb$message("SD features are created")
  #--------------Coefficient of Variation------------
  cv_features <- sd_features / mean_features 
  cv_features %<>%    rename("cv_Acc" = sd_Acc,
                             "cv_net_disp" = sd_net_disp ,
                             "cv_speed" = sd_speed,
                             "cv_height" = sd_height,
                             "cv_rel.angle" = sd_rel.angle,
                             "cv_dist_bus" = sd_dist_bus, 
                             "cv_dist_comm" = sd_dist_comm, 
                             "cv_dist_green" = sd_dist_green,
                             "cv_dist_res" = sd_dist_res, 
                             "cv_dist_shore" = sd_dist_shore,
                             "cv_dist_sub" = sd_dist_sub)
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
  rename("iqr_Acc" = Acc_mag,
         "iqr_net_disp" = net_disp ,
         "iqr_speed" = speed,
         "iqr_height" = height,
         "iqr_rel.angle" = rel.angle,
         "iqr_dist_bus" = dist_bus, 
         "iqr_dist_comm" = dist_comm, 
         "iqr_dist_green" = dist_green,
         "iqr_dist_res" = dist_res, 
         "iqr_dist_shore" = dist_shore,
         "iqr_dist_sub" = dist_sub)
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
    rename("skw_Acc" = Acc_mag,
           "skw_net_disp" = net_disp ,
           "skw_speed" = speed,
           "skw_height" = height,
           "skw_rel.angle" = rel.angle,
           "skw_dist_bus" = dist_bus, 
           "skw_dist_comm" = dist_comm, 
           "skw_dist_green" = dist_green,
           "skw_dist_res" = dist_res, 
           "skw_dist_shore" = dist_shore,
           "skw_dist_sub" = dist_sub)
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
    rename("krt_Acc" = Acc_mag,
           "krt_net_disp" = net_disp ,
           "krt_speed" = speed,
           "krt_height" = height,
           "krt_rel.angle" = rel.angle,
           "krt_dist_bus" = dist_bus, 
           "krt_dist_comm" = dist_comm, 
           "krt_dist_green" = dist_green,
           "krt_dist_res" = dist_res, 
           "krt_dist_shore" = dist_shore,
           "krt_dist_sub" = dist_sub)
  pb$tick()
  pb$message("Krt features are created")
  #------------Sum Log Energy---------------------
  sle_features <- raw_df %>%
    select(Acc_mag) %>%
    zoo::rollapply(
      data = .,
      width = window_size ,
      by = window_size,
      FUN = function(x)
        sum(log(x ^ 2 + 1))
    ) %>%
    as.data.frame() %>%
    rename("sle_Acc" = Acc_mag)
  pb$tick()
  pb$message("Sle features are created")
  #-------------------Peak Intensity-----------------
  pin_features <- raw_df %>%
    select(Acc_mag) %>%
    zoo::rollapply(
      data = .,
      width = window_size ,
      by = window_size,
      FUN = function(x) {
        x <- round(x = x, digits = 3)
        max_of_window <- max(x)
        which(x  == max_of_window) %>%
          length() %>%
          return()
      }
    ) %>%
    as.data.frame() %>%
    rename("peak_Acc" = Acc_mag)
  pb$tick()
  pb$message("Peak features are created")
  #---------------Dominant Frequency-------------
    dfr_features <- raw_df %>%
    select(Acc_mag) %>%
    zoo::rollapply(
      data = .,
      width = window_size ,
      by = window_size,
      FUN = function(x) {
        FT <- fft(x)
        return(max(Re(FT ^ 2)))
      }
    ) %>%
    as.data.frame() %>%
    rename("dfr_Acc"= Acc_mag)
  pb$tick()
  pb$message("Dfr features are created")
  #----------Amplitude of Dominant Frequency---------
  adf_features <- raw_df %>%
    select(Acc_mag) %>%
    zoo::rollapply(
      data = .,
      width = window_size ,
      by = window_size,
      FUN = function(x) {
        FT <- fft(x)
        idx <- which.max(Re(FT ^ 2))
        return(Re(FT[idx]))
      }
    ) %>%
    as.data.frame() %>%
    rename("adf_Acc" = Acc_mag)
  pb$tick()
  pb$message("Adf features are created")
  #-------------Entropy------------
  ent_features <- raw_df %>%
    select(Acc_mag,speed) %>%
    zoo::rollapply(
      data = .,
      width = window_size ,
      by = window_size,
      FUN = function(x) {
        probabilities <- prop.table(table(x))
        return(-sum(probabilities*log2(probabilities)))
        # Note:
        # Using the library entropy does not give us the same results
        # return(entropy::entropy.empirical(x, unit = "log2"))
        
      }
    ) %>%
    as.data.frame() %>%
    rename("ent_Acc" = Acc_mag,
           "ent_speed" = speed)
  pb$tick()
  pb$message("Entropy features are created")
  #Combine all the calculated ffeatures into a single data frame
  new_features <- cbind(sum_features,snp_features,mean_features,sd_features,
                      cv_features,iqr_features,skw_features,krt_features,
                      sle_features,pin_features,dfr_features,adf_features,
                      ent_features)
  return(new_features)
}

#Vary window size to check accuracy with varying model parameters

build_feature_matrix <- function(walk_df,motor_df,bus_df,bike_df,skytrain_df)
{
  window_size = c(3,5,7,10)
  
  for(i in 1:length(window_size))
  {
      walk_features <- GenerateFeatures(walk_df,window_size[i])
      walk_features$Mode <- "Walk"
      motor_features <- GenerateFeatures(motor_df,window_size[i])
      motor_features$Mode <- "Motor Vehicle"
      bus_features <- GenerateFeatures(bus_df,window_size[i])
      bus_features$Mode <- "Bus"
      skytrain_features <- GenerateFeatures(skytrain_df,window_size[i])
      skytrain_features$Mode <- "Sky Train"
      bike_features <- GenerateFeatures(bike_df,window_size[i])
      bike_features$Mode <- "Bicycle"
      new_features <- rbind(walk_features,motor_features,bus_features,
                            bike_features,skytrain_features)
    
    
    #### need to find a way to associate mode labels with each roll up time frame
    filename1 = paste("Calculated_features_window_size",window_size[i],".csv",sep="")
    write.csv(new_features,filename1)
    acc_features <- new_features[,c("sum_Acc","snp_Acc","mean_Acc","sd_Acc","cv_Acc","iqr_Acc",
                                    "skw_Acc","krt_Acc","sle_Acc","peak_Acc","dfr_Acc","adf_Acc",
                                    "ent_Acc","Mode")]
    gps_features <- new_features[,c("sum_height","sum_rel.angle","sum_speed","sum_net_disp",
                                    "mean_height","mean_rel.angle","mean_speed","mean_net_disp",
                                    "sd_height","sd_rel.angle","sd_speed","sd_net_disp",
                                    "cv_height","cv_rel.angle","cv_speed","iqr_net_disp",
                                    "skw_height","skw_rel.angle","skw_speed","skw_net_disp",
                                    "krt_height","krt_rel.angle","krt_speed","krt_net_disp","Mode")]
    gis_features <- new_features[,c("sum_dist_bus","sum_dist_comm","sum_dist_green","sum_dist_res",
                                    "sum_dist_shore","sum_dist_sub","mean_dist_bus","mean_dist_comm",
                                    "mean_dist_green","mean_dist_res","mean_dist_shore","mean_dist_sub",
                                    "sd_dist_bus","sd_dist_comm","sd_dist_green","sd_dist_res",
                                    "sd_dist_shore","sd_dist_sub","cv_dist_bus","cv_dist_comm",
                                    "cv_dist_green","cv_dist_res","cv_dist_shore","cv_dist_sub",
                                    "iqr_dist_bus","iqr_dist_comm","iqr_dist_green","iqr_dist_res",
                                    "iqr_dist_shore","iqr_dist_sub","skw_dist_bus","skw_dist_comm",
                                    "skw_dist_green","skw_dist_res","skw_dist_shore","skw_dist_sub",
                                    "krt_dist_bus","krt_dist_comm","krt_dist_green","krt_dist_res",
                                    "krt_dist_shore","krt_dist_sub","Mode")]
    
    filename2 = paste("Acc_calculated_features_window_size_",window_size[i],"_July28.csv",sep="")
    filename3 = paste("GPS_calculated_features_window_size_",window_size[i],"_July28.csv",sep="")
    filename4 = paste("GIS_calculated_features_window_size_",window_size[i],"_July28.csv",sep="")
    
    write.csv(acc_features,filename2)
    write.csv(gps_features,filename3)
    write.csv(gis_features,filename4)
  print("All feature matrices written to file!")
  }
  
}

build_feature_matrix(walk_df,motor_df,bus_df,bike_df,skytrain_df)


