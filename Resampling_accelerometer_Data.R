# Using functions from activityCounts package to perform resampling
library(magrittr)
library(seewave)
library(lubridate)
library(signal)
library(tibble)

trunc = function(data, min_value){
  # @param data : The input variable which will be altered if less than the threshold
  # @param min_value :  the threshold which the input below it will be set to zero
  # @return : returns zero if the "data" is less than the "mean_value" otherwise returns the "data"
  return(ifelse(data < min_value, 0, data))
}


runsum = function(data, len, threshold){
  # @param data : input data
  # @param len : the length
  # @param threshold : the threshold
  # @return : returns a

  N = length(data)
  cnt = ceiling(N/len)
  rs = rep(0, cnt)
  
  for(n in 1:cnt){
    for(p in (1+len*(n-1)):(len*n)){
      if(p < N & data[p] >= threshold){
        rs[n] = rs[n] + data[p] - threshold
        #
      }
    }
  }
  return(rs)
}

pptrunc = function(data, max_value){
  # @param data The variable that will be truncated
  # @param max_value The upper bound ( -max_value is the lower bound)
  # @return the highest(or the lowest) value of "data" and "max_value"
  outd = ifelse(data > max_value, max_value, data)
  return(ifelse(outd < - max_value, - max_value, outd))
}

counts = function(data,hertz = -1,x_axis = 2,y_axis = 3,z_axis = 4,
                  time_column = -1,start_time = -1) 
  {
  # Calculates ActiLife counts based on raw accelerometer data

  # @param data : Accelerometer data, Must have at least three columns.
  # @param hertz : Sampling frequency in Hz
  # @param x_axis : Indicates the column number which has the accel data for x direction, the default is 2
  # @param y_axis : Indicates the column number which has the accel data for y direction, the default is 3
  # @param z_axis : Indicates the column number which has the accel data for z direction, the default is 4
  # @param time_column : Optional. Indicates the column number which has the date and time.
  
  #  The first row will be considered as the start time of the study. You can use the
  #  "start_time" argument to provide the start time explicitly.
  # @param start_time : Optional. Use this to define the start time of the experiment.
  #  You can use this argument if the data does not contain a time column.
  
  if (hertz == -1) {
    warning("Sampling frequency is not assigned! (default value is 30Hz)")
    hertz = 30
  }
  
  if (time_column != -1) {
    start_time = floor_date(x = data[1, time_column], unit = "seconds")
  }
  else if (start_time == -1) {
    start_time = floor_date(x = now(), unit = "seconds")
    warning("Start date is not specified! (current time is considered as the start time)")
  }
  
  start_time = start_time %>%
    as_datetime()
  
  data = data[, c("AccelX", "AccelY", "AccelZ")]
  
  A = c(
    1,
    -4.1637,
    7.5712,
    -7.9805,
    5.385,
    -2.4636,
    0.89238,
    0.06361,
    -1.3481,
    2.4734,
    -2.9257,
    2.9298,
    -2.7816,
    2.4777,
    -1.6847,
    0.46483,
    0.46565,
    -0.67312,
    0.4162,
    -0.13832,
    0.019852
  )
  
  B = c(
    0.049109,
    -0.12284,
    0.14356,
    -0.11269,
    0.053804,
    -0.02023,
    0.0063778,
    0.018513,
    -0.038154,
    0.048727,
    -0.052577,
    0.047847,
    -0.046015,
    0.036283,
    -0.012977,
    -0.0046262,
    0.012835,
    -0.0093762,
    0.0034485,
    -0.00080972,
    -0.00019623
  )
  
  deadband = 0.068
  sf = 30
  peakThreshold = 2.13
  adcResolution = 0.0164
  integN = 10
  gain = 0.965
  out = NULL
  
  for (i in 1:3) {
    if (hertz > sf) {
      n = nrow(data)
      r = hertz/sf
      datares = data[seq(1, n, by = r), i]
      datab = bwfilter(
        datares,
        f = sf,
        n = 4,
        from = 0.01,
        to = 7,
        bandpass = TRUE
      )
    }
    else{
      AB = butter(4, c(0.01, 7) / (sf / 2))
      datab = filtfilt(AB$b, AB$a, data[, i])
    }
    
    B = B * gain
    fx8up = filter(B, A, datab)
    fx8 = pptrunc(fx8up[seq(1, length(fx8up), 3)], peakThreshold)
    out = cbind(out, runsum(floor(trunc(
      abs(fx8), deadband
    ) / adcResolution), integN, 0))
    
  }
  colnames(out) = c("count_x", "count_y", "count_z")
  
  out_length = nrow(out)
  out = out %>%
    as.data.frame() %>%
    add_column(
      Time = seq(
        from = start_time,
        length.out = out_length,
        by = "sec"
      ) ,
      .before = "count_x"
    )
  
  return(out)
}

data = read.csv("./Data/accel_gps_mode_merged.csv")
data$RecordTime = ymd_hms(data$RecordTime)
na_removed_data = na.omit(data[,c("RecordTime","AccelX","AccelY","AccelZ","User","Mode")])
str(data)
str(na_removed_data) #1391 records are removed ~ 0.12%

user2837 = counts(na_removed_data[na_removed_data$User==2837,],time_column = 1,hertz = 100)
head(user2837)
user2839 = counts(na_removed_data[na_removed_data$User==2839,],time_column = 1,hertz = 50)
user2836 = counts(na_removed_data[na_removed_data$User==2836,],time_column = 1,hertz = 50)
user3173 = counts(na_removed_data[na_removed_data$User==3173,],time_column = 1,hertz = 50)
user3191 = counts(na_removed_data[na_removed_data$User==3191,],time_column = 1,hertz = 50)
user3192 = counts(na_removed_data[na_removed_data$User==3192,],time_column = 1,hertz = 50)
user3200 = counts(na_removed_data[na_removed_data$User==3200,],time_column = 1,hertz = 50)
user3201 = counts(na_removed_data[na_removed_data$User==3201,],time_column = 1,hertz = 50)
user3207 = counts(na_removed_data[na_removed_data$User==3207,],time_column = 1,hertz = 50)
user3211 = counts(na_removed_data[na_removed_data$User==3211,],time_column = 1,hertz = 50)
user3213 = counts(na_removed_data[na_removed_data$User==3213,],time_column = 1,hertz = 50)
user3244 = counts(na_removed_data[na_removed_data$User==3244,],time_column = 1,hertz = 50)

na_removed_data$tid = c(1:nrow(na_removed_data))
head(na_removed_data)
