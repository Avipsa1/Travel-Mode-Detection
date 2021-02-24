library(data.table)
library(lubridate)
library(timeDate)
library(ggplot2)
library(gridExtra)
library(adehabitatLT)
library(lubridate)
library(xts)
library(raster)



setwd("C:/Users/aroy29/Dropbox (ASU)/INTERACT Travel Mode Detection/Data")
#Read the labelled data
data = read.csv("accel_gps_mode_merged.csv")
nrow(data)
data = na.omit(data)
nrow(data)
table(data$Mode)
unique(data$Mode)
data = subset(data, Mode!='NoTrip')
unique(data$User)
data$RecordTime = ymd_hms(data$RecordTime)
#Calculate the scalar magnitude of acceleration
data$vecma = sqrt(data$AccelX^2 +  data$AccelY^2 +  data$AccelZ^2)
nrow(data)

#Calculate trip time for all users
max(data$RecordTime) - min(data$RecordTime)

#Split data by users and count how many are from Vancouver & how many from St. John's
trips = aggregate(vecma~User+Mode,data,length)
trips = trips[order(trips$User),]
sum(trips$vecma)

#Visualize boxplots of acceleration magnitude for various mode types
ggplot(data) + geom_bar(aes(x=Mode))
#Group data by user,mode and record time to visualize
plot_data = aggregate(.~RecordTime+User+Mode, data, mean)
bike = subset(plot_data,Mode=='Bicycle')
bus = subset(plot_data,Mode=='Bus')
motor = subset(plot_data,Mode=='Motor Vehicle')
notrip = subset(plot_data,Mode=='NoTrip')
skytrain = subset(plot_data,Mode=='Sky Train')
subway = subset(plot_data,Mode=='Subway')
walk = subset(plot_data,Mode=='Walk')
plot_data$hour = hour(plot_data)
g1 = ggplot(plot_data) + geom_boxplot(aes(x = Mode, y=vecma))
g2 = ggplot(plot_data) + geom_boxplot(aes(x = Mode, y=AccelX))
g3 = ggplot(plot_data) + geom_boxplot(aes(x = Mode, y=AccelY))
g4 = ggplot(plot_data) + geom_boxplot(aes(x=Mode,y=AccelZ))
grid.arrange(g2,g3,g4,ncol=1)

#Visualize vector magnitude of acceleration for different modes for a single user
dt_bicycle = data[data$Mode=='Bicycle',]
dt_bus = data[data$Mode=='Bus',]
dt_walk = data[data$Mode=='Walk',]
dt_vehicle = data[data$Mode=='Motor Vehicle',]
dt_subway = data[data$Mode=='Subway',]
dt_train = data[data$Mode=='Sky Train',]
par(mar = c(7,5.5,6,3))
plot(dt_bicycle$RecordTime,dt_bicycle$vecma,col="red",lwd=1.2, type = "l", 
     xlab = "Day of week", ylab = "Magnitude of acceleration (m/s2)", 
     cex.axis = 1.8, cex.lab=1.8,font.lab=1.5)
lines(dt_bus$RecordTime,dt_bus$vecma,col="blue",lwd=1.35)
lines(dt_walk$RecordTime,dt_walk$vecma,col="green",lwd=1.35)
lines(dt_vehicle$RecordTime,dt_vehicle$vecma,col="purple",lwd=1.35)
lines(dt_subway$RecordTime,dt_subway$vecma,col="orange",lwd=1.35)
lines(dt_train$RecordTime,dt_train$vecma,col="black",lwd=1.35)
legend("topleft",legend = c("Bike","Bus","Walk","Car","Subway","Sky Train"),
       col = c("red","blue","green","purple","orange","black"),
       lty = c(1,1,1,1,1,1), cex = 1.02)

#Use adeHabitatLT to generate distance, speed and steps from the merged dataset
data <- data[order(data$RecordTime,data$User),]

data_gps_attr <- as.ltraj(xy=data[,c("Lat","Lon")],date=data$RecordTime,
                          id=data$User,burst=data$User,
                          proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
data_gps_attr

tot_mins <- sum(unlist(lapply(data_gps_attr,function(x) sum(x$dt, na.rm=TRUE)/60)))
tot_trips <- sum(summary(data_gps_attr)$nb.reloc)

## Compute speed from the summary statistics of the trajectory of each user

data_gps_attr[[1]]$speed = data_gps_attr[[1]]$dist/data_gps_attr[[1]]$dt
data_gps_attr[[1]]$year = year(data_gps_attr[[1]]$date)
data_gps_attr[[1]]$month = month(data_gps_attr[[1]]$date)
data_gps_attr[[1]]$day = day(data_gps_attr[[1]]$date)
data_gps_attr[[1]]$User = as.character(summary(data_gps_attr[1])$id)
df = data_gps_attr[[1]]
for (i in seq(2:length(data_gps_attr)))
{
  data_gps_attr[[i]]$speed = data_gps_attr[[i]]$dist/data_gps_attr[[i]]$dt
  data_gps_attr[[i]]$year = year(data_gps_attr[[i]]$date)
  data_gps_attr[[i]]$month = month(data_gps_attr[[i]]$date)
  data_gps_attr[[i]]$day = day(data_gps_attr[[i]]$date)
  data_gps_attr[[i]]$User = as.character(summary(data_gps_attr[i])$id)
  df = rbind(df,data_gps_attr[[i]])
}


# Merge the GPS features with acceleration values 

accel_gps_merged <- merge(df,data,by.x=c("date","x","y","User"),by.y=c("RecordTime","Lat","Lon","User"),all.y=TRUE)
nrow(accel_gps_merged)
head(accel_gps_merged)

colnames(accel_gps_merged) <- c('RecordTime','Lat','Lon','User','dx','dy','dist','dt','net_disp_sqrd','abs.angle','rel.angle','speed',
                                'year','month','day','Email','AccelX','AccelY','AccelZ','height','status','Mode','Acc_mag')

head(accel_gps_merged)

nrow(accel_gps_merged)
#Write the final feature set to a csv file for training the model
write.csv(data.frame(accel_gps_merged),"GPS_Acc_feature_set_Mar_03_2020.csv")

#Convert the feature set into shapefile
coordinates(accel_gps_merged) = ~Lon+Lat
projection(accel_gps_merged) = "+init=epsg:4326"
filename = paste("GPS_Acc_mode_merged_03Mar2020",".shp",sep="")
shapefile(accel_gps_merged,filename, overwrite=TRUE)
