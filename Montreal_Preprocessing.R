setwd("C:/Users/aroy29/Dropbox (ASU)/INTERACT Travel Mode Detection/Data/Montreal")
library(sf)
library(rgdal)
library(rgeos)
library(adehabitatLT)
library(lubridate)
library(raster)

montreal_traj = st_read("./trajets_mtl_trajet_2017/trajets_mtl_trajet_2017.shp")
plot(montreal_traj)
head(montreal_traj)

montreal_pts = st_read("./points_mtl_trajet_2017/points_mtl_trajet_2017.shp")
#plot(montreal_pts)
head(montreal_pts)

#Extract GIS features
# dist_bus	dist_comm	dist_green	dist_res	dist_shore	dist_subway

#Extract GPS features
#height	rel.angle	speed	net_disp
montreal_pts$timestamp <- ymd_hms(as.character(montreal_pts$timestamp))
save(montreal_pts, file = "Montreal-Trajectory-points-sf-dataframe.Rdata")

write.csv(montreal_pts,"Montreal-trajectory-points-na-removed.csv")
#Create trajectories using adehabitatLT- for extraction of GPS features 
load("Montreal-Trajectory-points-sf-dataframe.Rdata")

#check and remove duplicate dates
sum(duplicated(paste(montreal_pts$timestamp,montreal_pts$id_trip)))
dupz = which(duplicated(paste(montreal_pts$timestamp,montreal_pts$id_trip)))
montreal_no_dup = montreal_pts[-dupz,]
dates = montreal_pts$timestamp[-dupz]

df1 = montreal_no_dup[1:500000,]
dates1 = dates[1:500000]
coords1 = data.frame(lat = df1$latitude, lon = df1$longitude)
traj1 = as.ltraj(xy=coords1,
                date=dates1,
                id=df1$id_trip,
                proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))

save(traj1,file = "Trajectories_Montreal_1_500000.Rdata")

df2 = montreal_no_dup[500001:2000000,]
dates2 = dates[500001:2000000]
coords2 = data.frame(lat = df2$latitude, lon = df2$longitude)
traj2 = as.ltraj(xy=coords2,
                 date=dates2,
                 id=df2$id_trip,
                 proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
save(traj2,file = "Trajectories_Montreal_500000_2000000.Rdata")


df3 = montreal_no_dup[2000001:5000000,]
dates3 = dates[2000001:5000000]
coords3 = data.frame(lat = df3$latitude, lon = df3$longitude)
traj3 = as.ltraj(xy=coords3,
                 date=dates3,
                 id=df3$id_trip,
                 proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
save(traj3,file = "Trajectories_Montreal_2000001_5000000.Rdata")

df4 = montreal_no_dup[5000001:7500000,]
dates4 = dates[5000001:7500000]
coords4 = data.frame(lat = df4$latitude, lon = df4$longitude)
traj4 = as.ltraj(xy=coords4,
                 date=dates4,
                 id=df4$id_trip,
                 proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
save(traj4,file = "Trajectories_Montreal_5000001_9477008.Rdata")


df5 = montreal_no_dup[7500001:9477008,]
dates5 = dates[7500001:9477008]
coords5 = data.frame(lat = df5$latitude, lon = df5$longitude)
traj5 = as.ltraj(xy=coords5,
                 date=dates5,
                 id=df5$id_trip,
                 proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
save(traj5,file = "Trajectories_Montreal_7500001_9477008.Rdata")

## Compute speed from the summary statistics of the trajectory of each user

generateGpsAttrib <- function(data_gps_attr)
{
  data_gps_attr[[1]]$speed = data_gps_attr[[1]]$dist/data_gps_attr[[1]]$dt
  data_gps_attr[[1]]$year = year(data_gps_attr[[1]]$date)
  data_gps_attr[[1]]$month = month(data_gps_attr[[1]]$date)
  data_gps_attr[[1]]$day = day(data_gps_attr[[1]]$date)
  data_gps_attr[[1]]$trip = as.character(summary(data_gps_attr[1])$id)
  df = data_gps_attr[[1]]
  for (i in seq(2:length(data_gps_attr)))
  {
    data_gps_attr[[i]]$speed = data_gps_attr[[i]]$dist/data_gps_attr[[i]]$dt
    data_gps_attr[[i]]$year = year(data_gps_attr[[i]]$date)
    data_gps_attr[[i]]$month = month(data_gps_attr[[i]]$date)
    data_gps_attr[[i]]$day = day(data_gps_attr[[i]]$date)
    data_gps_attr[[i]]$trip = as.character(summary(data_gps_attr[i])$id)
    df = rbind(df,data_gps_attr[[i]])
  }
  return(df)
}

traj1Attributes <- generateGpsAttrib(traj1)
traj2Attributes <- generateGpsAttrib(traj2)
traj3Attributes <- generateGpsAttrib(traj3)
traj4Attributes <- generateGpsAttrib(traj4)
traj5Attributes <- generateGpsAttrib(traj5)

traj.final <- rbind(traj1Attributes,traj2Attributes,traj3Attributes,traj4Attributes,traj5Attributes)
write.csv(traj.final,file = "GPS-Traj-Features-Montreal.csv")

#Convert the feature set into shapefile
traj.final <- read.csv("GPS-Traj-Features-Montreal.csv")
coordinates(traj.final) = ~y+x
projection(traj.final) = "+init=epsg:4326"
shapefile(traj.final,filename = "GPS_Features_Montreal.shp", overwrite=TRUE)

df1 = montreal_traj[,c(1,2)] %>% st_drop_geometry()

traj.modes <- merge(df1,traj.final@data, by.x = "id_trip", by.y = "trip")
write.csv(traj.modes,"Montreal_GPS_features_with_modes.csv")


traj.modes <- read.csv("Montreal_GPS_features_with_modes.csv")
traj.modes$mode <- as.character(traj.modes$mode)
traj.modes$modeEng <- gsub("Transport collectif","Public",traj.modes$mode)
traj.modes$modeEng <- gsub("À pied","Active",traj.modes$modeEng)
traj.modes$modeEng <- gsub("Vélo","Active",traj.modes$modeEng)
traj.modes$modeEng <- gsub("Voiture / Moto","Private",traj.modes$modeEng)
traj.modes$modeEng <- gsub("Autopartage","Public",traj.modes$modeEng)
traj.modes$modeEng <- gsub("Taxi","Private",traj.modes$modeEng)
traj.modes$modeEng <- gsub("Autre","Other",traj.modes$modeEng)
unique(traj.modes$modeEng)
#French-English translations for modes
'''
 "Transport collectif" :  "Public transport" - Public
 "À pied": "Walk" - Active
 "Vélo": "Bicycle" - Active
 "Voiture / Moto": "Car / Motorcycle" - Private
 "Autopartage": "Carsharing" - Public
 "Taxi" - Private
 "Other" - Other
 Combinations - Mixed
'''

#Read collisions data
collisions <- read.csv("C:/Users/aroy29/Dropbox (ASU)/INTERACT Travel Mode Detection/NCDB_1999_to_2017.csv")
head(collisions)
collisions2017 <- subset(collisions, C_YEAR == 2017)
head(collisions2017)

#Read trajectories
library(sf)
gpsTraj <- st_read("GPS_Features_Montreal.shp")
n = nrow(gpsTraj)-1
gpsTraj$ID <- c(0:n)

#Read distance to GIS features
bikeInfra <- read.csv("Dist_BikeInfra.txt")[,c(2,4)]
busStops <- read.csv("Dist_to_busStops.txt")[,c(2,4)]
resAreas <- read.csv("Distance_to_residential_Areas.txt")[,c(2,4)]
openSpace <- read.csv("Dist_to_open_spacess.txt")[,c(2,4)]
shoreLine <- read.csv("Distance_to_shore.txt")[,c(2,4)]
commAreas <- read.csv("Distance_to_commercial_Areas.txt")[,c(2,4)]
subways <- read.csv("Dist_to_subways.txt")[,c(2,4)]

#Join using trip id
df1 <- merge(gpsTraj,bikeInfra,by.x = "ID", by.y = "IN_FID", all.x = TRUE)
df1 <- df1[,-c(2,4,5,16)] %>% st_drop_geometry()
colnames(df1)[13] <- 'DistBike'
head(df1)
df2 <- merge(df1,busStops,by.x = "ID", by.y ="IN_FID", all.x = TRUE)
colnames(df2)[14] <- 'DistBus'
df3 <- merge(df2,resAreas, by.x = "ID", by.y = "IN_FID", all.x = TRUE)
colnames(df3)[15] <- 'DistRes'
df4 <- merge(df3,openSpace, by.x = "ID", by.y = "IN_FID", all.x = TRUE)
colnames(df4)[16] <- 'DistOpen'
df5 <- merge(df4,shoreLine, by.x = "ID", by.y = "IN_FID", all.x = TRUE)
colnames(df5)[17] <- 'DistShore'
df6 <- merge(df5,commAreas, by.x = "ID", by.y = "IN_FID", all.x = TRUE)
colnames(df6)[18] <- 'DistComm'
df7 <- merge(df6,subways, by.x = "ID", by.y = "IN_FID", all.x = TRUE)
colnames(df7)[19] <- 'DistSubway'
head(df7)

write.csv(df7,"Montreal_GPS_GIS_feature_set.csv")
#Join with trip ids to collect modes
df = read.csv("Montreal_GPS_GIS_feature_set.csv")
df$id_trip <- df$trip
df$date <- ymd_hms(df$date)
traj.modes$date <- ymd_hms(traj.modes$date)
df8 <- merge(df, traj.modes[,c(2:4)], by = c("date","id_trip"))


MontrealFeatures <- read.csv("C:/Users/aroy29/Dropbox (ASU)/INTERACT Travel Mode Detection/Data/Montreal/Montreal_merged_GPS_GIS_Features_with_modes.csv")
str(MontrealFeatures)
MontrealFeatures$date <- ymd_hms(as.character(MontrealFeatures$date))

#separate different modes
MontrealFeatures$mode <- as.character(MontrealFeatures$mode)
walk_df = subset(MontrealFeatures,mode == 'À pied')
bike_df = subset(MontrealFeatures,mode == 'Vélo')
motorvehicle_df = subset(MontrealFeatures, mode == 'Voiture / Moto')
taxi_df = subset(MontrealFeatures, mode == 'Taxi')
carpool_df = subset(MontrealFeatures, mode == 'Autopartage')
public_df = subset(MontrealFeatures, mode == 'Transport collectif')

#Create 3 categories - active , public and private
activeModes = rbind(walk_df,bike_df)
activeModes$mode = 'Active'
privateModes = rbind(motorvehicle_df, taxi_df)
privateModes$mode = 'Private'
publicModes = rbind(carpool_df,public_df)
publicModes$mode = 'Public'

#Combine all 3 modes
allModes = rbind(activeModes,privateModes,publicModes)

#Join the Montreal data frame with original dataset to get height
library(sf)
library(lubridate)
ptsMontreal <- st_read("points_mtl_trajet_2017.shp")
str(ptsMontreal)
ptsMontreal$date <- ymd_hms(substr(as.character(ptsMontreal$timestamp),1,nchar(as.character(ptsMontreal$timestamp))-3))
df <- ptsMontreal[,c(4,8,10)] %>% st_drop_geometry()
head(df)

allModes <- merge(allModes,df,by = c("date","id_trip"))
write.csv(allModes,"Montreal_featureSet_3modes.csv")
