#Set working directory
setwd("C:/Users/aroy29/Dropbox (ASU)/INTERACT Travel Mode Detection/Avipsa_Analysis/Travel-Mode-Detection/Data/")

#Read Features from all 3 cities
stJohns = read.csv("StJohnsFeatureSet.csv")
Vancouver = read.csv("VancouverFeatureSet.csv")
Montreal = read.csv("MontrealFeatureSet.csv")[,-c(1)]

colnames(stJohns)
colnames(Vancouver)
colnames(Montreal)

#Add name of city in a separate variable
stJohns$city <- 'St. Johns'
Vancouver$city <- 'Vancouver'
Montreal$city <- 'Montreal'

#combine data from all 3 cities
allCities <- rbind(stJohns,Vancouver,Montreal)
write.csv(allCities,"GPS_GIS_attributes_all_cities.csv")
