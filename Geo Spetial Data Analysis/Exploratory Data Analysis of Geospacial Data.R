#Load data
library(readr)
Data <- read_csv("invento labs pvt ltd/Data_for_intern_project_1.csv", 
                 col_types = cols(device_time_stamp = col_datetime(format = "%m/%d/%Y %H:%M"), 
                                  packet_generating_station_id = col_character()))
#EDA of Data
str(Data)
summary(Data)
dim(Data)
anyNA(Data$track_record_id)
NA_val<-Data[which(is.na(Data$track_record_id)),]#4
Data<-Data[-which(is.na(Data$track_record_id)),]
anyNA(Data)
unique(Data$status_code)
Data$status_code<-factor(Data$status_code, levels=c(61445,61441), labels = c("good_log","bad_log"))
unique(Data$device_id_x)#38 equipments
nrow(table(unique(Data$track_record_id)))# 236339
asset_type<-data.frame(table(Data$asset_type))#17(11,4,1,)
Data$packet_generating_station_id<-as.character(Data$packet_generating_station_id)
unique(Data$packet_generating_station_id)#38
table(Data$track_num1)
unique(Data$asset_id_project)#38
unique(Data$project_id)#1(2)
unique(Data$supportsite_id)#2,3
hist(Data$status_code)
max(Data$device_time_stamp)-min(Data$device_time_stamp)


#Date-Time of logs used for counting total number of geospecial logs coming from different equipments per Day/hours.
hours<-format(Data$device_time_stamp,"%H")
Data$hours<-format(Data$device_time_stamp,"%H")
head(hours,20)
tail(hours)
barplot(table(hours),xlab = "Hour",ylab="count",col="Lightgreen",las=2,border = "gray")

Data$Day<-format(Data$device_time_stamp,"%A")
Data$Day<-as.factor(Data$Day)
barplot(table(Data$Day),xlab = "Day",ylab="count",col="orange",main="Logs per Weekday")

sum(table(Data$Day))#236339

#Compute daylength (photoperiod) for a latitude and date.
library(geosphere)
Data$time_stamp<-as.Date(Data$device_time_stamp,format="%yyyy-%mm-%dd")
Data$daylength<-daylength(Data$latitude_gps,Data$time_stamp)#in hours


#Plot Longitude and Lattitide
plot(Data$longitude_gps,Data$latitude_gps, cex=psize, pch=20, col='red',main = "Equipments")
polygon(Data$longitude_gps,Data$latitude_gps, col='blue', border='light blue')


#Creating SpatialPointDataFrame
library(sp)
Data.longitude_gpslatitude_gps = Data[c("longitude_gps", "latitude_gps")]
coordinates(Data.longitude_gpslatitude_gps) <- ~longitude_gps+latitude_gps
class(Data.longitude_gpslatitude_gps)
proj4string(Data.longitude_gpslatitude_gps)<-CRS("+proj=longlat +ellps=WGS84")
summary(Data.longitude_gpslatitude_gps)

#
library(raster)
lonlat<-cbind(Data$longitude_gps,Data$latitude_gps)
lns <- spLines(lonlat, crs=crdref)
lns
pols <- spPolygons(lonlat, crs=crdref)
pols
str(pols)
plot(pols, axes=TRUE, las=1)
plot(pols, border='blue', col='yellow', lwd=3, add=TRUE)
points(pts, col='red', pch=20, cex=3)


##Draw the map of world
library(rworldmap)
newmap<-getMap(resolution = "low")
plot(newmap,xlim=c(0,97.25),ylim=c(0,45),asp=1)
#lay the equipment location on the map
points(lonlat,col="red",cex=1)


data<-subset(Data,Data$latitude_gps==0)
summary(data)
table(data$status_code)
barplot(table(data$status_code))

#Plot main equipment
coord<-cbind(77.33970642,28.09029007)
plot(coord, cex=psize, pch=20, col='red',main = "Main Equipments")
newmap<-getMap(resolution = "low")
plot(newmap,xlim=c(0,97.25),ylim=c(0,45),asp=1)
#lay the equipment location on the map
points(coord,col="red",cex=1)
points(lonlat,col="blue",cex=.5)

#solution c:a
#for day wise plot
attach(Data)
#remove all bad_logs
Data<-Data[(Data$status_code=="good_log"),]
wed<-with(Data,subset(Data,Data$Day=="Wednesday"))
unique(wed$packet_generating_station_id)
thu<-with(Data,subset(Data,Data$Day=="Thursday"))
fri<-with(Data,subset(Data,Day=="Friday"))
sat<-with(Data,subset(Data,Day=="Saturday"))
sun<-with(Data,subset(Data,Day=="Sunday"))


library(raster)
wed_coord<-cbind(wed$longitude_gps,wed$latitude_gps)
thu_coord<-cbind(thu$longitude_gps,thu$latitude_gps)
fri_coord<-cbind(fri$longitude_gps,fri$latitude_gps)
sat_coord<-cbind(sat$longitude_gps,sat$latitude_gps)
sun_coord<-cbind(sun$longitude_gps,sun$latitude_gps)
coord<-cbind(77.33970642,28.09029007)#mainequipment

#Solution c-b
#between 9:00 am,9:10,am
anyNA(Data)#FALSE
Data$device_time_stamp<-as.character(Data$device_time_stamp)
wed_new<-Data[(Data$device_time_stamp>"2017-12-13 09:00:00")&(Data$device_time_stamp<"2017-12-13 09:10:00"),]
thu_new<-Data[(Data$device_time_stamp>"2017-12-14 09:00:00")&(Data$device_time_stamp<"2017-12-14 09:10:00"),]
fri_new<-Data[(Data$device_time_stamp>"2017-12-15 09:00:00")&(Data$device_time_stamp<"2017-12-15 09:10:00"),]
sat_new<-Data[(Data$device_time_stamp>"2017-12-16 09:00:00")&(Data$device_time_stamp<"2017-12-16 09:10:00"),]
sun_new<-Data[(Data$device_time_stamp>"2017-12-17 09:00:00")&(Data$device_time_stamp<"2017-12-17 09:10:00"),]
mon_new<-Data[(Data$device_time_stamp>"2017-12-18 09:00:00")&(Data$device_time_stamp<"2017-12-18 09:10:00"),]#no observation

library(raster)
wed_coord<-cbind(wed_new$longitude_gps,wed_new$latitude_gps)
thu_coord<-cbind(thu_new$longitude_gps,thu_new$latitude_gps)
fri_coord<-cbind(fri_new$longitude_gps,fri_new$latitude_gps)
sat_coord<-cbind(sat_new$longitude_gps,sat_new$latitude_gps)
sun_coord<-cbind(sun_new$longitude_gps,sun_new$latitude_gps)
coord<-cbind(77.33970642,28.09029007)#mainequipment

library(rworldmap)
newmap<-getMap(resolution = "low")
plot(newmap,xlim=c(0,97.25),ylim=c(0,45),asp=1)
#lay the equipment location on the map
points(coord,col="red",cex=3,pch=18)

points(wed_coord,col="blue",cex=2,pch=19)
points(thu_coord,col="yellow",cex=1.5,pch=15)
points(fri_coord,col="green",cex=1.5,pch=16)
points(sat_coord,col="orange",cex=1.5,pch=17)
points(sun_coord,col="gray",cex=1.5,pch=20)

#Simple  interactive method 
library("leaflet")
m <- leaflet()
  m<-  addTiles(m)  # Add default OpenStreetMap map tiles
  m<-  addMarkers(m,lng=77.33970642, lat=28.09029007, popup="Main Equipment")
  m<-  addMarkers(m,lng=wed_new$longitude_gps, lat=wed_new$latitude_gps)#between 9:00 am to 9:10am
  m # Print the map

                      



  

