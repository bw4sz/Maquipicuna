#####################################
#GPS MATCHING AND FLOWER TAXONOMY
#####################################

#Load GPS
#Read and convert gpx points to a single dataframe and save it as a shapefile
require(plotKML)
require(reshape2)
require(maptools)
require(dplyr)
require(stringr)
require(taxize)
library(ggplot2)

#Set working directory
droppath<-"C:/Users/Ben/Dropbox/"

setwd(droppath)

###############################################
#Read in GPS Data from Summer 2013 Field Season for Karen
###############################################

f<-list.files("Thesis\\Maquipucuna_SantaLucia\\Data2013\\GPS",full.names=TRUE,pattern=".gpx",recursive=TRUE)

gpxkaren<-list()
for (x in 1:length(f)){
  try(
    gpxkaren[[x]]<-readGPX(f[x],waypoints=TRUE)$waypoints)
}

#################################################
#Read in GPS Data from Holger Field Season
#################################################

##Repeat for first gps
g<-list.files("Holger\\Transect_Protocol_Holger\\WayPoints",full.names=TRUE,pattern=".gpx",recursive=TRUE)

#loop through input files and find the errors. 
gpx<-list()

g<-capture.output(for (x in 1:length(g)){
  try(
    gpx[[x]]<-readGPX(g[x],waypoints=TRUE)$waypoints)
})

#Bind into one dataframe, bit ugly
gpx.dat<-rbind_all(list(
  rbind_all(gpx[sapply(gpx,class)=="data.frame"])
  ,rbind_all(gpxkaren[sapply(gpxkaren,class)=="data.frame"])))

gpx.dat$name<-as.character(gpx.dat$name)

#Combine gps types
colnames(gpx.dat)

#create  spatial object
gps<-SpatialPointsDataFrame(coords=cbind(gpx.dat$lon,gpx.dat$lat),as.data.frame(gpx.dat))

#Create month ID column in the GPS data
gps$MonthID<-sapply(gps$time,function(x){
  b<-strsplit(as.character(x),"T")[[1]][1]
  if(is.na(b)){
    return("S")
  }
  return(as.numeric(format(as.POSIXlt(b),"%m")))
})

#Date column
#Create month ID column in the GPS data
gps$Date_F<-sapply(gps$time,function(x){
  b<-strsplit(as.character(x),"T")[[1]][1]
  if(is.na(b)){
    return("S")
  }
  return(format(as.POSIXlt(b),"%Y-%m-%d"))
})

#remove bad gps events
gps<-gps[!gps$Date_F=="S",]

#Create Year ID column in the GPS data
gps$YearID<-sapply(gps$Date_F,function(b){
yr<-format(as.POSIXlt(b),"%Y")
  return(as.numeric(yr))
})

#Round elevation to the nearest 10m 
gps$ele<-round(as.numeric(as.character(gps$ele)),-1)

#remove weird place holger from values below 100
gps$GPS.ID<-sapply(gps$name,function(x){
  a<-as.numeric(as.character(x))
  if(is.na(a)){
    return(x)
  }
  if(is.numeric(a)){
    return(a)
  }
})

#remove duplicates
gps_noduplicate<-gps[-duplicated(gps@data),]

#create shapefile
writePointsShape(gps,"Thesis\\Maquipucuna_SantaLucia\\Data2013\\Shapefiles\\GPSshapeFlowerTransects.shp")

gps<-gps[!duplicated(gps@data),]

############################################
##############Merge GPS Info with Data######
############################################

#There are two video files, that need to be merge
dat<-read.csv("Thesis/Maquipucuna_SantaLucia/Results/FlowerTransects/CleanedHolgerTransect.csv")

head(dat)

#merge
datg<-merge(dat,gps_noduplicate,by.x=c("GPS_ID","month","year"),by.y=c("GPS.ID","MonthID","YearID"))

#remove nonsensical elevations
datg$ele[datg$ele > 2600]<-NA
datg$ele[datg$ele < 1200]<-NA

#There are few dates that don't match, by 1 day.
datg[datg$month==10 & datg$year==2014 & datg$Transect_R=='1900_2100' & datg$Date_F=="2014-10-23","Date_F"]<-"2014-10-22"
datg[datg$month==10 & datg$year==2014 & datg$Transect_R=='1500_1700' & datg$Date_F=="2014-10-22","Date_F"]<-"2014-10-27"
datg[datg$month==8 & datg$year==2013 & datg$Transect_R=='2300_2500' & datg$Date_F=="2013-08-07","Date_F"]<-"2013-08-03"
datg[datg$month==8 & datg$year==2013 & datg$Transect_R=='2100_2300' & datg$Date_F=="2013-08-03", "Date_F"]<-"2013-08-02"
datg[datg$month==8 & datg$year==2013 & datg$Transect_R=='1700_1900' & datg$Date_F=="2013-08-03", "Date_F"]<-"2013-08-02"
datg[datg$month==7 & datg$year==2014 & datg$Transect_R=='1700_1900' & datg$Date_F=="2014-07-03","Date_F"]<-"2014-07-22"
datg[datg$month==7 & datg$year==2014 & datg$Transect_R=='1300_1500' & datg$Date_F=="2014-07-02","Date_F"]<-"2014-07-16"
datg[datg$month==7 & datg$year==2013 & datg$Transect_R=='2100_2300' & datg$Date_F=="2013-07-26","Date_F"]<-"2013-07-25"
datg[datg$month==7 & datg$year==2013 & datg$Transect_R=='1700_1900' & datg$Date_F=="2013-07-26","Date_F"]<-"2013-07-27"
datg[datg$month==7 & datg$year==2013 & datg$Transect_R=='1500_1700' & datg$Date_F=="2014-07-25","Date_F"]<-"2013-07-19"

#Write camera data to file
write.csv(datg,"Thesis/Maquipucuna_SantaLucia/Results/FlowerTransects/FlowerTransectClean.csv")

## Elevation ranges 

#Elevation Ranges

eleIndex<-dplyr::group_by(datg,Iplant_Double) %>% dplyr::summarize(Low=quantile(ele,0.2,na.rm=T),m=mean(ele,na.rm=T),High=quantile(ele,0.8,na.rm=T))

#1 for low elevation, 2 for high elevation, 3 for both
#high elevation
eleIndex[eleIndex$Low > 1700,"Index"]<-2

#low elevation
eleIndex[eleIndex$High < 1700,"Index"]<-1

#The result are both
eleIndex[is.na(eleIndex$Index),"Index"]<-3

#view result
as.data.frame(eleIndex)

write.csv(eleIndex,"C:/Users/Ben/Documents/Maquipicuna/OutData/PlantElevation.csv")

