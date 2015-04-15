#####################################
#GPS MATCHING AND FLOWER TAXONOMY
#####################################

#Load GPS
#Read and convert gpx points to a single dataframe and save it as a shapefile
require(plotKML)
require(reshape2)
require(maptools)
library(plyr)
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

#Bind into one dataframe
gpx.dat<-rbind.fill(rbind.fill(gpx[sapply(gpx,class)=="data.frame"]),rbind.fill(gpxkaren[sapply(gpxkaren,class)=="data.frame"]))
gpx.dat$name<-as.character(gpx.dat$name)

#Combine gps types
colnames(gpx.dat)

#create  spatial object
gps<-SpatialPointsDataFrame(coords=cbind(gpx.dat$lon,gpx.dat$lat),gpx.dat)

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

############################################
##############Merge GPS Info with Data######
############################################

#There are two video files, that need to be merge
dat<-read.csv("Thesis/Maquipucuna_SantaLucia/Results/FlowerTransects/CleanedHolgerTransect.csv")

head(dat)

#merge
datg<-merge(dat,gps_noduplicate,by.x=c("GPS_ID","month","year"),by.y=c("GPS.ID","MonthID","YearID"),)

#remove nonsensical elevations
datg<-datg[datg$ele < 2600,]

#Write camera data to file
write.csv(datg,"Thesis/Maquipucuna_SantaLucia/Results/FlowerTransects/FlowerTransectClean.csv")

