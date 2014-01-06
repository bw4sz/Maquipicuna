#Load GPS
#Read and convert gpx points to a single dataframe and save it as a shapefile
require(plotKML)
require(reshape2)
require(maptools)

#Set working directory
droppath<-"C:/Users/Ben/Dropbox/"
setwd(droppath)

###############################################
#Read in GPS Data from Summer 2013 Field Season
###############################################

f<-list.files("Thesis\\Maquipucuna_SantaLucia\\Data2013\\GPS",full.names=TRUE,pattern=".gpx",recursive=TRUE)

gpx<-list()
for (x in 1:length(f)){
  print(x)
  try(
  gpx[[x]]<-readGPX(f[x],waypoints=TRUE)$waypoints)
}

#Which one failed? Why?
readGPX(f[135])

#read in 
formerGPS<-read.csv("Thesis/Maquipucuna_SantaLucia/Data2013/GPS/Ben2013SummerGPS.txt",header=TRUE)

#################################################
#Read in GPS Data from Holger Field Season
#################################################

##Repeat for first gps
g<-list.files("Holger\\Transect_Protocol_Holger\\WayPoints",full.names=TRUE,pattern=".gpx",recursive=TRUE)

#loop through input files and find the errors. 
gpx2<-list()
for (x in 1:length(g)){
  print(x)
  try(
    gpx2[[x]]<-readGPX(g[x],waypoints=TRUE)$waypoints)
}

#Need to remind nelly to upload her gps. 

#Bind into one dataframe
gpx.dat<-rbind.fill(rbind.fill(gpx[sapply(gpx,class)=="data.frame"]),rbind.fill(gpx2[sapply(gpx2,class)=="data.frame"]))
gpx.dat$name<-as.character(gpx.dat$name)

#Combine gps types
colnames(gpx.dat)
tomatch<-formerGPS[,colnames(formerGPS) %in% c("lat" ,"long","altitude","ident")]

colnames(tomatch)<-c("name","lat","lon","ele")

tomatch$name<-as.character(tomatch$name)
gpx.all<-rbind.fill(gpx.dat,tomatch)

#create  spatial object
gps<-SpatialPointsDataFrame(coords=cbind(gpx.all$lon,gpx.all$lat),gpx.all)

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

#Round to the nearest 10m 
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

#create shapefile
writePointsShape(gps,"Thesis\\Maquipucuna_SantaLucia\\Data2013\\Shapefiles\\GPSshape.shp")

############################################
##############Merge GPS Info with Data######
############################################

dat<-read.csv("Thesis/Maquipucuna_SantaLucia/Data2013/csv/FlowerVideo.csv")

datg<-merge(dat,gps,by.x="ID",by.y="GPS.ID")

dim(dat)
dim(datg)

datg[datg$ID %in% dat$"GPS.ID",]

levels(factor(datg[is.na(datg$ele),]$ID))

###### Add in manual elev branches for missing levels?

datg[datg$ID %in% "FL066","ele"]<-1350
datg[datg$ID %in% "FL084","ele"]<-1850
datg[datg$ID %in% "FL049","ele"]<-1350
datg[datg$ID %in% "FL050","ele"]<-1600
datg[datg$ID %in% "FL053","ele"]<-1550
datg[datg$ID %in% "FL054","ele"]<-1500

#Write camera data to file
write.csv("Thesis/Maquipucuna_SantaLucia/Data2013/csv/FlowerVideo.csv")

#########################################################3

#Start with hummingbird observations on transects
transectRows<-read.csv("Thesis/Maquipucuna_SantaLucia/Results/HummingbirdTransects/HumTransectRows.csv",row.names=1)

#match gps type
transectRows$MonthID<-sapply(transectRows$Month,function(x){
  if(x == c(6,7,8)){
    return("S")
  }
  if(!x==c(6,7,8)){
    return(x)
  }
})

#Merge all that fit the month and ID?
TransectMatch<-merge(transectRows,gps,by.x=c("GPS.ID","MonthID"),by.y=c("name","MonthID"),all.x=TRUE,all.y=FALSE)

#Which points are missing information


#Combine holger points
#get the records that are not summer ("S") or 6 7 8 == monthID
gpsH<-gps[!gps$MonthID %in% c("S","6","7","8"),]
Holgerpts<-transectRows[!transectRows$Month %in% c(6,7,8),]

#merge dataframes
HolgerMatch<-merge(Holgerpts,gpsH,by.x=c("GPS.ID","Month"),by.y=c("name","MonthID"),all.x=TRUE)

#Get missing GPS IDs
holgerMissing<-HolgerMatch[is.na(HolgerMatch$ele),]$GPS.ID
HolgerMatch[HolgerMatch$GPS.ID %in% holgerMissing,"HolgerID"]<-as.numeric(as.character(holgerMissing)))

for (x in holgerMissing){
  print(x)
  lookup<-HolgerMatch[HolgerMatch$ID_N %in% x,"ID_N"]
  gpsH[gpsH$HolgerID %in% lookup,"ele"]<-HolgerMatch[HolgerMatch$ID_N %in% x,"ele"]
}

#remerge
paste("Still Missing Holger Levels",levels(factor(HolgerMissingpts[!HolgerMissingpts$HolgerID %in% gpsH$HolgerID,]$HolgerID)))
HolgerMatchMiss<-merge(HolgerMissingpts,gpsH,by="HolgerID")

#Bind all levels together
Holger_tran<-rbind.fill(HolgerMatch,HolgerMatchMiss)
missingInfo<-Holgerpts[!Holgerpts$GPS.ID %in% Holger_tran$GPS.ID,]$GPS.ID

#For any data still missing gps, take the mean of the transect
for (x in missingInfo){
  tr<-dat_e[dat_e$ID %in% x,"Transect_R"]
  el<-mean(as.numeric(strsplit(as.character(tr),split="_")[[1]]))
  dat_e[dat_e$ID %in% x,"ele"]<-el
}

datelev<-rbind.fill(dat_GPS,HolgerMatch)
datf<-rbind.fill(datelev,HolgerMatchMiss)

#Okay what am i still missing
finalMissing<-levels(factor(dat[!dat$ID %in% datf$ID,]$ID))

print(paste(finalMissing,"Final Missing Elevation"))





















####Combine with GPS info seperately, due to weird IDs of old GPS, non-unique
#Ben's GPS Summer 2013 had no dates





#Get levels from summer for Ben's GPS
BenSummer<- gps[is.na(gps$time),]
BenSummerpts<-dat[dat$Month %in% c(6,7,8),]

#Merge first set of gps points
dat_part1<-merge(BenSummerpts,BenSummer,by.x="ID",by.y="name")

#Any missing levels, look up on the other GPS, probably taken by Karen or Anusha
missingGPS<-levels(factor(BenSummerpts$ID))[!levels(factor(BenSummerpts$ID)) %in% BenSummer$name]

#Combine missing levels
found_Summer<-merge(BenSummerpts[BenSummerpts$ID %in% missingGPS,],gps,by.x="ID",by.y="name")
print(paste("Ben Missing Levels:",missingGPS[!missingGPS %in% gps$name]))


#Add the remaining levels with NA elevation until they can be corroborated?
dat_e<-rbind.fill(datf,dat[dat$ID %in% finalMissing,])

#humans can only be in one place at once, this should look like a step function
#ggplot(dat_e,aes(x=Date,y=ele)) + geom_point() + geom_line() + coord_flip()

#For the holger points, take the mean of the above and below point, can't have walked very far.

for (x in c("864","887","868","892","898","899","901","930")){
  tr<-dat_e[dat_e$ID %in% x,"Transect_R"]
  el<-mean(as.numeric(strsplit(as.character(tr),split="_")[[1]]))
  dat_e[dat_e$ID %in% x,"ele"]<-el
}

dat_e[is.na(dat_e$ele),]