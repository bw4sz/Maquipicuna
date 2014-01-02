#Load GPS
#Read and convert gpx points to a single dataframe and save it as a shapefile
require(plotKML)
require(reshape2)
require(maptools)

#Set working directory
droppath<-"C:/Users/Jorge/Dropbox/"
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

#Round to the nearest 10m 
gps$ele<-round(as.numeric(as.character(gps$ele)),-1)

#create shapefile
writePointsShape(gps,"Thesis\\Maquipucuna_SantaLucia\\Data2013\\Shapefiles\\GPSshape.shp")

############################################
##############Merge GPS Info with Data######
############################################

#Bring in Holger Flower Transect Data
full.fl<-read.csv("Thesis/Maquipucuna_SantaLucia/Results/FlowerTransects/CleanedHolgerTransect.csv")

#Combine holger points
#get the records that are not summer ("S") or 6 7 8 == monthID
gpsH<-gps[!gps$MonthID %in% c("S","6","7","8"),]
Holgerpts<-full.fl[!full.fl$month %in% c(6,7,8),]

#paste the month and ID together, assuming there are no overlapping IDs in a month
gpsH$HolgerID<-paste(gpsH$MonthID,gpsH$name,sep="_")

#paste month and ID together in the destination dat
Holgerpts$HolgerID<-paste(Holgerpts$month,Holgerpts$GPS_ID,sep="_")

#Which dont levels match from Holger's GPS
paste("Missing Holger GPS IDS",levels(factor(Holgerpts[!Holgerpts$HolgerID %in% gpsH$HolgerID,]$HolgerID)))

#merge dataframes
HolgerMatch<-merge(Holgerpts,gpsH,by="HolgerID")

#Looks like holger added an additional space to numbers below 100?
holgerMissing<-levels(factor(Holgerpts[!Holgerpts$GPS_ID %in% gpsH$name,]$GPS_ID))
HolgerMissingpts<-Holgerpts[Holgerpts$GPS_ID %in% holgerMissing,]
HolgerMissingpts$ID_N<-paste("0",HolgerMissingpts$GPS_ID,sep="")
HolgerMissingpts$HolgerID<-paste(HolgerMissingpts$month,HolgerMissingpts$ID_N,sep="_")

#remerge
paste("Still Missing Holger Levels",levels(factor(HolgerMissingpts[!HolgerMissingpts$HolgerID %in% gpsH$HolgerID,]$HolgerID)))
HolgerMatchMiss<-merge(HolgerMissingpts,gpsH,by="HolgerID")

#maybe even again?
holgerMissing<-levels(factor(Holgerpts[!Holgerpts$GPS_ID %in% gpsH$name,]$GPS_ID))
HolgerMissingpts<-Holgerpts[Holgerpts$GPS_ID %in% holgerMissing,]
HolgerMissingpts$ID_N<-paste("0",HolgerMissingpts$GPS_ID,sep="")
HolgerMissingpts$HolgerID<-paste(HolgerMissingpts$month,HolgerMissingpts$ID_N,sep="_")


#Bind all levels together
dat_GPS<-rbind(dat_part1,found_Summer)
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

###### Add in manual elev branches for missing levels?

dat_e[dat_e$ID %in% "FL066","ele"]<-1350
dat_e[dat_e$ID %in% "FL084","ele"]<-1850
dat_e[dat_e$ID %in% "FL049","ele"]<-1350
dat_e[dat_e$ID %in% "FL050","ele"]<-1600
dat_e[dat_e$ID %in% "FL053","ele"]<-1550
dat_e[dat_e$ID %in% "FL054","ele"]<-1500
dat_e[dat_e$ID %in% "FL066","ele"]<-1550

#For the holger points, take the mean of the above and below point, can't have walked very far.

for (x in c("864","887","868","892","898","899","901","930")){
  tr<-dat_e[dat_e$ID %in% x,"Transect_R"]
  el<-mean(as.numeric(strsplit(as.character(tr),split="_")[[1]]))
  dat_e[dat_e$ID %in% x,"ele"]<-el
}

dat_e[is.na(dat_e$ele),]