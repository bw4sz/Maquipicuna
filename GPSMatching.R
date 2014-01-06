#Load GPS
#Read and convert gpx points to a single dataframe and save it as a shapefile
require(plotKML)
require(reshape2)
require(maptools)
require(reshape)

#Set working directory
#droppath<-"C:/Users/Jorge/Dropbox/"
#setwd(droppath)

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

#error rows
#merge
datg<-merge(dat,gps,by.x="ID",by.y="GPS.ID",all.x=TRUE)

dim(dat)
dim(datg)


#find duplicates
d<-data.frame(table(dat$ID),table(datg$ID))

dups<-d[which(!d$Freq == d$Freq.1),]$Var1

#Repeat merge at elim duplicates
dup.g<-gps[gps$GPS.ID %in% dups,]
dup.g[order(dup.g$GPS.ID),]

gps<-gps[-c(5198,663),]

#merge
datg<-merge(dat,gps,by.x="ID",by.y="GPS.ID",all.x=TRUE)

dim(dat)
dim(datg)

paste("Missing Cameras GPS:",levels(factor(datg[is.na(datg$ele),]$ID)))

###### Add in manual elev branches for missing levels?

datg[datg$ID %in% "FL066","ele"]<-1350
datg[datg$ID %in% "FL084","ele"]<-1850
datg[datg$ID %in% "FL049","ele"]<-1350
datg[datg$ID %in% "FL050","ele"]<-1600
datg[datg$ID %in% "FL053","ele"]<-1550
datg[datg$ID %in% "FL054","ele"]<-1500


################
#Flower Taxonomy
################

#Go through a series of data cleaning steps, at the end remove all rows that are undesired

#Repeat for species
Species<-levels(factor(datg$Flower))
iplant_names<-ResolveNames(Species)
print(CompareNames(Species,iplant_names))
Species_Result<-data.frame(Species,iplant_names)

#Set the Species column
for (x in 1:nrow(datg)){
  y<-datg[x,]
  toMatch<-y$Flower
  datg[x,"Iplant_Double"]<-levels(droplevels(
  Species_Result[Species_Result$Species %in% toMatch,"iplant_names"] ))   
}

#Write camera data to file
write.csv(datg,"Thesis/Maquipucuna_SantaLucia/Data2013/csv/FlowerVideoClean.csv")
