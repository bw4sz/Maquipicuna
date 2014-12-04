#####################################
#GPS MATCHING AND FLOWER TAXONOMY
#####################################

#Load GPS
#Read and convert gpx points to a single dataframe and save it as a shapefile
require(plotKML)
require(reshape2)
require(maptools)
require(plyr)
require(stringr)
require(taxize)
#Set working directory
droppath<-"C:/Users/Ben/Dropbox/"

setwd(droppath)

###############################################
#Read in GPS Data from Summer 2013 Field Season
###############################################

f<-list.files("Thesis\\Maquipucuna_SantaLucia\\Data2013\\GPS",full.names=TRUE,pattern=".gpx",recursive=TRUE)

gpx<-list()
for (x in 1:length(f)){
  try(
  gpx[[x]]<-readGPX(f[x],waypoints=TRUE)$waypoints)
}


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
  try(
    gpx2[[x]]<-readGPX(g[x],waypoints=TRUE)$waypoints)
}

###########Nelly gps
g<-list.files("Nelly\\DataEntry\\Nelly Informacion Camaras\\WAYPOINTS",full.names=TRUE,pattern=".gpx",recursive=TRUE)

#loop through input files and find the errors. 
gpx3<-list()
for (x in 1:length(g)){
  try(
    gpx3[[x]]<-readGPX(g[x],waypoints=TRUE)$waypoints)
}

#Bind into one dataframe
gpx.dat<-rbind.fill(rbind.fill(gpx[sapply(gpx,class)=="data.frame"]),rbind.fill(gpx2[sapply(gpx2,class)=="data.frame"]),rbind.fill(gpx3[sapply(gpx3,class)=="data.frame"]))
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

#nelly didn't put in 0 before name in gps, but did for cameras

#first letter of the name is N
nelldata<-gps[substring(gps$name,1,1) %in% "N",]

#get the digits
## split string at non-digits
s <- strsplit(nelldata$GPS.ID, "[^[:digit:]]")

## convert strings to numeric ("" become NA)
solution <- as.numeric(unlist(s))

## remove NA and duplicates
solution <- solution[!is.na(solution)]

newname<-paste("NF0",solution,sep="")

#replace name
gps[which(substring(gps$name,1,1) %in% "N"),"GPS.ID"]<-newname

#create shapefile
writePointsShape(gps,"Thesis\\Maquipucuna_SantaLucia\\Data2013\\Shapefiles\\GPSshape.shp")

############################################
##############Merge GPS Info with Data######
############################################

#There are two video files, that need to be merge
dat<-read.csv("Thesis/Maquipucuna_SantaLucia/Data2013/csv/FlowerVideo.csv")

colnames(dat)[11]<-"Pierce"

datAuto<-read.csv("Thesis/Maquipucuna_SantaLucia/Data2013/FlowerVideoAuto.csv")

#Which flID are in the original, but not the automated

datADD<-dat[!dat$ID %in% datAuto$ID,]

#Combine with automated monitering
datT<-rbind.fill(datAuto,datADD)

#error rows
#merge
datg<-merge(datT,gps,by.x="ID",by.y="GPS.ID",all.x=TRUE)

dim(datT)
dim(datg)


#find duplicates
d<-data.frame(table(datT$ID),table(datg$ID))

dups<-d[which(!d$Freq == d$Freq.1),]$Var1

#Repeat merge at elim duplicates
dup.g<-gps[gps$GPS.ID %in% dups,]
dup.g[order(dup.g$GPS.ID),]

#additional hardcoded duplicates or hand errors
gps<-gps[-c(5198,663),]

gps[gps$ID %in% "NF009","GPS.ID"]<-"NF09"
gps[gps$ID %in% "NF2","GPS.ID"]<-"NF02"
gps[gps$ID %in% "NF003","GPS.ID"]<-"NF03"


#remerge
datg<-merge(datT,gps,by.x="ID",by.y="GPS.ID",all.x=TRUE)

dim(datT)
dim(datg)

paste("Missing Cameras GPS:",levels(factor(datg[is.na(datg$ele),]$ID)))

###### Add in manual elev branches for missing levels?

datg[datg$ID %in% "FL066","ele"]<-1350
datg[datg$ID %in% "FL084","ele"]<-1850
datg[datg$ID %in% "FL049","ele"]<-1350
datg[datg$ID %in% "FL050","ele"]<-1600
datg[datg$ID %in% "FL053","ele"]<-1550
datg[datg$ID %in% "FL054","ele"]<-1500

#Still missing elevation information
paste("Missing Cameras GPS:",levels(factor(datg[is.na(datg$ele),]$ID)))

################
#Flower Taxonomy
################

#Go through a series of data cleaning steps, at the end remove all rows that are undesired

datg[datg$Flower %in% "Columnea ","Flower"]<-"Columnea"

#remove family names they just confuse the issue
# if trinomial remove first word

levels(datg$Flower)<-sapply(levels(datg$Flower),function(x){
  nw<-length(strsplit(x, " ")[[1]])
  if(nw ==3){
    return(word(x,2,3))
  } else {
    return(x)
  }
})


#Repeat for species
SpeciesG<-levels(factor(datg$Flower))

tax<-tnrs(query = unique(SpeciesG), source = "iPlant_TNRS")

#remove one troublesome
datg<-datg[!datg$Flower %in% 'sp. ("ginger")',]

#Set the Species column
for (x in 1:nrow(datg)){
  print(x)
  y<-datg[x,]
  toMatch<-y$Flower
  datg[x,"Iplant_Double"]<-unique(tax[tax$submittedname %in% toMatch,"acceptedname"])
}

#Write camera data to file
write.csv(datg,"Thesis/Maquipucuna_SantaLucia/Data2013/csv/FlowerVideoClean.csv")

print("GPS data merge complete")