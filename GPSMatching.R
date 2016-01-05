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
library(dplyr)
library(ggplot2)
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

#remove literally identical rows
gps<-gps[!duplicated(gps@data),]
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


#find duplicates with different elevations
d<-data.frame(table(datT$ID),table(datg$ID))

dups<-d[which(!d$Freq == d$Freq.1),]$Var1

#Repeat merge at elim duplicates
dup.g<-gps[gps$GPS.ID %in% dups,]
dup.g[order(dup.g$GPS.ID),]@data

#how many of those name duplicates are identical?

#hardcoded errors, multiple gps points, same name.
gps<-gps[!(gps$name=="FH1108" & gps$ele %in% 1810),]
gps<-gps[!(gps$name=="FH1306" & gps$ele %in% 2240),]
gps<-gps[!(gps$name=="FH316" & gps$ele %in% 2280),]
gps<-gps[!(gps$name=="FH620" & gps$ele %in% 2280),]
gps<-gps[!(gps$name=="FH625" & gps$ele %in% 2160),]
gps<-gps[!(gps$name=="FH629" & gps$ele %in% 2270),]
gps<-gps[!(gps$name=="FL061" & gps$ele %in% 1290),]
gps<-gps[!(gps$name=="FL064" & gps$ele %in% 1600),]
gps<-gps[!(gps$name=="N13" & gps$ele %in% 1610),]

gps[gps$ID %in% "NF009","GPS.ID"]<-"NF09"
gps[gps$ID %in% "NF2","GPS.ID"]<-"NF02"
gps[gps$ID %in% "NF003","GPS.ID"]<-"NF03"

#remerge
datg<-merge(datT,gps,by.x="ID",by.y="GPS.ID",all.x=TRUE)

dim(datT)
dim(datg)

paste("Missing Cameras GPS:",levels(factor(datg[is.na(datg$ele),]$ID)))

#looks like missing some data from nelly
nelldat<-read.csv("C:/Users/Ben/Dropbox/Thesis/Maquipucuna_SantaLucia/Data2013/csv/CameraData Nelly Ultima version 2015.csv")
dt<-str_detect(nelldat$Elevation..m,"(\\d+.\\d+) ft")
ft<-str_match(nelldat$Elevation..m,"(\\d+.\\d+) ft")[,2]
nelldat$Elevation..m<-as.character(nelldat$Elevation..m)

nelldat[!dt,"ele.nelly"]<-as.character(nelldat$Elevation..m.[!dt])
nelldat[dt,"ele.nelly"]<-as.numeric(na.omit(round(as.numeric(ft) * 0.304)))
nelldat$GPSID<-as.character(nelldat$GPSID)

#nelly added a 0 in front of numbers under 100
a<-as.numeric(str_extract(nelldat$GPSID,"\\d+"))
b<-a[a<100 & !is.na(a)]

nelldat[as.numeric(str_extract(nelldat$GPSID,"\\d+"))<100,"GPSID"]<-paste("NF","0",b,sep="")

#nelly added a 0 in front of numbers under 100
a<-as.numeric(str_extract(nelldat$GPSID,"\\d+"))
b<-a[a<10 & !is.na(a)& !a==2]
nelldat[a <10 & !a==2,"GPSID"]<-paste("NF","00",b,sep="")

#fix one individually
nelldat[nelldat$GPSID=="NF02","GPSID"]<-"NF2"

#which Ids are missing elevation?
missingelev<-datg$ID[is.na(datg$ele)]
nellele<-missingelev[missingelev %in% nelldat$GPSID]
datg<-merge(datg,nelldat[,c("GPSID","ele.nelly")],by.x=c("ID"),by="GPSID",all.x=T)
datg[!is.na(datg$ele.nelly),"ele"]<-datg$ele.nelly[!is.na(datg$ele.nelly)]

#still missing some elevations
unique(droplevels(datg$ID[is.na(datg$ele)]))

###### Add in manual elev branches for missing levels?
datg[datg$ID %in% "FL066","ele"]<-1350
datg[datg$ID %in% "FL084","ele"]<-1850
datg[datg$ID %in% "FL049","ele"]<-1350
datg[datg$ID %in% "FL050","ele"]<-1600
datg[datg$ID %in% "FL053","ele"]<-1550
datg[datg$ID %in% "FL054","ele"]<-1500

#appprox inferred from the protocol dates
datg[datg$ID %in% "FH201","ele"]<-2260
datg[datg$ID %in% "NF089","ele"]<-1400

datg[datg$ID %in% "NF089","ele"]<-1400
datg[datg$ID %in% "NF090","ele"]<-1400
datg[datg$ID %in% "NF092","ele"]<-1400
datg[datg$ID %in% "NF096","ele"]<-1400
datg[datg$ID %in% "NF099","ele"]<-1400
datg[datg$ID %in% "NF100","ele"]<-1400
datg[datg$ID %in% "NF101","ele"]<-1400
datg[datg$ID %in% "NF102","ele"]<-1400
datg[datg$ID %in% "NF108","ele"]<-1400
datg[datg$ID %in% "NF108A","ele"]<-1400
datg[datg$ID %in% "NF137","ele"]<-1400

#mysteriouis elevation error, refill from holger table
datg$ele[datg$ele < 1000]<-NA

#
datg$ele<-as.numeric(datg$ele)
#Search for those names
stillmiss<-levels(factor(datg[is.na(datg$ele),]$ID))

#Holger's missing cameras
holgcam<-read.csv(file = "C:/Users/Ben/Dropbox/Thesis/Maquipucuna_SantaLucia/Data2013/csv/Camera_Protocol.csv")
tomerge<-holgcam[holgcam$GPS.ID %in% stillmiss,c("GPS.ID","Elevation")]

for (x in 1:nrow(tomerge)){
  datg[datg$ID %in% tomerge$GPS.ID[x],"ele"]<-as.numeric(as.character(tomerge[x,"Elevation"] ))
}

#remerge missing
#first correct them
levels(datg$ID)[levels(datg$ID) %in% "FH_801"]<-"FH801"
gps@data$GPS.ID[gps@data$GPS.ID %in% "fh1205"]<-"FH1205"
levels(datg$ID)[levels(datg$ID) %in% "FH201HR"]<-"FH201"
datg[datg$ID %in% "FH201","ele"]<-2262
datg[datg$ID %in% "NF197","ele"]<-1400

gps@data$GPS.ID[str_detect(gps@data$GPS.ID,"515")]

datg[datg$ID %in% "",]
gps@data[gps@data$GPS.ID == "KFL216",]

#need to find more information about these cameras:
datg[datg$ele<1000,]

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

tax<-gnr_resolve(names = SpeciesG[-1],best_match_only = T,canonical = T)

#remove one troublesome
datg<-datg[!datg$Flower %in% 'sp. ("ginger")',]

#Set the Species column
for (x in 1:nrow(datg)){
  y<-datg[x,]
  toMatch<-y$Flower
  if(tolower(toMatch) %in% tolower(tax$submitted_name)){
  datg[x,"Iplant_Double"]<-unique(tax[tolower(tax$submitted_name) %in% tolower(toMatch),"matched_name2"])
  }}

#Add in any not known
#for anything not found, need to reinsert
toinsert<-datg[is.na(datg$Iplant_Double),"Flower"]

#make sure to cap correctly
levels(toinsert)[levels(toinsert) %in% "fuschia macrostigma"]<-"fuchsia macrostigma"
datg$Iplant_Double<-as.character(datg$Iplant_Double)

datg[is.na(datg$Iplant_Double), "Iplant_Double"]<-as.character(toinsert)


#Duplicate GPS points. Two were taken with the same name. Check for sample bird, same camera, same time.
a<-datg %>% select(Hummingbird,Iplant_Double,Time)
table(a$Time)

#Write camera data to file
write.csv(datg,"Thesis/Maquipucuna_SantaLucia/Data2013/csv/FlowerVideoClean.csv")

print("GPS data merge complete")
