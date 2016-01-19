##########################
#Hummingbird Transect Data
##########################

#Data Collected by Holger Beck,K. Lohman and B. Weinstein
#R script Ben Weinstein - Stony Brook University 7/7/2013
#Under Git repository - Maquipucuna

#Read in required libraries
require(ggplot2)
require(reshape2)
require(maptools)
require(dplyr)
require(plotKML)
require(reshape)
require(chron)
library(taxize)
library(stringr)
#Set DropBox Working Directory
setwd("C:/Users/Ben/Dropbox/")

#Read in workspace if desired for quick access
#load("Thesis/Maquipucuna_SantaLucia/Results/HummingbirdTransect.Rdata")

#For the sake of simplicity, make everything lowercase
.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

#Read in Ben's transect data
Hum<-read.csv("Thesis/Maquipucuna_SantaLucia/Data2013/csv/HummingbirdTransect.csv")

#Transect IDs from Summer 2013 data
TID<-read.csv("Thesis/Maquipucuna_SantaLucia/Data2013/csv/TransectIID.csv")

#Bring in holger's hummingbird datasheet.
holger.hum<-read.csv("Thesis/Maquipucuna_SantaLucia/Data2013/csv/HolgerTransect_Hummingbirds.csv")

#Bring in holger transect data
holgerID<-read.csv("Thesis/Maquipucuna_SantaLucia/Data2013/csv/TransectIIDHolger.csv")

#Fix holger's ID elev columns, make them more general, transect delim
for (x in 1:6){
  holgerID[holgerID$Transect %in% x,"Elevation.Begin"]<-1100 + 200*x 
  holgerID[holgerID$Transect %in% x,"Elevation.End"]<-1300 + 200*x
}

####################################
#Clean Holger's data, begins in 9/2013
###################################
holgerID$Transect_R<-factor(paste(holgerID$Elevation.Begin,holgerID$Elevation.End,sep="_"))

#Format dates
holgerID$Date_F<-sapply(holgerID$Date,function(x){
  #grab the year
  d<-strsplit(as.character(x),split="/")[[1]]
  yr<-d[[3]]
  #get the last two characters
  yrsplit<-substr(yr, nchar(yr)-2+1, nchar(yr))
  dat_f<-as.Date(paste(paste(d[[1]],d[[2]],sep="/"),yrsplit,sep="/"),format="%d/%m/%y")
  return(as.character(dat_f))
})

holger.hum$Date_F<-sapply(holger.hum$Date,function(x){
  #grab the year
  d<-strsplit(as.character(x),split="/")[[1]]
  yr<-d[[3]]
  #get the last two characters
  yrsplit<-substr(yr, nchar(yr)-2+1, nchar(yr))
  dat_f<-as.Date(paste(paste(d[[1]],d[[2]],sep="/"),yrsplit,sep="/"),format="%d/%m/%y")
  return(as.character(dat_f))
})

#Create Month Columns
holgerID$Month<-as.numeric(format(as.Date(holgerID$Date_F),"%m"))

#Create ID columns
holgerID$ID<-factor(paste(holgerID$Transect,holgerID$Date_F,sep="_"))
holger.hum$ID<-factor(paste(holger.hum$Transect,holger.hum$Date_F,sep="_"))

#How many dates are missing?
dim(holger.hum)
dim(holgerID)

holger.full<-merge(holger.hum,holgerID,by=c("ID","Date_F"))
dim(holger.full)

#Needs a year column
holger.full$Year<-years(holger.full$Date_F)
#legacy change, keep all observations
holgerInter<-holger.full

################
#Flower Taxonomy
################

#Go through a series of data cleaning steps, at the end remove all rows that are undesired
Families<-levels(factor(holgerInter$Family))
tax<-gnr_resolve(names = Families, splitby=30,best_match_only =T,canonical  = T)

#Set the Family column
for (x in 1:nrow(holgerInter)){
  y<-holgerInter[x,]
  toMatch<-y$Family
  if(!toMatch %in% tax$submitted_name){next} else{
    holgerInter[x,"Iplant_Family"]<-tax[tax$submitted_name %in% toMatch,"matched_name2"]
  }}

#Known id error:
holgerInter$Genus<-as.character(holgerInter$Genus)
holgerInter$Species<-as.character(holgerInter$Species)

holgerInter$Genus[holgerInter$Genus %in%  "Hepiella"]<-"Glossoloma" 
holgerInter$Species[holgerInter$Species %in%  "ulmifolia"]<-"oblongicalyx" 

#Repeat for species double
Species<-levels(factor(paste(holgerInter$Genus,holgerInter$Species,sep=" ")))
tax<-gnr_resolve(names = Species, splitby=30,best_match_only = T,canonical = T)

#Set the Species column
for (x in 1:nrow(holgerInter)){
  y<-holgerInter[x,]
  toMatch<-factor(paste(y$Genus,y$Species,sep=" "))
  if(!toMatch %in% tax$submitted_name){next} else{
    holgerInter[x,"Iplant_Double"]<-tax[tax$submitted_name %in% toMatch,"matched_name2"]
  }}

#replace any missing names.
toinsert<-holgerInter[is.na(holgerInter$Iplant_Double),]
toin <- gsub(" $","", paste(toinsert$Genus,toinsert$Species), perl=T)
holgerInter[is.na(holgerInter$Iplant_Double), "Iplant_Double"]<-toin

#get the desired columns
holgerInter<-holgerInter[,colnames(holgerInter) %in% c("ID","Hummingbird.Species","Iplant_Double","Way.Point","Month","Date_F","Transect_R","Transect","Year")]

#########################
####Add GPS information
#########################

##Repeat for first gps
g<-list.files("Holger\\Transect_Protocol_Holger\\WayPoints",full.names=TRUE,pattern=".gpx",recursive=TRUE)

#loop through input files and find the errors. 
gpx2<-list()
for (x in 1:length(g)){
  try(
    gpx2[[x]]<-readGPX(g[x],waypoints=TRUE)$waypoints)
}

#Bind into one dataframe
gpx.dat<-rbind_all(gpx2[sapply(gpx2,class)=="data.frame"])
gpx.dat$name<-as.character(gpx.dat$name)

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

gps$Year<-years(gps$Date_F)

#remove literally identical rows
gps<-gps[!duplicated(gps@data),]

#############################
#Merge GPS info with transects
#############################

#Merge all that fit the month year and ID
HMatch<-merge(holgerInter,gps,by.x=c("Way.Point","Month","Year"),by.y=c("GPS.ID","MonthID","Year"),all.x=T)

#just want the date from the input data.
HMatch<-HMatch[,!colnames(HMatch) %in% "Date_F.y"]
colnames(HMatch)[colnames(HMatch) %in% "Date_F.x"]<-"Date_F"

nrow(HMatch[is.na(HMatch$ele),])
missingGPS<-HMatch[is.na(HMatch$ele),]$Way.Point

#For missing data, take the mean of the transect?
#For any data still missing gps, take the mean of the transect
for (x in missingGPS){
  tr<-HMatch[HMatch$Way.Point %in% x,"Transect_R"]
  el<-mean(as.numeric(strsplit(as.character(tr),split="_")[[1]]))
  HMatch[HMatch$Way.Point %in% x,"ele"]<-el
}

qplot(HMatch$ele,"point")

#create shapefile
write.csv(HMatch,"Thesis/Maquipucuna_SantaLucia/Results/HummingbirdTransects/HolgerHummingbirdTransectsCleaned.csv")

#################################
#Clean Summer 2013 Data
#################################

#add in summer transect data
head(Hum)

#fix the unknown uppercase
levels(Hum$Hummingbird.Species)[levels(Hum$Hummingbird.Species) %in% "UkWN"]<-toupper(levels(Hum$Hummingbird.Species)[levels(Hum$Hummingbird.Species) %in% "UkWN"])

#Create identical structure to holgers data, we need a data and month column
# Fix the caps at the plant species
table(Hum$Plant.Species)
levels(Hum$Plant.Species)[levels(Hum$Plant.Species) %in% ""]<-NA
table(Hum$Plant.Species)

#Select the Bird transects
TID.f<-TID[TID$Type %in% "Hummingbird",]
TID$TransectID<-as.factor(TID$TransectID)
Hum$ID
hum.id<-merge(Hum,TID.f,by.x="ID",by.y="TransectID")

#Create date column
hum.id$Date_F<-as.Date(as.character(hum.id$Date),"%m/%d/%Y")
hum.id$Month<-as.numeric(format(as.Date(hum.id$Date_F),"%m"))
hum.id$Transect_R<-paste(hum.id$Elevation.Begin,hum.id$Elevation.End,sep="_")

############################
###Taxonomoy of plant names
############################

#Repeat for genus
plants<-levels(factor(hum.id$Plant.Species))
#Go through a series of data cleaning steps, at the end remove all rows that are undesired

Families<-levels(factor(plants))
tax<-gnr_resolve(names = Families, splitby=30,best_match_only = T,canonical = T)

#Set the Family column
for (x in 1:nrow(hum.id)){
  y<-hum.id[x,]
  toMatch<-y$Plant.Species
  if(!toMatch %in% tax$submitted_name){next} else{
    hum.id[x,"Iplant_Double"]<-tax[tax$submitted_name %in% toMatch,"matched_name2"]
  }}

###########################
#Attach GPS information
########################

formerGPS<-read.csv("Thesis/Maquipucuna_SantaLucia/Data2013/GPS/Ben2013SummerGPS.txt",header=TRUE)

#remove weird place holger from values below 100
formerGPS$GPS.ID<-sapply(as.character(formerGPS$ident),function(x){
  a<-as.numeric(x)
  if(is.na(a)){
    return(x)
  }
  if(!is.na(a)){
    return(a)
  }
})

#Merge all that fit the month and ID?
BMatch<-merge(hum.id,formerGPS,"GPS.ID",all.x=TRUE,all.y=FALSE)

dim(BMatch)
dim(hum.id)

#how many are missing?
nrow(BMatch[is.na(BMatch$altitude),])

missingGPS<-BMatch[is.na(BMatch$altitude),]$GPS.ID

#For missing data, take the mean of the transect?
#For any data still missing gps, take the mean of the transect
for (x in missingGPS){
  tr<-BMatch[BMatch$GPS.ID %in% x,"Transect_R"]
  el<-mean(as.numeric(strsplit(as.character(tr),split="_")[[1]]))
  BMatch[BMatch$GPS.ID %in% x,"altitude"]<-el
}

#round to the nearest 10m
BMatch$altitude<-round(BMatch$altitude,-1)
qplot(BMatch$altitude,"point")

##############################
#Combine transect Rows
##############################
head(BMatch)
head(HMatch)

Brows<-BMatch[,colnames(BMatch) %in% c("GPS.ID","Iplant_Double","Hummingbird.Species","Month","Date_F","Transect_R","ID","altitude","lat","long")]

colnames(HMatch)
Hrows<-HMatch[,colnames(HMatch) %in% c("Way.Point" ,"Iplant_Double","Hummingbird.Species","Month","Date_F","Transect_R","ID","ele","lat","lon")]

colnames(Hrows)
colnames(Brows)[colnames(Brows) %in% c("altitude","long")]<-c("lon","ele")
colnames(Hrows)[colnames(Hrows) %in% c("Way.Point","Date_F.y")]<-c("GPS.ID","Date_F")

Hrows$Date_F<-as.Date(Hrows$Date_F)
Brows$ID<-as.character(Brows$ID)

transectRows<-rbind_all(list(Brows,Hrows))

#error on dateF for a handful of rows.
resplit<-transectRows$ID[is.na(transectRows$Date_F)]
redate<-sapply(resplit,function(x){
  str_split(x,"_")[[1]][[2]]
  })

transectRows$Date_F[is.na(transectRows$Date_F)]<-redate

write.csv(transectRows,"Thesis/Maquipucuna_SantaLucia/Results/HummingbirdTransects/HumTransectRows.csv")

#Return end of file
print("HummingbirdTransects")

save.image("Thesis/Maquipucuna_SantaLucia/Results/HummingbirdTransect.Rdata")

