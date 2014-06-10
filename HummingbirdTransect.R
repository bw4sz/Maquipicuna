##########################
#Hummingbird Transect Data
##########################

#Data Collected by Holger Beck,K. Lohman and B. Weinstein
#R script Ben Weinstein - Stony Brook University 7/7/2013
#Under Git repository - Maquipucuna

#Read in required libraries
require(ggplot2)
require(stringr)
require(reshape2)
require(maptools)
require(plyr)
require(plotKML)
require(reshape)
require(chron)
require(rPlant)

#Set DropBox Working Directory
#setwd("C:/Users/Ben/Dropbox/")

#Read in workspace if desired for quick access
#load("Thesis/Maquipucuna_SantaLucia/Results/HummingbirdTransect.Rdata")

#Read in Ben's transect data
Hum<-read.csv("Thesis/Maquipucuna_SantaLucia/Data2013/csv/HummingbirdTransect.csv")
head(Hum)

#Transect IDs from Summer 2013 data
TID<-read.csv("Thesis/Maquipucuna_SantaLucia/Data2013/csv/TransectIID.csv")

#Bring in holger's hummingbird datasheet.
holger.hum<-read.csv("Thesis/Maquipucuna_SantaLucia/Data2013/csv/HolgerTransect_Hummingbirds.csv")
head(holger.hum)

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

#This is causing a real headache, for now just take the last two chracters of the year
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
holgerID$ID[!holgerID$ID %in% holger.hum$ID]

#How many dates are missing?
levels(droplevels(holger.hum$ID[!holger.hum$ID %in% holgerID$ID]))
holger.full<-merge(holger.hum,holgerID,"ID")

#Get all the rows in holger.hum that have species
holger.full$Full<-paste(holger.full$Family,holger.full$Genus,holger.full$Species)
#holgerInter<-holger.full[!holger.full$Full %in% "  " ,]

#legacy change, keep all observations
holgerInter<-holger.full

################
#Flower Taxonomy
################

#Repeat for genus species
Species<-levels(factor(paste(holgerInter$Genus,holgerInter$Species,sep=" ")))

#look up online, skip the blank
tax<-tnrs(query = Species[-1], source = "iPlant_TNRS")

#Set the Species column
for (x in 1:nrow(holgerInter)){
  y<-holgerInter[x,]
  toMatch<-paste(y$Genus,y$Species,sep=" ")
  if(toMatch %in% tax$submittedname){
    holgerInter[x,"Iplant_Double"]<-unique(tax[tax$submittedname %in% toMatch,"acceptedname"]   )
  } else {
    next
  }}

#Lots of cleaning left to do, but that's a start. 

#Fix any known ID mistakes

holgerInter[holgerInter$Iplant_Double %in% "Heppiella ulmifolia","Iplant_Double"]<-"Glossoloma_oblongicalyx"

#Final levels
print(paste("Final Flower Species:", levels(factor(holgerInter$Iplant_Double))))

#get the desired columns
colnames(holgerInter)
holgerInter<-holgerInter[,colnames(holgerInter) %in% c("ID","Hummingbird.Species","Iplant_Double","Way.Point","Month","Date_F.y","Transect_R")]

monthInter<-melt(table(holgerInter$Hummingbird.Species,holgerInter$Iplant_Double,holgerInter$Month))
colnames(monthInter)<-c("Hummingbird","Plant","Month","value")

#visualize holger only data
#remove no interactions
monthInter<-monthInter[!monthInter$value==0,]
p<-ggplot(monthInter,aes(Hummingbird,Plant,fill=value)) + geom_tile() + facet_wrap(~Month) + scale_fill_continuous(na.value="White",high="red") + theme_bw()
p<- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(p)

#########################
####Add GPS information
#########################

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
gpx.dat<-rbind.fill(rbind.fill(gpx2[sapply(gpx2,class)=="data.frame"]))
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

#################
#Merge GPS info with transects

#Merge all that fit the month and ID?
HMatch<-merge(holgerInter,gps,by.x=c("Way.Point","Month"),by.y=c("GPS.ID","MonthID"),all.x=TRUE,all.y=FALSE)

dim(HMatch)
dim(holgerInter)

#how many are missing?
nrow(HMatch[is.na(HMatch$ele),])
missingGPS<-HMatch[is.na(HMatch$ele),]$Way.Point

#For missing data, take the mean of the transect?
#For any data still missing gps, take the mean of the transect
for (x in missingGPS){
  print(x)
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

#Take out empty rows?
hum.id<-hum.id[!is.na(hum.id$Plant.Species),]

#remove trinomial frmo plant species
levels(hum.id$Plant.Species)<-sapply(levels(hum.id$Plant.Species),function(x){
    nw<-length(strsplit(x, " ")[[1]])
    if(nw==3){
      return(word(x,2,3))
    } else return(x)
  })

Species<-levels(hum.id$Plant.Species)

#look up online, skip the blank
tax<-tnrs(query = Species, source = "iPlant_TNRS")

#Set the Species column
for (x in 1:nrow(hum.id)){
  y<-hum.id[x,]
  toMatch<-y$Plant.Species
  if(toMatch %in% tax$submittedname){
    hum.id[x,"Iplant_Double"]<-unique(tax[tax$submittedname %in% toMatch,"acceptedname"]   )
  } else {
    next
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
  print(x)
  tr<-BMatch[BMatch$GPS.ID %in% x,"Transect_R"]
  el<-mean(as.numeric(strsplit(as.character(tr),split="_")[[1]]))
  BMatch[BMatch$GPS.ID %in% x,"altitude"]<-el
}

#round to the nearest 10m
BMatch$altitude<-round(BMatch$altitude,-1)
qplot(BMatch$altitude,"point")

################################################
#Interaction table for summer data
################################################

#Cap all species names
levels(BMatch$Hummingbird.Species)<-sapply(levels(BMatch$Hummingbird.Species),function(x){.simpleCap(tolower(x))})
humInter<-melt(table(BMatch$Hummingbird.Species,BMatch$Iplant_Double,BMatch$Month))
colnames(humInter)<-c("Hummingbird","Plant","Month","value")

#visualize summer only data
#remove no interactions
humInter<-humInter[!humInter$value==0,]
p<-ggplot(humInter,aes(Hummingbird,Plant,fill=value)) + geom_tile() + facet_wrap(~Month) + scale_fill_continuous(na.value="White",high="red") + theme_bw()
p<- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(p)

################################################
#Combine Holger's transect data with summer data
################################################
fullInter<-rbind(humInter,monthInter)

p<-ggplot(fullInter,aes(Hummingbird,Plant,fill=value)) + geom_tile() + facet_wrap(~Month,nrow=2) + scale_fill_continuous(na.value="White",high="red") + theme_bw()
p<- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(p)
ggsave(filename="Thesis/Maquipucuna_SantaLucia/Results/HummingbirdTransects/HummingbirdTransectInteractions.jpeg",height=15,width=20)

#write this matrix to file
write.csv(fullInter,"Thesis/Maquipucuna_SantaLucia/Results/HummingbirdTransects/HumTransectMatrix.csv")
print("Matrix_Written")

##############################
#Combine transect Rows
##############################
head(BMatch)
head(HMatch)

Brows<-BMatch[,colnames(BMatch) %in% c("GPS.ID","Iplant_Double","Hummingbird.Species","Month","Date_F","Transect_R","ID","altitude","lat","long")]

colnames(HMatch)
Hrows<-HMatch[,colnames(HMatch) %in% c("Way.Point" ,"Iplant_Double","Hummingbird.Species","Month","Date_F.y","Transect_R","ID","ele","lat","lon")]

colnames(Hrows)
colnames(Brows)[colnames(Brows) %in% c("altitude","long")]<-c("lon","ele")
colnames(Hrows)[colnames(Hrows) %in% c("Way.Point","Date_F.y")]<-c("GPS.ID","Date_F")

transectRows<-rbind.fill(Brows,Hrows)

write.csv(transectRows,"Thesis/Maquipucuna_SantaLucia/Results/HummingbirdTransects/HumTransectRows.csv")

#Return end of file
print("HummingbirdTransects")

save.image("Thesis/Maquipucuna_SantaLucia/Results/HummingbirdTransect.Rdata")
