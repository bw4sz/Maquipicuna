#Flower Diversity Transects and Nectar
#Data Collected by Holger Beck,K. Lohman and B. Weinstein
#R script Ben Weinstein - Stony Brook University 7/7/2013
#Under Git repository - Maquipucuna

#Read in required libraries
require(ggplot2)
require(reshape2)
require(maptools)
library(plyr)
require(plotKML)
require(reshape)
require(chron)

#Set DropBox Working Directory
setwd("C:/Users/Ben/Dropbox/")

#Read in workspace if desired for quick access
load("Thesis/Maquipucuna_SantaLucia/Results/FlowerTransect.Rdata")

#Read in Flower Transect Data from summer field season
fl<-read.csv(file="Thesis/Maquipucuna_SantaLucia/Data2013/csv/FlowerTransects.csv")

#Load in holger's on going data
holger.fl<-read.csv("Thesis/Maquipucuna_SantaLucia/Data2013/csv/HolgerTransects.csv")

#Bring in holger's hummingbird datasheet.
holger.hum<-read.csv("Thesis/Maquipucuna_SantaLucia/Data2013/csv/HolgerTransect_Hummingbirds.csv")

#Bring in holger transect data
holgerID<-read.csv("Thesis/Maquipucuna_SantaLucia/Data2013/csv/TransectIIDHolger.csv")

#Create ID columns
holgerID$ID<-factor(paste(holgerID$Transect,holgerID$Date,sep="_"))

holger.fl$ID<-factor(paste(holger.fl$Transect,holger.fl$Date,sep="_"))

holgerID$ID[!holgerID$ID %in% holger.fl$ID]
levels(droplevels(holger.fl$ID[!holger.fl$ID %in% holgerID$ID]))

holger.full<-merge(holger.fl,holgerID,"ID")

#Fix the column names and rbindfill
colnames(fl)<-c("Family","Genus","Species","Height","Flowers","Stalks","Inflorescences.Plants","GPS_ID","Accuracy","Hummingbird.Species","Photo","Transect.ID","Comment")

#Just get the desired columns and rename
holger.full<-holger.full[,colnames(holger.full) %in% c(colnames(holger.full)[1:15],"Elevation.Begin","Elevation.End") ]
colnames(holger.full)<-c("ID","Transect.ID","Date","Time","Family","Genus","Species","Height","Flowers","Stalks","Inflorescences.Plants","Hummingbird.Species","GPS_ID","Comment","Photo","Elevation.Begin","Elevation.End")

#To compare transects we need the transect id page
TID<-read.csv("Thesis/Maquipucuna_SantaLucia/Data2013/csv/TransectIID.csv")

#Select the flower transects
TID.f<-TID[TID$Type=="Flower",]

#Make factors both
fl$Transect.ID<-factor(fl$Transect.ID)
TID.f$TransectID<-factor(TID.f$TransectID)

###############################
#NEEDS TO ADDRESS
which(!TID.f$TransectID %in% fl$Transect.ID)

#Missing level?
fl.id<-merge(fl,TID.f,by.x="Transect.ID",by.y="TransectID")

#How many rows did we lose?
dim(fl)
dim(fl.id)

head(fl.id)

full.fl<-rbind.fill(fl.id,holger.full)

#Set holger as observer
full.fl$Observer<-as.character(full.fl$Observer)

full.fl$Observer[is.na(full.fl$Observer)]<-"Holger"

full.fl$Observer<-factor(full.fl$Observer)

head(full.fl)

#Create elevation ID
full.fl$Transect_R<-factor(paste(full.fl$Elevation.Begin,full.fl$Elevation.End,sep="_"))

##############################################
#Basic Records and Cleaning of Flower Taxonomy
##############################################

#Turn first letter of family to uppercase
#see toupper?
.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

full.fl$Family<-as.factor(sapply(full.fl$Family,function(x) .simpleCap(as.character(x))))
#How many families do we have?
fl.t<-melt(table(full.fl$Family))
names(fl.t)<-c("Family","Count")
#ggplot(fl.t,aes(Family,Count)) + geom_bar() + coord_flip() + theme_bw()

#Turn genus to uppercase
full.fl$Genus<-as.factor(sapply(full.fl$Genus,function(x) .simpleCap(as.character(x))))
table(full.fl$Genus)
fl.g<-melt(table(full.fl$Genus))
names(fl.g)<-c("Genus","Count")
#ggplot(fl.g,aes(Genus,Count)) + geom_bar() + coord_flip() + theme_bw()

#MS species need to be subbed in for the best taxonomy known
full.fl<-full.fl[!full.fl$Family %in% c("Ms1","Ms2"),]

########################################################
#Combination of family genus count
fl.fg<-melt(table(paste(full.fl$Family,full.fl$Genus)))
names(fl.fg)<-c("F.Genus","Count")
#ggplot(fl.fg,aes(F.Genus,Count)) + geom_bar() + coord_flip() + theme_bw()

#For species, turn all to lowercase
full.fl$Species<-as.factor(sapply(full.fl$Species,tolower))
fl.s<-melt(table(full.fl$Species))
names(fl.s)<-c("Species","Count")
#ggplot(fl.s,aes(Species,Count)) + geom_bar() + coord_flip() + theme_bw()

#Family Genus Species, in future, create abreviations
fl.all<-melt(table(paste(full.fl$Family,full.fl$Genus,full.fl$Species)))
names(fl.all)<-c("Species","Count")
ggplot(fl.all,aes(Species,Count)) + geom_bar() + coord_flip() + theme_bw()
ggsave(filename="Thesis/Maquipucuna_SantaLucia/Results/SpeciesCount.jpeg",height=8,width=11)

#Use this as the marker for now
full.fl$Full<-paste(full.fl$Family,full.fl$Genus,full.fl$Species)

#Create total flower column by multiplying the Flowers per stalk, by stalks and total inflorescens
#For now, remove any rows that have no flowers

#First need to take the average flowers per stalk, and all other columns
full.fl$mean_flowerStalk<-sapply(full.fl$Flowers,function(x) {
  mean(as.numeric(strsplit(as.character(as.list(x)[[1]]),",")[[1]]),na.rm=TRUE)
})

#remove any records that have no flowers info.
full.fl<-full.fl[!is.na(full.fl$mean_flowerStalk),]

##############################################
#Calculate Total Number of Flowers per Records
##############################################

full.fl$mean_Stalk<-sapply(full.fl$Stalks,function(x) {
  mean(as.numeric(strsplit(as.character(as.list(x)[[1]]),",")[[1]]))
})

full.fl$mean_inflorescences<-sapply(full.fl$Inflorescences.Plants,function(x) {
  mean(as.numeric(strsplit(as.character(as.list(x)[[1]]),",")[[1]]))
})

full.fl$Total_Flowers<-full.fl$mean_flowerStalk*full.fl$mean_Stalk*full.fl$mean_inflorescences
hist(full.fl$Total_Flowers)

#Visualize total flowers by species
ggplot(data=full.fl,aes(Full,Total_Flowers)) + geom_boxplot() + coord_flip()

#visualize height by species
ggplot(data=full.fl,aes(Full,as.numeric(Height))) + geom_boxplot() + coord_flip()

################################################
#Number of Flowers at Each Elevation over Time
################################################

#Create month column
full.fl$month<-NA
for (j in 1:nrow(full.fl)){
  x<-full.fl[j,]
  if(x[["Observer"]] %in% c("Karen","Ben")){
    full.fl[j,"month"]<-months(chron(as.character(x$Date)))
  }
    if(x[["Observer"]] %in% "Holger"){
      full.fl[j,"month"]<-months(chron(as.character(x[x$Observer %in% "Holger","Date"]),format="d/m/y"))
    }
  }

#There is the one unique transect where holger did the transect with us. 

head(full.fl[is.na(full.fl$month),])

#plot total flowers over time?
fl.totals<-aggregate(full.fl$Total_Flowers,list(full.fl$Transect_R,full.fl$month,full.fl$Date),sum)
colnames(fl.totals)<-c("Elev","Month","Date","TotalFlowers")

try(dev.off())
##Flowers per month and elevation
ggplot(fl.totals,aes(x=Elev,TotalFlowers,col=as.factor(Month))) + geom_point(size=3) + geom_smooth(aes(group=Month)) + facet_wrap(~Month) + theme_bw() + labs(col="Month")
#ggsave

#Flowers at each elevation over time
ggplot(fl.totals,aes(x=Month,TotalFlowers,col=Elev)) + geom_point(size=3) + theme_bw()  + geom_smooth(aes(group=Elev)) + facet_wrap(~Elev,scales="free_x")
#ggsave()


##############################################
#Read in Spatial Data, still needs to be fixed. 
##############################################

#Read and convert gpx points to a single dataframe and save it as a shapefile
f<-list.files("Holger/Transect_Protocol_Holger/WayPoints/",full.names=TRUE)

#loop through input files and find the errors. 
gpx<-list()
for (x in 1:length(f)){
  print(x)
  try(
    gpx[[x]]<-readGPX(f[x],waypoints=TRUE)$waypoints)
}

##Repeat for Karen's GPS data, label Karen
f<-list.files("F:\\KarenGPS\\KarenFirstgps/",full.names=TRUE)

gpx2<-list()
for (x in 1:length(f)){
  print(x)
  try(
    gpx2[[x]]<-readGPX(f[x],waypoints=TRUE)$waypoints)
}

#Bind together the days that contain data
#Label Observer
holger.gps<-data.frame(rbind.fill(gpx[sapply(gpx,class)=="data.frame"]),Observer="Holger")
karen.gps<-data.frame(rbind.fill(gpx2[sapply(gpx2,class)=="data.frame"]),Observer="Karen")

#Combine data
gpx.dat<-rbind.fill(holger.gps,karen.gps)

full.fl$GPS_ID<-as.numeric(full.fl$GPS_ID)

#Match each point with an elevation
fl.elev<-merge(full.fl,gpx.dat,by.x=c("GPS_ID","Observer"),by.y=c("name","Observer"))

#How records were in fl, but not matched
full.fl[!full.fl$GPS_ID %in% fl.elev$GPS_ID,]$GPS_ID


#spatially
ggplot(fl.elev,aes(x=coords.x1,y=coords.x2,col=Total_Flowers),size=200) + theme_bw() + geom_point() + scale_color_continuous(high="red",low="blue",limits=c(0,200))

#How many flowers at each elevation
ggplot(fl.elev,aes(ele,y=Total_Flowers)) + geom_line() + geom_point(aes(color=Family),size=2) + theme_bw() 

p<-ggplot(fl.elev,aes(x=ele,y=Total_Flowers,col=Species),size=2) + geom_point() + theme_bw() + facet_wrap(~Family,scales='free_y') + guides(color="none")
p+stat_smooth(method='lm',aes(group=Family))

save.image("Thesis/Maquipucuna_SantaLucia/Results/FlowerTransect.Rdata")
