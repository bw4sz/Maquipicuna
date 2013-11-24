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
#load("Thesis/Maquipucuna_SantaLucia/Results/FlowerTransect.Rdata")

#Read in Flower Transect Data from summer field season
fl<-read.csv(file="Thesis/Maquipucuna_SantaLucia/Data2013/csv/FlowerTransects.csv")

#Load in holger's on going data
holger.fl<-read.csv("Thesis/Maquipucuna_SantaLucia/Data2013/csv/HolgerTransects.csv")

#Bring in holger's hummingbird datasheet.
holger.hum<-read.csv("Thesis/Maquipucuna_SantaLucia/Data2013/csv/HolgerTransect_Hummingbirds.csv")

#Bring in holger transect data
holgerID<-read.csv("Thesis/Maquipucuna_SantaLucia/Data2013/csv/TransectIIDHolger.csv")

#Fix the column names and rbindfill
colnames(fl)<-c("Family","Genus","Species","Height","Flowers","Stalks","Inflorescences.Plants","GPS_ID","Accuracy","Hummingbird.Species","Photo","Transect.ID","Comment")
colnames(holger.fl)<-c("Transect.ID","Date","Time","Family","Genus","Species","Height","Flowers","Stalks","Inflorescences.Plants","Hummingbird.Species","GPS_ID","Comment","Photo")

#To compare transects we need the transect id page
TID<-read.csv("Thesis/Maquipucuna_SantaLucia/Data2013/csv/TransectIID.csv")
TID$Transect_R<-as.factor(paste(TID$Elevation.Begin,TID$Elevation.End,sep="_"))

#Select the flower transects
TID.f<-TID[TID$Type=="Flower",]
fl.id<-merge(fl,TID.f,by.x="Transect.ID",by.y="TransectID")

#view flower data
head(fl.id)

full.fl<-rbind.fill(fl.id,holger.fl[,1:14])

head(full.fl)
#Data cleaning and check
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
ggplot(fl.t,aes(Family,Count)) + geom_bar() + coord_flip() + theme_bw()

#Turn genus to uppercase
full.fl$Genus<-as.factor(sapply(full.fl$Genus,function(x) .simpleCap(as.character(x))))
table(full.fl$Genus)
fl.g<-melt(table(full.fl$Genus))
names(fl.g)<-c("Genus","Count")
ggplot(fl.g,aes(Genus,Count)) + geom_bar() + coord_flip() + theme_bw()

#MS species need to be subbed in for the best taxonomy known
full.fl<-full.fl[!full.fl$Family %in% c("Ms1","Ms2"),]

########################################################
#Combination of family genus count
fl.fg<-melt(table(paste(full.fl$Family,full.fl$Genus)))
names(fl.fg)<-c("F.Genus","Count")
ggplot(fl.fg,aes(F.Genus,Count)) + geom_bar() + coord_flip() + theme_bw()

#For species, turn all to lowercase
full.fl$Species<-as.factor(sapply(full.fl$Species,tolower))
fl.s<-melt(table(full.fl$Species))
names(fl.s)<-c("Species","Count")
ggplot(fl.s,aes(Species,Count)) + geom_bar() + coord_flip() + theme_bw()

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

full.fl<-full.fl[!is.na(full.fl$mean_flowerStalk),]

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

#########################
#########################
#Read and convert gpx points to a single dataframe and save it as a shapefile
f<-list.files("Holger/Transect_Protocol_Holger/WayPoints/",full.names=TRUE)

#loop through input files and find the errors. 
gpx<-list()
for (x in 1:length(f)){
  print(x)
  try(
    gpx[[x]]<-readGPX(f[x],waypoints=TRUE)$waypoints)
}

##Repeat for first gps
f<-list.files("F:\\KarenGPS\\KarenFirstgps/",full.names=TRUE)

#loop through input files and find the errors. 
gpx2<-list()
for (x in 1:length(f)){
  print(x)
  try(
    gpx2[[x]]<-readGPX(f[x],waypoints=TRUE)$waypoints)
}
#Bind together the days taht contain data
gpx.dat<-rbind(rbind.fill(gpx[sapply(gpx,class)=="data.frame"]),rbind.fill(gpx2[sapply(gpx2,class)=="data.frame"]))

#create  spatial object
dat.sp<-SpatialPointsDataFrame(coords=cbind(gpx.dat$lon,gpx.dat$lat),gpx.dat)

#Turn GPS ID into factor
full.fl$GPS_ID<-as.numeric(full.fl$GPS_ID)

#add to 00 to anything less than 10
full.fl$GPS_ID[full.fl$GPS_ID < 10]<-paste("00",full.fl$GPS_ID[full.fl$GPS_ID < 10],sep="")
full.fl$GPS_ID[as.numeric(full.fl$GPS_ID) < 100 & as.numeric(full.fl$GPS_ID) >= 10 ]<-paste("0",full.fl$GPS_ID[as.numeric(full.fl$GPS_ID) < 100 & as.numeric(full.fl$GPS_ID) >= 10],sep="")

#Match each point with an elevation
fl.elev<-merge(full.fl,dat.sp,by.x="GPS_ID",by.y="name")

#turn elevation to a number, not a chararacter, and round to the nearest 10m?
fl.elev$ele<-round(as.numeric(fl.elev$ele),-1)

#How records were in fl, but not matched
length(full.fl[!full.fl$GPS_ID %in% fl.elev$GPS_ID,]$GPS_ID)

#How many flowers at each elevation
ggplot(fl.elev,aes(ele,y=Total_Flowers)) + geom_line() + geom_point(aes(color=Family),size=2) + theme_bw() 

p<-ggplot(fl.elev,aes(x=ele,y=Total_Flowers,col=Species),size=2) + geom_point() + theme_bw() + facet_wrap(~Family,scales='free_y') + guides(color="none")
p+stat_smooth(method='lm',aes(group=Family))

#spatially
ggplot(fl.elev,aes(x=coords.x1,y=coords.x2,col=Total_Flowers),size=200) + theme_bw() + geom_point() + scale_color_continuous(high="red",low="blue",limits=c(0,200))

#Set holger as observer
fl.elev$Observer<-as.character(fl.elev$Observer)

fl.elev$Observer[is.na(fl.elev$Observer)]<-"Holger"

fl.elev$Observer<-factor(fl.elev$Observer)


#Create month column
fl.elev$month<-NA
for (j in 1:nrow(fl.elev)){
  x<-fl.elev[j,]
  if(x[["Observer"]] %in% c("Karen","Ben")){
    fl.elev[j,"month"]<-months(chron(as.character(x$Date)))
  }
    if(x[["Observer"]] %in% "Holger"){
      fl.elev[j,"month"]<-months(chron(as.character(x[x$Observer %in% "Holger","Date"]),format="d/m/y"))
    }
  }

#cut data into the 200m transects?

bre<-seq(1300,2500,200)
fl.elev$transectR<-cut(fl.elev$ele,bre,dig.lab=4)

#plot total flowers over time?
fl.totals<-aggregate(fl.elev$Total_Flowers,list(fl.elev$transectR,fl.elev$month,fl.elev$Date),sum)
colnames(fl.totals)<-c("Elev","Month","Date","TotalFlowers")

ggplot(fl.totals,aes(Elev,TotalFlowers,col=as.factor(Month))) + geom_point(size=4) + geom_smooth(aes(group=Month)) 
##Flowers per month and elevation
ggplot(fl.totals,aes(x=Elev,TotalFlowers,col=as.factor(Month))) + geom_point() + geom_smooth(aes(group=Month)) + theme_bw()
#ggsave()


ggplot(fl.totals,aes(Elev,TotalFlowers)) + facet_wrap(~Month) + geom_boxplot()
#Create interaction matrix for hummingbirds and flowers
save.image("Thesis/Maquipucuna_SantaLucia/Results/FlowerTransect.Rdata")
