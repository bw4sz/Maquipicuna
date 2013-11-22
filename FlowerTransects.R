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
#for now just remove?

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
#First need to take the average flowers per stalk, and all other columns
full.fl$mean_flowerStalk<-sapply(full.fl$Flowers,function(x) {
  mean(as.numeric(strsplit(as.character(as.list(x)[[1]]),",")[[1]]),na.rm=TRUE)
})

full.fl$mean_Stalk<-sapply(full.fl$Stalks,function(x) {
  mean(as.numeric(strsplit(as.character(as.list(x)[[1]]),",")[[1]]))
})

full.fl$mean_inflorescences<-sapply(full.fl$Inflorescences.Plants,function(x) {
  mean(as.numeric(strsplit(as.character(as.list(x)[[1]]),",")[[1]]))
})

full.fl$Total_Flowers<-full.fl$mean_flowerStalk*full.fl$mean_Stalk*full.fl$mean_inflorescences
hist(fl$Total_Flowers)

#Visualize total flowers by species
ggplot(data=full.fl,aes(Full,Total_Flowers)) + geom_boxplot() + coord_flip()

#visualize height by species
ggplot(data=full.fl,aes(Full,as.numeric(Height))) + geom_boxplot() + coord_flip()

################################################
#Data cleaning on hummingbirds
################################################
#Needs to be fixed.
hum.split<-colsplit(as.character(full.fl$Hummingbird.Species),",",c("Hummingbird","Sex"))

full.fl<-data.frame(full.fl,hum.split)

#Count of hummingbird records
table(full.fl$Hummingbird)

full.fl$Hummingbird<-as.factor(full.fl$Hummingbird)

#Ignore blank cells
levels(full.fl$Hummingbird)[1]<-NA

#Fix typing errors
levels(full.fl$Hummingbird)[levels(full.fl$Hummingbird) %in% "Booted Rackettail"]<-"Booted Racket-tail"
levels(full.fl$Hummingbird)[levels(full.fl$Hummingbird) %in% "Booted Racketail"]<-"Booted Racket-tail"
levels(full.fl$Hummingbird)[levels(full.fl$Hummingbird) %in% "White-whskered hermit"]<-"White-whiskered Hermit"
levels(full.fl$Hummingbird)[levels(full.fl$Hummingbird) %in% "Gorgeted Sunangel"]<-"Gorgetted Sunangel"
levels(full.fl$Hummingbird)[levels(full.fl$Hummingbird) %in% "Violet-tailed Slyph"]<-"Violet-tailed Sylph"


levels(full.fl$Hummingbird)

#Hummingbirds on which flowers
G.hum<-melt(table(full.fl$Full,full.fl$Hummingbird))
colnames(G.hum)<-c("Flower","Hummingbird","Observation")
G.hum[G.hum$Observation==0,"Observation"]<-NA


############################################
#Bring in Holger's transect hummingbird data
############################################

#Create full name flower column
holger.hum$Family<-as.factor(sapply(holger.hum$Family,function(x) .simpleCap(as.character(x))))
holger.hum$Genus<-as.factor(sapply(holger.hum$Genus,function(x) .simpleCap(as.character(x))))
holger.hum$Species<-as.factor(sapply(holger.hum$Species,tolower))
holger.hum$Full<-with(holger.hum,paste(Family,Genus,Species))

#Check hummingbird levels

head(holger.hum)
levels(holger.hum$Hummingbird.Species)[levels(holger.hum$Hummingbird.Species) %in% "Booted Racketail"] <- "Booted Racket-tail"

G.hhum<-melt(table(holger.hum$Full,holger.hum$Hummingbird))
colnames(G.hhum)<-c("Flower","Hummingbird","Observation")

#remove the empty observations
G.hhum[G.hhum$Observation==0,"Observation"]<-NA


#merge dataframes together
flwr_bird<-merge(G.hhum,G.hum,all=TRUE)

#remove the NA and non flower rows
flwr_bird<-flwr_bird[!is.na(flwr_bird$Observation),]

flwr_bird<-flwr_bird[!flwr_bird$Flower %in% levels(flwr_bird$Flower)[[1]],]

write.csv(flwr_bird,"Thesis/Maquipucuna_SantaLucia/Results/TransectBird_Flower.csv")

GH<-ggplot(flwr_bird,aes(x=Flower,y=Hummingbird,fill=Observation)) + geom_tile() + theme_bw() + scale_fill_continuous(high="red",na.value="white")
GH + theme(axis.text.x=element_text(angle=90))

#It actually seems like it would make most sense to export the data as rows rather as a contingency table
write.csv(full.fl[is.finite(full.fl$Hummingbird),],"Thesis/Maquipucuna_SantaLucia/Results/TransectHumRows.csv")


ggsave(filename="Thesis/Maquipucuna_SantaLucia/Results/Hummingbird_Genus.jpeg",height=8,width=11)

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

#spatially?
ggplot(fl.elev,aes(x=coords.x1,y=coords.x2,col=Total_Flowers),size=200) + theme_bw() + geom_point() + scale_color_continuous(high="red",low="blue",limits=c(0,200))

#Bring in nectar data
Nectar <- read.csv("Thesis/Maquipucuna_SantaLucia/Data2013/csv/Nectar.csv")

#Fix colnames that are ugly
colnames(Nectar)[c(8,10,11,12,13)]<-c("Height","TubeLength","Brix","EffectiveCorolla","TotalCorolla")

#Data Cleaning
#All family and Genus should be capitalized
Nectar$Family<-as.factor(sapply(Nectar$Family,function(x) .simpleCap(as.character(x))))
Nectar$Genus<-as.factor(sapply(Nectar$Genus,function(x) .simpleCap(as.character(x))))

#species should be lowercase
Nectar$Species<-as.factor(sapply(Nectar$Species,function(x) tolower(as.character(x))))

#Create full name column
Nectar$Full<-paste(Nectar$Family,Nectar$Genus,Nectar$Species)

#Some basic visualizations to check data clarity
#number of records per species
m.Nectar<-melt(table(Nectar[!is.na(Nectar$Brix),]$Full))

#No records should be 0? turn these to NA
Nectar[Nectar$Brix==0 & is.finite(Nectar$Brix),]

#Is the tube given in diameter?
#tube column needs to have correct math.

as.numeric(Nectar$Tube.Type)/2 * 2*pi * Nectar$TubeLength

ggplot(m.Nectar,aes(x=Var.1,value)) + geom_bar() + coord_flip() + geom_text(aes(label=value),col="red",hjust=1) + theme_bw()

p<-ggplot(Nectar[!is.na(Nectar$Brix),],aes(x=Species,y=Brix)) + geom_point() + facet_wrap(~Family,scales="free_x")
p+ theme_bw() +theme(axis.text.x = element_text(angle = 90,size=10))

p<-ggplot(Nectar[!is.na(Nectar$TotalCorolla),],aes(x=Species,y=TotalCorolla)) + geom_point() + facet_wrap(~Family,scales="free_x")
p+ theme_bw() +theme(axis.text.x = element_text(angle = 90,size=10)) + geom_point()

ggplot(Nectar,aes(x=TotalCorolla,y=Brix)) + geom_point(aes(color=Family)) + stat_smooth(method="lm") + xlim(0,60)
                    
ggplot(Nectar,aes(x=EffectiveCorolla,y=Brix)) + geom_point(aes(color=Family)) + stat_smooth(method="lm") + geom_text(aes(label=Family))

ggplot(Nectar,aes(x=Corolla.Width,y=Brix)) + geom_point(aes(color=Family)) + stat_smooth(method="lm") + xlim(0,14)

#combine the elevation flower transects with Nectar data 

#First take the average nectar reading by species
Nectar.mean<-aggregate(Nectar,list(Nectar$Full),mean,na.rm=TRUE)[,c("Group.1","EffectiveCorolla","TotalCorolla","Corolla.Width","Brix")]
fl.nectar<-merge(fl.elev,Nectar.mean,by.x="Full",by.y="Group.1")

#how many transect rows didn't match nectar rows
missing<-fl.elev[!fl.elev$Full %in% Nectar.mean$Group.1,]
dim(missing)
table(missing$Full)

#okay, before we go crazy with nectar,lets look at corolla length with elevation
ggplot(fl.nectar,aes(x=ele,y=TotalCorolla,col=Family)) + geom_point(position="jitter") + stat_smooth(aes(group=1),method='lm') 

#Nectar concentration and elevation, there are some odd records that need to be fixed
ggplot(fl.nectar,aes(x=ele,y=Brix,col=Family)) + geom_point() + stat_smooth(aes(group=1),method='lm') + ylim(5,30)

#create volume calculations
save.image("Thesis/Maquipucuna_SantaLucia/Results/FlowerTransect.Rdata")

#Flower bloomign through time
head(fl.nectar)

#Set holger as observer
fl.nectar$Observer<-as.character(fl.nectar$Observer)

fl.nectar$Observer[is.na(fl.nectar$Observer)]<-"Holger"

#Create month column
fl.nectar$month<-NA
for (j in 1:nrow(fl.nectar)){
  x<-fl.nectar[j,]
  if(x[["Observer"]] %in% c("Karen","Ben")){
    fl.nectar[j,"month"]<-months(chron(as.character(x$Date)))
  }
    if(x[["Observer"]] %in% "Holger"){
      fl.nectar[j,"month"]<-months(chron(as.character(x[x$Observer %in% "Holger","Date"]),format="d/m/y"))
    }
  }


#plot elevation and nectar
ggplot(fl.nectar,aes(month,Total_Flowers),groups=1) + geom_point() + geom_smooth()

#Create interaction matrix for hummingbirds and flowers
