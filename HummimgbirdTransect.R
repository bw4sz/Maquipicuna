################
#Hummingbird Transect Data
################

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
setwd("C:/Users/Jorge/Dropbox/")

#Read in workspace if desired for quick access
#load("Thesis/Maquipucuna_SantaLucia/Results/HummingbirdTransect.Rdata")

#Read in Flower Transect Data from summer field season
fl<-read.csv(file="Thesis/Maquipucuna_SantaLucia/Data2013/csv/FlowerTransects.csv")

#Read in Ben's transect data
Hum<-read.csv("Thesis/Maquipucuna_SantaLucia/Data2013/csv/HummingbirdTransect.csv")
head(Hum)

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

#Data Cleaning
#check the spelling and factor levels
table(Hum$Hummingbird.Species)

#fix the unknown upper
levels(Hum$Hummingbird.Species)[levels(Hum$Hummingbird.Species) %in% "UkWN"]<-toupper(levels(Hum$Hummingbird.Species)[levels(Hum$Hummingbird.Species) %in% "UkWN"])

head(Hum)

# Fix the caps at the plant species
table(Hum$Plant.Species)

levels(Hum$Plant.Species)[levels(Hum$Plant.Species) %in% ""]<-NA

table(Hum$Plant.Species)

#Fix the height column, turn back from factors
ggplot(Hum,aes(x=Hummingbird.Species,y=as.numeric(Height))) + geom_boxplot() + coord_flip()

#Bring the transect data
TID<-read.csv("TransectIID.csv")

#Select the flower transects
TID.f<-TID[TID$Type %in% "Hummingbird",]
TID$TransectID<-as.factor(TID$TransectID)
Hum$ID
hum.id<-merge(Hum,TID.f,by.x="ID",by.y="TransectID")

hum.id$Range<-paste(hum.id$Elevation.Begin,hum.id$Elevation.End,sep="_")
#Get rid of the NA
hum.id<-hum.id[!hum.id$Range %in% "NA_NA",]
#Overal range presence absence
ggplot(hum.id,aes(x=Range,Hummingbird.Species)) + geom_line(aes(group=Hummingbird.Species)) + geom_point() 

#Broken up by replicate
ggplot(hum.id,aes(x=Range,Hummingbird.Species)) + geom_line(aes(group=Hummingbird.Species)) + geom_point() + facet_wrap(~Replicate.Number)


################################################
#Data cleaning on hummingbirds
################################################
#Needs to be fixed!
#It looks like there are two hummingbird species columns
Hummingbird.Species<-sapply(full.fl$Hummingbird.Species,function(x){
  strsplit(as.character(x),",")[1]
})

#There are still some with double entries?
#Needs to be addressed within the data

full.fl$Hummingbird.Species<-droplevels(as.factor(full.fl$Hummingbird.Species))


#Count of hummingbird records
table(full.fl$Hummingbird.Species)

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

holger.hum<-read.csv("C:/Users/Ben/Dropbox/Thesis//Maquipucuna_SantaLucia//Data2013//csv//HolgerTransect_Hummingbirds.csv")
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
