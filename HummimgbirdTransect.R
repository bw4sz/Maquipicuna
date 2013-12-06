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

#Read in Ben's transect data
Hum<-read.csv("Thesis/Maquipucuna_SantaLucia/Data2013/csv/HummingbirdTransect.csv")
head(Hum)

#Bring in holger's hummingbird datasheet.
holger.hum<-read.csv("Thesis/Maquipucuna_SantaLucia/Data2013/csv/HolgerTransect_Hummingbirds.csv")
head(holger.hum)

#Bring in holger transect data
holgerID<-read.csv("Thesis/Maquipucuna_SantaLucia/Data2013/csv/TransectIIDHolger.csv")

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

levels(droplevels(holger.hum$ID[!holger.hum$ID %in% holgerID$ID]))
holger.full<-merge(holger.hum,holgerID,"ID")

#Get all the rows in holger.hum that have species
holger.full$Full<-paste(holger.full$Family,holger.full$Genus,holger.full$Species)
holgerInter<-holger.full[!holger.full$Full %in% "  " ,]

#get the desired columns
colnames(holgerInter)

holgerInter<-holgerInter[,colnames(holgerInter) %in% c("ID","Transect.x","Family","Genus","Species","Hummingbird.Species","Full","Way.Point","Month")]

############################################################################# Data cleaning complete for flowers
#Data Cleaning
#check the spelling and factor levels
monthInter<-melt(table(holgerInter$Hummingbird.Species,holgerInter$Full,holgerInter$Month))
colnames(monthInter)<-c("Hummingbird","Plant","Month","value")

#remove no interactions
monthInter<-monthInter[!monthInter$value==0,]

p<-ggplot(monthInter,aes(Hummingbird,Plant,fill=value)) + geom_tile() + facet_wrap(~Month) + scale_fill_continuous(na.value="White",high="red") + theme_bw()
p<- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
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
Hummingbird.Species<-sapply(holger.full$Hummingbird.Species,function(x){
  strsplit(as.character(x),",")[1]
})

#There are still some with double entries?
#Needs to be addressed within the data

holger.full$Hummingbird.Species<-droplevels(as.factor(holger.full$Hummingbird.Species))

#Count of hummingbird records
table(holger.full$Hummingbird.Species)

#Ignore blank cells
levels(holger.full$Hummingbird)[1]<-NA

#Fix typing errors
levels(holger.full$Hummingbird)[levels(holger.full$Hummingbird) %in% "Booted Rackettail"]<-"Booted Racket-tail"
levels(holger.full$Hummingbird)[levels(holger.full$Hummingbird) %in% "Booted Racketail"]<-"Booted Racket-tail"
levels(holger.full$Hummingbird)[levels(holger.full$Hummingbird) %in% "White-whskered hermit"]<-"White-whiskered Hermit"
levels(holger.full$Hummingbird)[levels(holger.full$Hummingbird) %in% "Gorgeted Sunangel"]<-"Gorgetted Sunangel"
levels(holger.full$Hummingbird)[levels(holger.full$Hummingbird) %in% "Violet-tailed Slyph"]<-"Violet-tailed Sylph"

levels(holger.full$Hummingbird)

#Hummingbirds on which flowers
G.hum<-melt(table(holger.full$Full,holger.full$Hummingbird))
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
write.csv(holger.full[is.finite(holger.full$Hummingbird),],"Thesis/Maquipucuna_SantaLucia/Results/TransectHumRows.csv")


ggsave(filename="Thesis/Maquipucuna_SantaLucia/Results/Hummingbird_Genus.jpeg",height=8,width=11)
