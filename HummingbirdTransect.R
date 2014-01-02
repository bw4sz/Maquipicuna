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
require(plyr)
require(plotKML)
require(reshape)
require(chron)

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
holgerInter<-holger.full[!holger.full$Full %in% "  " ,]

################
#Flower Taxonomy
################

#Go through a series of data cleaning steps, at the end remove all rows that are undesired
Families<-levels(factor(holgerInter$Family))
iplant_names<-ResolveNames(names=Families)
CompareNames(Families,iplant_names)

Fam_Result<-data.frame(Families,iplant_names)
Fam_Errors<-Fam_Result[Fam_Result$iplant_names %in% "","Families"]

#Post to output which plant families need to be address
print(paste(Fam_Errors,"not found in taxonomy database"))

#Repeat for genus
Genus<-levels(factor(holgerInter$Genus))
iplant_names<-ResolveNames(names=Genus)
CompareNames(Genus,iplant_names)

Genus_Result<-data.frame(Genus,iplant_names)
Genus_Errors<-Genus_Result[Genus_Result$iplant_names %in% "","Genus"]

#Post to output which plant families need to be address
print(paste(Genus_Errors,"not found in taxonomy database"))

#Set the Genus column
for (x in 1:nrow(holgerInter)){
  y<-holgerInter[x,]
  holgerInter[x,"Iplant_Genus"]<-levels(droplevels(Genus_Result[Genus_Result$Genus %in% y$Genus,"iplant_names"] ))   
}

#Repeat for species
Species<-levels(factor(paste(holgerInter$Iplant_Genus,holgerInter$Species,sep="_")))
iplant_names<-ResolveNames(Species)
print(CompareNames(Species,iplant_names))
Species_Result<-data.frame(Species,iplant_names)

#Set the Species column
for (x in 1:nrow(holgerInter)){
  y<-holgerInter[x,]
  toMatch<-paste(y$Iplant_Genus,y$Species,sep="_")
  holgerInter[x,"Iplant_Double"]<-levels(droplevels(
    Species_Result[Species_Result$Species %in% toMatch,"iplant_names"] ))   
}

#Lots of cleaning left to do, but that's a start. 

#Fix any known ID mistakes

holgerInter[holgerInter$Iplant_Double %in% "Heppiella_ulmifolia","Iplant_Double"]<-"Glossoloma_oblongicalyx"

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

#Select the flower transects
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

###Taxonomoy of plant names
#Repeat for genus
plants<-levels(factor(hum.id$Plant.Species))
iplant_names<-ResolveNames(names=plants)
CompareNames(plants,iplant_names)

Genus_Result<-data.frame(plants,iplant_names)
Genus_Errors<-Genus_Result[Genus_Result$iplant_names %in% "","Genus"]

#Post to output which plant families need to be address
print(paste(Genus_Errors,"not found in taxonomy database"))

#Set the plant column
for (x in 1:nrow(hum.id)){
  y<-hum.id[x,]
  hum.id[x,"Iplant_Double"]<-levels(droplevels(Genus_Result[Genus_Result$plants %in% y$Plant.Species,"iplant_names"] )) 
}

################################################
#Interaction table for summer data
################################################

#Cap all species names
levels(hum.id$Hummingbird.Species)<-sapply(levels(hum.id$Hummingbird.Species),function(x){.simpleCap(tolower(x))})
humInter<-melt(table(hum.id$Hummingbird.Species,hum.id$Iplant_Double,hum.id$Month))
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

#Okay, but it looks like the network functions just want the raw rows. 
#Get all the summer rows that have plants
humNetwork<-hum.id[!is.na(hum.id$Plant.Species),]
humNetwork<-humNetwork[,colnames(humNetwork) %in% c("ID","Plant.Species","Hummingbird.Species","Month","Date_F","Transect_R","GPS.ID")]

#match up desired columns with holger's data
colnames(humNetwork)
colnames(holgerInter)<-c("ID","Hummingbird.Species","GPS.ID","Transect_R","Date_F","Month","Plant.Species")

transectRows<-rbind(humNetwork,holgerInter)
write.csv(transectRows,"Thesis/Maquipucuna_SantaLucia/Results/HummingbirdTransects/HumTransectRows.csv")

#Return end of file
print("HummingbirdTransects")

save.image("Thesis/Maquipucuna_SantaLucia/Results/HummingbirdTransect.Rdata")
