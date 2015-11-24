###############################################################################
#Hummingbird Flower Interactions at the Santa Lucia and Maquipucuna EcoReserves
###############################################################################
#Ben Weinstein - Stony Brook University - Department of Ecology and Evolution

library(chron)
library(bipartite)
library(ggplot2)
library(ape)
library(reshape)
library(sna)
library(stringr)
library(maptools)
library(taxize)
library(picante)
library(dplyr)
library(scales)

#Set Dropbox Location
#setwd to dropbox
droppath<-"C:/Users/Ben/Dropbox/"
setwd(droppath)
#Set github path
gitpath<-"C:/Users/Ben/Documents/Maquipicuna/"

#Read in data

#bring in clade data
clades<-read.csv(paste(gitpath,"InputData//CladeList.txt",sep=""),header=FALSE)[,-1]
colnames(clades)<-c("Clade","Genus","Species","double","English")
clades<-clades[,1:5]

#Bring in the hummingbird phylogeny
tree<-read.nexus(paste(gitpath,"InputData/ColombiaPhylogenyUM.tre",sep=""))

#Read in names file to replace names in Nexis file
spnames<-read.table(paste(gitpath,"InputData/SpNameTree.txt",sep="") , sep = "\t", header = TRUE)

#Replace tip.label with Spnames#
tree$tip.label<-as.character(spnames$SpName) 

#Read in trait distance between species, run from Morphology.R
sp.dist<-read.csv("Thesis/Maquipucuna_SantaLucia/Results/HummingbirdDist.csv",row.names=1)

#Read in plant phylogeny 
pco<-read.csv(paste(gitpath,"InputData/PlantRelatedness.csv",sep=""))

###Read in Flower Camera Dataset####
dat<-read.csv("Thesis/Maquipucuna_SantaLucia/Data2013/csv/FlowerVideoClean.csv",row.names=1)

#Get desired columns
#dat<-dat[,colnames(dat) %in% c("ID","Video","Date","Iplant_Double","Time","Hummingbird","Sex","Temp","Pierce","lon","lat","ele")]

#Fix date format
dat$Month<-as.numeric(format(as.Date(dat$Date,"%m/%d/%Y"),"%m"))

####Bring in interaction matrix for the flower transects, see FlowerTransects.R
transect.FL<-read.csv("Thesis/Maquipucuna_SantaLucia/Results/HummingbirdTransects/HumTransectRows.csv",row.names=1)

#make the columns as similiar as possible to videodata
colnames(transect.FL)<-c("GPS.ID","TransectID","Hummingbird","Date","Month","Transect_R","Iplant_Double","lat","lon","ele")

transect.FL$Iplant_Double<-gsub("_"," ",transect.FL$Iplant_Double)

#Bind in the transect rows to the bottom of dat?
dat<-rbind_all(list(dat,transect.FL))

dat$Iplant_Double<-as.factor(dat$Iplant_Double)

dat<-as.data.frame(dat)
############################################
#Create Universal Date Stamp

dat$DateP<-sapply(dat$Date,function(x){
  if(is.na(x)){
    return(NA)
  }
  if(str_detect(x,"-")){
    toR<-as.character(strptime(x,"%Y-%m-%d"))
    #print(toR)
    return(toR)
  }
  
  if(str_detect(x,"/")){
    toR<-as.character(strptime(x,format="%m/%d/%Y"))
    #print(toR)
    return(toR)
  }
})

#which are NA?
#Known date errors
dat[is.na(dat$DateP),]
dat$DateP[is.na(dat$DateP) & dat$ID=="FL080"]<-c("2013-07-25")

dat$DateP<-as.POSIXlt(dat$DateP)

head(dat)

#Mistakenly labeled 2012
dat[dat$Date %in% "6/4/2012","DateP"]<-"2013-06-04"

###########################
#Hummingbird Data Cleaning 
###########################
.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

#Caps Hummingbird
dat$Hummingbird<-factor(sapply(dat$Hummingbird,function(x) {.simpleCap(as.character(x))}))

#make a object, just to save typing
h<-levels(dat$Hummingbird)

missp<-h[!h %in% clades$English]

paste("misspelled levels",missp)

#correct levels
h[h %in% "Booted Racketail"]<-"Booted Racket-tail"
h[h %in% "Booted Racketail"]<-"Green-crowned Woodnymph"
h[h %in% "Violet-tailed Slyph"]<-"Violet-tailed Sylph"
h[h %in% "Ukwn"]<-"UKWN"

levels(dat$Hummingbird) <- h

#Take our any bad data
dat_e<-droplevels(dat[!dat$Hummingbird %in% c("","NANA","UKWN","Ukwn"),])

#Remove out piercing events for now?
table(dat$Pierce)
datPierce<-dat_e[dat_e$Piercing %in% c("Yes","YES","y","Y"),]
dat_e<-dat_e[!dat_e$Pierce %in% c("Yes","YES","y","Y"),]

#Drop any unused factors?
dat_e<-droplevels(dat_e)

#Drop any observations without plants
dat_e$Iplant_Double<-as.character(dat_e$Iplant_Double)
datinter<-dat_e[!dat_e$Iplant_Double %in% "",]


sp_l<-unique(datinter$Iplant_Double)

l<-sapply(sp_l,function(x){
  str2 <- gsub(' {2,}',' ',x)
  length(strsplit(str2,' ')[[1]])
})

sp_r<-names(which(l==1))


datinter<-droplevels(datinter[!datinter$Iplant_Double %in% sp_r,])
#################Data Cleaning Complete################

#make sure capitalize
Genus<-paste(toupper(substring(word(datinter$Iplant_Double),1,1)),substring(word(datinter$Iplant_Double),2),sep="")
Species<-tolower(word(datinter$Iplant_Double,2))
datinter$Iplant_Double<-paste(Genus,Species,sep=" ")

#Taxonomic changes
datinter[datinter$Iplant_Double %in% "Heliconia griggsianna","Iplant_Double"]<-"Heliconia griggsiana"

datinter[datinter$Iplant_Double=="Alloplectus purpureus","Iplant_Double"]<-"Glossoloma purpureum"
datinter[datinter$Iplant_Double=="Capanea affinis","Iplant_Double"]<-"Kohleria affinis"
datinter[datinter$Iplant_Double=="Columnea cinerea","Iplant_Double"]<-"Columnea mastersonii"
datinter[datinter$Iplant_Double=="Alloplectus teuscheri","Iplant_Double"]<-"Drymonia teuscheri"

datinter$Hummingbird<-as.character(datinter$Hummingbird)
datinter$Hummingbird[datinter$Hummingbird %in% "Green-crowned Woodnymph"]<-"Crowned Woodnymph"

#Final levels
print(paste("Final Flower Species:", levels(factor(datinter$Iplant_Double))))

#How many Birds Species
#print(paste("Number of Hummingbird Species:",nlevels(datinter$Hummingbird)))
#print(paste("Final Hummingbird Species:",levels(datinter$Hummingbird)))

#Known taxnomic errors:
datinter$Hummingbird[datinter$Hummingbird %in% "Green-crowned Woodnymph"]<-"Crowned Woodnymph"


write.csv(datinter,"Thesis/Maquipucuna_SantaLucia/Results/Network/HummingbirdInteractions.csv")

#print("data cleaned")
