###############################################################################
#Hummingbird Flower Interactions at the Santa Lucia and Maquipucuna EcoReserves
###############################################################################
#Ben Weinstein - Stony Brook University - Department of Ecology and Evolution

require(chron)
require(bipartite)
require(ggplot2)
require(ape)
require(reshape)
require(sna)
require(stringr)
require(maptools)
require(taxize)
require(picante)
require(plyr)
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
colnames(transect.FL)<-c("GPS.ID","TransectID","Hummingbird","Date","Month","Transect_R","lat","lon","ele","Iplant_Double")

transect.FL$Iplant_Double<-gsub("_"," ",transect.FL$Iplant_Double)

#Bind in the transect rows to the bottom of dat?
dat<-rbind.fill(dat,transect.FL)

dat$Iplant_Double<-as.factor(dat$Iplant_Double)

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

dat$DateP<-as.POSIXlt(dat$DateP)

head(dat)

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
h[h %in% missp]

spellC<-c("Booted Racket-tail","Green-crowned Woodnymph","UKWN","UKWN","UKWN","Violet-tailed Sylph","White-lined Flowerpiercer","White-sided Flowerpiercer")

paste("Spelling Correction",spellC)

h[h %in% missp]<-spellC

head(clades)

#can taxize do english names? 
levels(dat$Hummingbird) <- h

#Take our any bad data
dat_e<-droplevels(dat[!dat$Hummingbird %in% c("","NANA","UKWN","Ukwn","Western Emerald"),])

#Remove out piercing events for now?
table(dat$Pierce)
datPierce<-dat_e[dat_e$Piercing %in% c("Yes","YES","y","Y"),]
dat_e<-dat_e[!dat_e$Pierce %in% c("Yes","YES","y","Y"),]

#remove species without double name

#Drop any unused factors?
dat_e<-droplevels(dat_e)

#Drop any observations without plants
dat_e<-droplevels(dat_e[!dat_e$Iplant_Double %in% "",])

table(dat_e$Iplant_Double)
sp_l<-levels(dat_e$Iplant_Double)

l<-sapply(sp_l,function(x){
  str2 <- gsub(' {2,}',' ',x)
  length(strsplit(str2,' ')[[1]])
})

sp_r<-names(which(l==1))

dat_e<-droplevels(dat_e[!dat_e$Iplant_Double %in% sp_r,])
#################Data Cleaning Complete################

#Final levels
#print(paste("Final Flower Species:", levels(factor(dat_e$Iplant_Double))))

#How many Birds Species
#print(paste("Number of Hummingbird Species:",nlevels(dat_e$Hummingbird)))
#print(paste("Final Hummingbird Species:",levels(dat_e$Hummingbird)))

write.csv(dat_e,"Thesis/Maquipucuna_SantaLucia/Results/Network/HummingbirdInteractions.csv")

#print("data cleaned")
