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
require(rPlant)
require(maptools)

#############################
#Set Dropbox Location
#Read in flower videos
#droppath<-"C:/Users/Jorge/Dropbox/"
#setwd(droppath)

#Set Gitpath
#gitpath<-"C:/Users/Jorge/Documents/Maquipicuna/"

#Where are the outputs?
netPath<-paste(droppath,"Thesis/Maquipucuna_SantaLucia/Results/Network/",sep="")

#############################
#Load image for convienance
#load("Thesis/Maquipucuna_SantaLucia/Results/Network/NetworkData.Rdata")

#bring in clade data
clades<-read.csv(paste(gitpath,"InputData//CladeList.txt",sep=""),header=FALSE)[,-1]
colnames(clades)<-c("Clade","Genus","Species","double","English")
clades<-clades[,1:5]

###################
#Source Functions
###################
source(paste(gitpath,"NetworkSource.R",sep=""))

#############
#Read in Data
#############

#Read in Flower Camera Dataset
dat<-read.csv("Thesis/Maquipucuna_SantaLucia/Data2013/csv/FlowerVideo.csv")

#Read in 
#If this has not been created see HummingbirdTransects.R

####Bring in interaction matrix for the flower transects, see FlowerTransects.R
transect.FL<-read.csv(paste(droppath,"Thesis/Maquipucuna_SantaLucia/Results/HummingbirdTransects/HumTransectRows.csv",sep=""))[,-1]

#make the columns as similiar as possible to videodata
colnames(transect.FL)<-c("TransectID","Hummingbird","ID","Flower","Date","Month","Transect_R")

#Bring in the phylogeny
#Read in phylogeny
tree<-read.nexus(paste(gitpath,"InputData/ColombiaPhylogenyUM.tre",sep=""))

#Read in names file to replace names in Nexis file
spnames<-read.table(paste(gitpath,"InputData/SpNameTree.txt",sep="") , sep = "\t", header = TRUE)

#Replace tip.label with Spnames#
tree$tip.label<-as.character(spnames$SpName) 
head(dat)

#Read in trait distance between species, run from Morphology.R
sp.dist<-read.csv("Thesis/Maquipucuna_SantaLucia/Results/HummingbirdDist.csv",row.names=1)

#Fix date format
dat$Month<-as.numeric(format(as.Date(dat$Date,"%m/%d/%Y"),"%m"))

#Bind in the transect rows to the bottom of dat?
dat<-rbind.fill(dat,transect.FL)

####################################
#GPS Info
####################################
#Read in GPS and round to nearest 10m
gps<-readShapePoints("Thesis\\Maquipucuna_SantaLucia\\Data2013\\Shapefiles\\GPSshape.shp")
gps$ele<-round(as.numeric(as.character(gps$ele)),-1)

####Combine with GPS info seperately, due to weird IDs of old GPS, non-unique
#Ben's GPS Summer 2013 had no dates

#Create month ID column in the GPS data
gps$MonthID<-sapply(gps$time,function(x){
  b<-strsplit(as.character(x),"T")[[1]][1]
  if(is.na(b)){
    return("S")
  }
  return(as.numeric(format(as.POSIXlt(b),"%m")))
})

#Get levels from summer for Ben's GPS
BenSummer<- gps[is.na(gps$time),]
BenSummerpts<-dat[dat$Month %in% c(6,7,8),]

#Merge first set of gps points
dat_part1<-merge(BenSummerpts,BenSummer,by.x="ID",by.y="name")

#Any missing levels, look up on the other GPS, probably taken by Karen or Anusha
missingGPS<-levels(factor(BenSummerpts$ID))[!levels(factor(BenSummerpts$ID)) %in% BenSummer$name]

#Combine missing levels
found_Summer<-merge(BenSummerpts[BenSummerpts$ID %in% missingGPS,],gps,by.x="ID",by.y="name")
print(paste("Ben Missing Levels:",missingGPS[!missingGPS %in% gps$name]))

#Combine holger points
#get the records that are not summer ("S") or 6 7 8 == monthID
gpsH<-gps[!gps$MonthID %in% c("S","6","7","8"),]
Holgerpts<-dat[!dat$Month %in% c(6,7,8),]

#paste the month and ID together, assuming there are no overlapping IDs in a month
gpsH$HolgerID<-paste(gpsH$MonthID,gpsH$name,sep="_")

#paste month and ID together in the destination dat
Holgerpts$HolgerID<-paste(Holgerpts$Month,Holgerpts$ID,sep="_")

#Which dont levels match from Holger's GPS
paste("Missing Holger Levels",levels(factor(Holgerpts[!Holgerpts$HolgerID %in% gpsH$HolgerID,]$HolgerID)))
HolgerMatch<-merge(Holgerpts,gpsH,by="HolgerID")

#Looks like holger added an additional space?
holgerMissing<-levels(factor(Holgerpts[!Holgerpts$ID %in% gpsH$name,]$ID))
HolgerMissingpts<-Holgerpts[Holgerpts$ID %in% holgerMissing,]
HolgerMissingpts$ID_N<-paste("0",HolgerMissingpts$ID,sep="")
HolgerMissingpts$HolgerID<-paste(HolgerMissingpts$Month,HolgerMissingpts$ID_N,sep="_")

#remerge
paste("Still Missing Holger Levels",levels(factor(HolgerMissingpts[!HolgerMissingpts$HolgerID %in% gpsH$HolgerID,]$HolgerID)))
HolgerMatchMiss<-merge(HolgerMissingpts,gpsH,by="HolgerID")

#Bind all levels together
dat_GPS<-rbind(dat_part1,found_Summer)
datelev<-rbind.fill(dat_GPS,HolgerMatch)
datf<-rbind.fill(datelev,HolgerMatchMiss)

#Okay what am i still missing
finalMissing<-levels(factor(dat[!dat$ID %in% datf$ID,]$ID))

print(paste(finalMissing,"Final Missing Elevation"))

#Add the remaining levels with NA elevation until they can be corroborated?
dat_e<-rbind.fill(datf,dat[dat$ID %in% finalMissing,])

#humans can only be in one place at once, this should look like a step function
#ggplot(dat_e,aes(x=Date,y=ele)) + geom_point() + geom_line() + coord_flip()

###### Add in manual elev branches for missing levels?

dat_e[dat_e$ID %in% "FL066","ele"]<-1350
dat_e[dat_e$ID %in% "FL084","ele"]<-1850
dat_e[dat_e$ID %in% "FL049","ele"]<-1350
dat_e[dat_e$ID %in% "FL050","ele"]<-1600
dat_e[dat_e$ID %in% "FL053","ele"]<-1550
dat_e[dat_e$ID %in% "FL054","ele"]<-1500
dat_e[dat_e$ID %in% "FL066","ele"]<-1550

#For the holger points, take the mean of the above and below point, can't have walked very far.

for (x in c("864","887","868","892","898","899","901","930")){
  tr<-dat_e[dat_e$ID %in% x,"Transect_R"]
  el<-mean(as.numeric(strsplit(as.character(tr),split="_")[[1]]))
  dat_e[dat_e$ID %in% x,"ele"]<-el
}

dat_e[is.na(dat_e$ele),]

###########################
#Hummingbird Data Cleaning 
###########################

#Caps Hummingbird
dat_e$Hummingbird<-factor(sapply(dat_e$Hummingbird,function(x) {.simpleCap(as.character(x))}))

#make a object, just to save typing
h<-levels(dat_e$Hummingbird)

#can taxize do english names? 

#Fix common mistakes
h[h %in% "Fawn Breasted Brilliant"] <- "Fawn-breasted Brilliant"
h[h %in% "Gorgetted Sunangel"]<-"Gorgetted Sunangel"
h[h %in% "Violet-tailed Slyph"]<-"Violet-tailed Sylph"
h[h %in% "Booted Racketail"]<-"Booted Racket-tail"
h[h %in% "Green-crowned Woodnymph"]<-"Crowned Woodnymph" 

levels(dat_e$Hummingbird) <- h

#Take our any bad data
dat_e<-droplevels(dat_e[!dat_e$Hummingbird %in% c("","NANA","UKWN","Ukwn"),])

#Remove out piercing events for now?
table(dat_e$Piercing)
datPierce<-dat_e[dat_e$Piercing %in% c("Yes","YES"),]
dat_e<-dat_e[!dat_e$Piercing %in% c("Yes","YES"),]

#Lots of cleaning left to do, but that's a start. 
#Final levels
print(paste("Final Flower Species:", levels(factor(dat_e$Iplant_Double))))

#How many Birds Species
print(paste("Number of Hummingbird Species:",nlevels(dat_e$Hummingbird)))
print(paste("Final Hummingbird Species:",levels(dat_e$Hummingbird)))

write.csv(dat_e,"Thesis/Maquipucuna_SantaLucia/Results/Network/HummingbirdInteractions.csv")

############################################
#Run Network Function for the entire dataset
NetworkC(datf=dat_e,naming="Total")
############################################

############################################
#Run Network Function for the entire dataset, with elev seperatiton?
dat.split<-split(dat_e,cut(dat_e$ele,breaks=c(1300,1700,2500),dig.lab=4,labels=c("Total_Low","Total_High")),drop=TRUE)

for (x in 1:length(dat.split)){
  NetworkC(datf=dat.split[[x]],naming=names(dat.split)[[x]])
}
############################################

####################################################
#Temporal Change in Network Structure
####################################################
dat.split<-split(dat_e,dat$Month,drop=TRUE)

##############################################
#Compute metrics for each month
##############################################

for (x in 1:length(dat.split)){
  NetworkC(datf=dat.split[[x]],naming=names(dat.split)[[x]])
}

##################################
#Retrieve Classes, name them, melt 
#Start with networkwide properties
##################################

#Get the desired files from paths
fil.list<-list.files(netPath,pattern="NetworkProperties.csv",recursive=TRUE,full.names=TRUE)

fil<-list()
#Read and name each file
for (x in 1:length(fil.list)){
  fil[[x]]<-read.csv(fil.list[[x]])
  names(fil)[x]<-strsplit(fil.list[[x]],"/")[[1]][10]
}

#melt the outputs to a single dataframe
m.Prop<-melt(fil)
colnames(m.Prop)<-c("Metric","Level","value","Time")

#Correct the naming of levels
levels(m.Prop$Level)<-c("Hummingbirds","Plants")

#If you want to remove overall metrics
month.Prop<-m.Prop[!m.Prop$Time=="Total",]

#For each metric plot them with time
dir.create(paste(netPath,"TimeFigures",sep=""))
setwd(paste(netPath,"TimeFigures",sep=""))

#Which metrics are desired?
droplevels(month.Prop)

metricskeep<-c("connectance","links per species","nestedness","Shannon diversity","H2","niche overlap","robustness.HL","number of compartments","robustness.LL","number.of.species.HL")

month.Prop<-droplevels(month.Prop[month.Prop$Metric %in% metricskeep,])
#Quick and dirty look at all metrics
p<-ggplot(na.omit(month.Prop),aes(x=as.numeric(Time),y=value,col=Level)) + geom_point() + geom_line(linetype="dashed",aes(group=Level)) + facet_wrap(~Metric,scales="free_y")
p + theme_bw() 
ggsave("MetricsFacet.svg",height=8,width=11)

dir.create("Metric_TimePlots")
setwd("Metric_TimePlots")

#Individual plots
for(x in levels(month.Prop$Metric)) {
  print(x)
  toplot<-na.omit(month.Prop[month.Prop$Metric %in% x,])
  
  #If there are no records, go to next metric
  if(nrow(toplot)==0) next
  
  #Plot and Save
  p<-ggplot(toplot,aes(x=as.numeric(Time),y=value,col=Level)) + geom_point() + geom_line(linetype="dashed",aes(group=Level))
  p + theme_bw() + ylab(x)
  ggsave(paste(x,".svg",sep=""),height=8,width=11)
}

##############################################
#Compute Metrics for each Humminbird species
##############################################

#Get the desired files from paths
fil.list<-list.files(netPath,pattern="HummingbirdMetrics.csv",recursive=TRUE,full.names=TRUE)

fil<-list()
#Read and name each file
for (x in 1:length(fil.list)){
  fil[[x]]<-read.csv(fil.list[[x]])
  names(fil)[x]<-strsplit(fil.list[[x]],"/")[[1]][10]
}

Hum.Time<-melt(fil)
colnames(Hum.Time)<-c("Species","Metric","value","Time")

#Just get the Metrics which make sense for this analysis
head(Hum.Time)

metricskeep<-c("nestedrank","resource.range","betweenness","d","degree","species.strength")
  Hum.Time<-droplevels(Hum.Time[Hum.Time$Metric %in% metricskeep ,])

#Probably should exclude rare species?
H.c<-cast(Hum.Time,...~Metric)
Hum.Time<-melt(H.c[H.c$degree > 2,])

#Quick and dirty look across species 
ggplot(Hum.Time,aes(as.numeric(Time),value,col=Species)) + facet_wrap(~Metric,scales="free") + geom_line(linetype="dashed",aes(group=Species)) + geom_point() + theme_bw()
ggsave(paste(netPath,"TimeFigures/HumSpecies_Time.svg",sep=""),height=8,width=11)

#Plot for each species, or for each metric?
for(x in levels(droplevels(Hum.Time$Species))){
  print(x)
  if(nrow(Hum.Time[Hum.Time$Species %in% x & !Hum.Time$Time %in% "Total",])==0) next
  
  #drop the total column and added a dashed total line
  p<-ggplot(Hum.Time[Hum.Time$Species %in% x & !Hum.Time$Time %in% "Total",],aes(as.numeric(Time),value)) + facet_wrap(~Metric,scales="free") + geom_line(linetype="dashed",aes(group=Species)) + geom_point() + theme_bw()
  ggsave(paste(netPath,paste(x,".svg",sep=""),sep="TimeFigures/"),height=8,width=11) 

}

#######################################################################
#Bring in Transect Data and Compare Specilization and available resources across all Elevations
#######################################################################
#At first just use the across entire network properies and all flowers from all elevations
#The next step is to set species ranges and get flower transects across that range

#The script FlowerTransects.R must be run
#source(paste(gitpath),"FlowerTransects.R")

####################
#Network Properties
head(month.Prop)
####################

########################################################
#For now there is a bit of a mismatch, since the network 
#is not split by elevation, but the flowers are aggregated into 200m bins
#########################################################

setwd(droppath)
load("Thesis/Maquipucuna_SantaLucia/Results/FlowerTransect.Rdata")

#The aggregate totals of the flower assemblage
head(fl.totals)

#aggregate by month for now, not elev split
month.totals<-aggregate(fl.totals$TotalFlowers,list(fl.totals$Month),sum)
colnames(month.totals)<-c("Month","Flowers")

#Start with just hummingbird levels
month.Hum<-month.Prop[month.Prop$Level == "Hummingbirds",]

#combine the flower totals and network metrics
network.fl<-merge(month.totals,month.Hum,by.x="Month",by.y="Time")

#Quick visualization
ggplot(network.fl,aes(Flowers,value,col=as.factor(Month))) + facet_wrap(~Metric,scale="free") + geom_point() + geom_smooth(method="lm",aes(group=1))
ggsave(paste(netPath,"NetworkProp_Flowers.svg",sep=""),height=8,width=11,dpi=300)

###############################################
#Hummingbird Properties and Available Resources
###############################################
head(Hum.Time)

#Take out the total time
Hum.Time<-Hum.Time[!Hum.Time$Time %in% "Total",]
hum.fl<-merge(month.totals,Hum.Time,by.x="Month",by.y="Time")

#Need to subset by number of interactions, get rid of the species just seen once?
with(hum.fl,table(Species,Month))
month_Pres<-aggregate(hum.fl$Month,list(hum.fl$Species),function(x) nlevels(factor(x)))

#Keep species seen more than 1 month
species_keep<-month_Pres[which(month_Pres$x > 1),]$Group.1

#remove an unknwon species
species_keep<-species_keep[!species_keep %in% "UKWN"]
ggplot(hum.fl[hum.fl$Species %in% species_keep,],aes(as.numeric(Flowers),value,col=as.factor(Month))) + facet_grid(Metric~Species,scale="free") + geom_point() + geom_smooth(method="lm",aes(group=1))
ggsave(paste(netPath,"SpeciesProp_Flowers.svg",sep=""),height=8,width=11,dpi=300)

#Save image to file
setwd(droppath)
save.image("Thesis/Maquipucuna_SantaLucia/Results/Network/NetworkData.Rdata")