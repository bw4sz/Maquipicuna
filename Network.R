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
require(taxize)
require(picante)
require(plyr)

#Set Dropbox Location
#setwd to dropbox
droppath<-"C:/Users/Ben/Dropbox/"
setwd(droppath)
#Set github path
gitpath<-"C:/Users/Ben/Documents/Maquipicuna/"

#Where are the outputs?
netPath<-paste(droppath,"Thesis/Maquipucuna_SantaLucia/Results/Network/",sep="")

###################
#Source Functions
###################

source(paste(gitpath,"NetworkSource.R",sep=""))

############################################
##############Read In Data##################
############################################

#Load image for convienance
#load("Thesis/Maquipucuna_SantaLucia/Results/Network/NetworkData.Rdata")

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
dat<-dat[,colnames(dat) %in% c("ID","Video","Date","Iplant_Double","Time","Hummingbird","Sex","Temp","Pierce","lon","lat","ele")]

#Fix date format
dat$Month<-as.numeric(format(as.Date(dat$Date,"%m/%d/%Y"),"%m"))

####Bring in interaction matrix for the flower transects, see FlowerTransects.R
transect.FL<-read.csv("Thesis/Maquipucuna_SantaLucia/Results/HummingbirdTransects/HumTransectRows.csv",row.names=1)

#make the columns as similiar as possible to videodata
colnames(transect.FL)<-c("GPS.ID","TransectID","Hummingbird","Date","Month","Transect_R","Iplant_Double","lat","lon","ele")

transect.FL$Iplant_Double<-gsub("_"," ",transect.FL$Iplant_Double)
##############Data Imported#####################

#Bind in the transect rows to the bottom of dat?
dat<-rbind.fill(dat,transect.FL)

dat$Iplant_Double<-as.factor(dat$Iplant_Double)
############################################
##############Data Import Complete##########
############################################

#Create Universal Date Stamp

dat$DateP<-sapply(dat$Date,function(x){
  #print(x)
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

spellC<-c("Booted Racket-tail","Green-crowned Woodnymph","Rufous-tailed Hummingbird","UKWN","UKWN","UKWN","Violet-tailed Sylph")

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

sp_l<-levels(dat_e$Iplant_Double)

l<-sapply(sp_l,function(x){
  str2 <- gsub(' {2,}',' ',x)
  length(strsplit(str2,' ')[[1]])
})

sp_r<-names(which(l==1))

dat_e<-droplevels(dat_e[!dat_e$Iplant_Double %in% sp_r,])
#################Data Cleaning Complete################

#Final levels
print(paste("Final Flower Species:", levels(factor(dat_e$Iplant_Double))))

#How many Birds Species
print(paste("Number of Hummingbird Species:",nlevels(dat_e$Hummingbird)))
print(paste("Final Hummingbird Species:",levels(dat_e$Hummingbird)))

write.csv(dat_e,"Thesis/Maquipucuna_SantaLucia/Results/Network/HummingbirdInteractions.csv")

print("data cleaned")

############################################
#Run Network Function for the entire dataset
NetworkC(datf=dat_e,naming="Total")
############################################

############################################
#Run Network Function for the entire dataset, with elev seperatiton?
dat.split<-split(dat_e,cut(dat_e$ele,breaks=c(1300,1700,2500),dig.lab=4,labels=c("Total_Low","Total_High")),drop=TRUE)

for (x in 1:length(dat.split)){
  NetworkC(datf=dat.split[[x]],naming=paste("Total",names(dat.split)[[x]],sep="/"))
}

#############################################
#Temporal Change in Network Structure
#############################################

dat.split<-split(dat_e,dat_e$Month,drop=TRUE)

#############################################
#Compute metrics for each month
#############################################

for (x in 1:length(dat.split)){
  NetworkC(datf=dat.split[[x]],naming=paste("Temporal",names(dat.split)[[x]],sep="/"))
}

#############################################
#Compute metrics for each month split by elev
#############################################
dat.split<-split(dat_e,list(dat_e$Month,cut(dat_e$ele,breaks=c(1300,1700,2500),dig.lab=4,labels=c("Low","High"))),drop=TRUE)

names(dat.split)<-gsub("\\.","/",names(dat.split))
for (x in 1:length(dat.split)){
  NetworkC(datf=dat.split[[x]],naming=paste("Temporal_Split",names(dat.split)[[x]],sep="/"))
}

############################################
##############Networks Created##############
############################################

##################################
#Retrieve Classes, name them, melt 
#Start with networkwide properties
##################################

#Get the desired files from paths - within time?
fil.list<-list.files(paste(netPath,"Temporal",sep="/"),pattern="NetworkProperties.csv",recursive=TRUE,full.names=TRUE)

fil<-list()
#Read and name each file
for (x in 1:length(fil.list)){
  fil[[x]]<-read.csv(fil.list[[x]])
  names(fil)[x]<-strsplit(fil.list[[x]],"/")[[1]][11]
}

#melt the outputs to a single dataframe
m.Prop<-melt(fil)
colnames(m.Prop)<-c("Metric","Level","value","Time")

#Correct the naming of levels
levels(m.Prop$Level)<-c("Hummingbirds","Plants")

#If you want to remove overall metrics
month.Prop<-m.Prop[!m.Prop$Time=="Total",]

#write csv to file
write.csv(month.Prop,"C:/Users/Ben/Dropbox/Thesis/Maquipucuna_SantaLucia/Results/Network/timeMetrics.csv")

#For each metric plot them with time
dir.create(paste(netPath,"TimeFigures",sep=""))
setwd(paste(netPath,"TimeFigures",sep=""))

#Which metrics are desired?
droplevels(month.Prop)

#metricskeep<-c("connectance","links per species","nestedness","Shannon diversity","H2","niche overlap","robustness.HL","number of compartments","robustness.LL","number.of.species.HL")
#month.Prop<-droplevels(month.Prop[month.Prop$Metric %in% metricskeep,])

#Quick and dirty look at all metrics

p<-ggplot(na.omit(month.Prop),aes(x=factor(Time),y=value,col=Level)) + geom_point() + geom_line(linetype="dashed",aes(group=Level)) + facet_wrap(~Metric,scales="free_y") 
p + theme_bw() 
ggsave("MetricsFacet.svg",height=8,width=11)

##Add a trend line across all months?



#be interested to add a "mean line" and then the value of the total network...


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
fil.list<-list.files(paste(netPath,"Temporal",sep="/"),pattern="HummingbirdMetrics.csv",recursive=TRUE,full.names=TRUE)

fil<-list()
#Read and name each file
for (x in 1:length(fil.list)){
  fil[[x]]<-read.csv(fil.list[[x]])
  names(fil)[x]<-strsplit(fil.list[[x]],"/")[[1]][11]
}

Hum.Time<-melt(fil)
colnames(Hum.Time)<-c("Species","Metric","value","Time")

#Just get the Metrics which make sense for this analysis
head(Hum.Time)

metricskeep<-c("nestedrank","resource.range","betweenness","d","degree","species.strength")
  Hum.Time<-droplevels(Hum.Time[Hum.Time$Metric %in% metricskeep ,])

#Probably should exclude rare species?
H.c<-cast(Hum.Time,...~Metric)
Hum.Time<-melt(H.c)

#Quick and dirty look across species 
ggplot(Hum.Time,aes(Time,value,col=Species)) + facet_wrap(~Metric,scales="free") + geom_line(linetype="dashed",aes(group=Species)) + geom_point() + theme_bw()
ggsave(paste(netPath,"TimeFigures/HumSpecies_Time.svg",sep=""),height=8,width=11)

#Plot for each species, or for each metric?
for(x in levels(droplevels(Hum.Time$Species))){
  print(x)
  if(nrow(Hum.Time[Hum.Time$Species %in% x & !Hum.Time$Time %in% "Total",])==0) next
  
  #drop the total column and added a dashed total line
  p<-ggplot(Hum.Time[Hum.Time$Species %in% x & !Hum.Time$Time %in% "Total",],aes(as.numeric(Time),value)) + facet_wrap(~Metric,scales="free") + geom_line(linetype="dashed",aes(group=Species)) + geom_point() + theme_bw()
  p
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

#setwd to dropbox
droppath<-"C:/Users/Ben/Dropbox/"
setwd(droppath)
#Set github path
gitpath<-"C:/Users/Ben/Documents/Maquipicuna/"

#The aggregate totals of the flower assemblage
head(fl.totals)

#aggregate by month for now, not elev split
month.totals<-aggregate(fl.totals$TotalFlowers,list(fl.totals$Month,fl.totals$Year),sum)
colnames(month.totals)<-c("Month","Year","Flowers")

#Start with just hummingbird levels
month.Hum<-month.Prop[month.Prop$Level == "Hummingbirds",]

#combine the flower totals and network metrics
network.fl<-merge(month.totals,month.Hum,by.x="Month",by.y="Time")

#write to file
write.csv(network.fl,"C:\\Users\\Ben\\Dropbox\\Thesis\\Maquipucuna_SantaLucia\\Results\\Network//networkflowers.csv")
#Quick visualization, get rid of some months for now
p<-ggplot(network.fl[,],aes(Flowers,value,shape=Year,col=as.factor(Month))) + facet_wrap(~Metric,scale="free") + geom_point(size=3) + geom_smooth(method="lm",aes(group=Year)) + theme_bw()
ggsave(paste(netPath,"NetworkPropFlowers.svg",sep=""),height=8,width=11,dpi=300)

p<-ggplot(network.fl[ network.fl$Metric %in% c("connectance","cluster coefficient"),],aes(Flowers,value,shape=Year,col=as.factor(Month))) + facet_wrap(~Metric,scale="free",nrow=2) + geom_point(size=3) + geom_smooth(method="lm",aes(group=Year)) + theme_bw() + labs(col="Month")
ggsave(paste(netPath,"NetworkConnectance.jpeg",sep=""),height=11,width=8,dpi=300) 

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
ggplot(hum.fl[hum.fl$Species %in% species_keep,],aes(as.numeric(Flowers),value,col=as.factor(Month))) + facet_wrap(~Metric,scale="free") + geom_point() + geom_smooth(method="lm",aes(group=1))
ggsave(paste(netPath,"SpeciesPropFlowers.svg",sep=""),height=8,width=11,dpi=300)

#Save image to file
setwd(droppath)
save.image("Thesis/Maquipucuna_SantaLucia/Results/Network/NetworkData.Rdata")