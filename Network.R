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

#############################
#Set Dropbox Location
#Read in flower videos
#droppath<-"C:/Users/Ben/Dropbox/"
setwd(droppath)

#Set Gitpath
#gitpath<-"C:/Users/Ben/Documents/Maquipicuna/"

#Where are the outputs?
netPath<-paste(droppath,"Thesis/Maquipucuna_SantaLucia/Results/Network/",sep="")

#############################
#Load image for convienance
#load("Thesis/Maquipucuna_SantaLucia/Results/Network/NetworkData.Rdata")

##############
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

#Read in Transect Dataset, needs to run from the HummingbirdTransect.R script
humT<-read.csv("Thesis/Maquipucuna_SantaLucia/Results/HumTransectMatrix.csv",row.names=1)

#Bring in the phylogeny
#Read in phylogeny
tree<-read.nexus("Shared Ben and Catherine/DimDivEntire/Files for Analysis/ColombiaPhylogenyUM.tre")

#Read in names file to replace names in Nexis file
spnames<-read.table(file="Shared Ben and Catherine/DimDivEntire/Files for Analysis/SpNameTree.txt" , sep = "\t", header = TRUE)

#Replace tip.label with Spnames#
tree$tip.label<-as.character(spnames$SpName) 
head(dat)

#For the sake of simplicity, make everything lowercase
.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

#bring in clade data
clades<-read.csv("Shared Ben and Catherine/DimDivEntire/Files for Analysis/CladeList.txt",header=FALSE)[,-1]
colnames(clades)<-c("Clade","Genus","Species","double","English")
clades<-clades[,1:5]

#Bring in trait data
###Bring in trait data
morph <- read.csv(paste(gitpath,"//InputData//MorphologyShort.csv",sep=""),na.strings="9999")

#just get males
morph.male<-morph[morph$Sex=="Macho",c("SpID","ExpC","Peso","AlCdo")]
morph.complete<-morph.male[complete.cases(morph.male),]

#aggregate for species
agg.morph<-aggregate(morph.complete,list(morph.complete$SpID),mean)
mon<-agg.morph[,-2]
colnames(mon)<-c("Species","Bill","Mass","WingChord")
rownames(mon)<-gsub(" ",".",mon[,1])
mon<-mon[,-1]

#principal component traits and get euclidean distance matrix
means <- apply(mon, 2, mean)
Bill <- mon$Bill - means["Bill"]/sd(mon$Bill)
Mass <- mon$Mass - means["Mass"]/sd(mon$Mass)
WingChord <- (mon$WingChord - means["WingChord"])/sd(mon$WingChord)
z.scores <- data.frame(Bill, Mass, WingChord)
rownames(z.scores) <- rownames(mon)
trait_pc <- as.matrix(dist(z.scores, method = "euclidean"))

####Bring in interaction matrix for the flower transects, see FlowerTransects.R
transect.FL<-read.csv(paste(droppath,"Thesis/Maquipucuna_SantaLucia/Results/HummingbirdTransects/HumTransectRows.csv",sep=""))[,-1]

#make the columns as similiar as possible to videodata
colnames(transect.FL)<-c("TransectID","Hummingbird","ID","Flower","Date","Month","Transect_R")

#Fix date format
dat$Month<-as.numeric(format(as.Date(dat$Date,"%m/%d/%Y"),"%m"))

#Bind in the transect rows to the bottom of dat?
dat<-rbind.fill(dat,transect.FL)
####################################################
#Analysis of Flower Usage for each Hummingbird Species
####################################################

##############
#Data Cleaning 
##############

#Capitalize Flowers
dat$Flower<-factor(sapply(dat$Flower,function(x) {.simpleCap(as.character(x))}))

#Caps Hummingbird
dat$Hummingbird<-factor(sapply(dat$Hummingbird,function(x) {.simpleCap(as.character(x))}))

#Take our any bad data
dat<-droplevels(dat[!dat$Hummingbird %in% c("","NANA"),])

#Overall statistics
#How many flowers Species
paste("Number of Flower Species:",nlevels(dat$Flower))

#How many Birds Species
paste("Number of Hummingbird Species:",nlevels(dat$Hummingbird))

############################################
#Run Network Function for the entire dataset
NetworkC(dat,"Total")
############################################

####################################################
#Temporal Change in Network Structure
####################################################

dat.split<-split(dat,dat$Month,drop=TRUE)

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
ggplot(hum.fl[hum.fl$Species %in% species_keep,],aes(Flowers,value,col=as.factor(Month))) + facet_grid(Metric~Species,scale="free") + geom_point() + geom_smooth(method="lm",aes(group=1))
ggsave(paste(netPath,"SpeciesProp_Flowers.svg",sep=""),height=8,width=11,dpi=300)

#Save image to file
setwd(droppath)
save.image("Thesis/Maquipucuna_SantaLucia/Results/Network/NetworkData.Rdata")