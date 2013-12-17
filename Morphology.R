#Data Collected by Holger Beck,K. Lohman and B. Weinstein
#R script Ben Weinstein - Stony Brook University 7/7/2013
#Under Git repository - Maquipucuna

require(vegan)
require(reshape2)
require(ggplot2)
require(grid)

#needs dev library ggbiplot
require(devtools)
install_github("ggbiplot", "vqv")
library(ggbiplot)
#

#setwd to dropbox
droppath<-"C:/Users/Jorge/Dropbox/"
setwd(droppath)
#Set github path
gitpath<-"C:/Users/Jorge/Documents/Maquipicuna/"

#if not being sourced from Specialization.R, run the next line to get the env
#save.image("Thesis/Maquipucuna_SantaLucia/Results/Network/NetworkData.Rdata")

#bring in clade data
clades<-read.csv(paste(gitpath,"InputData//CladeList.txt",sep=""),header=FALSE)[,-1]
colnames(clades)<-c("Clade","Genus","Species","double","English")
clades<-clades[,1:5]

###Bring in trait data, made possible by G. Stiles
morph <- read.csv(paste(gitpath,"InputData//MorphologyShort.csv",sep=""),na.strings="9999")

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
zscore <- apply(mon, 2, function(x){
  y<-(x - mean(x))/sd(x)
  return(y)
})

############################################
#match the species to the names at the site
#############################################

#Clean English Names
#Get the scientific names to match to the morphology dataset

#which english names have errors?
errors<-levels(dat$Hummingbird)[!levels(dat$Hummingbird) %in% clades$English]

#Fix the levels
levels(dat$Hummingbird)[!levels(dat$Hummingbird) %in% clades$English]<-c("Andean Emerald","Fawn-breasted Brilliant","Gorgeted Sunangel","Tyrian Metaltail")

#Add a species never seen, but known from the siote
sp.list<-c(levels(dat$Hummingbird),"Empress Brilliant")
sci.name<-clades[clades$English %in% sp.list,"double"]

#The trait dataset has . instead of " "
site.trait<-z.scores[rownames(z.scores) %in% gsub(" ",".",sci.name),]
trait_pc<-prcomp(site.trait)

#normal plot, kinda ugly, need to zoom in
biplot(trait_pc,cex=.5)

#Try the ggplot biplot to color by clades (or later, behavioral roles)
toCol<-clades[gsub(" ",".",clades$double) %in% rownames(trait_pc$x),"Clade"]

#Label species names and clades
ggbiplot(trait_pc,groups=toCol,labels=rownames(trait_pc$x))

#optionally add in circles
ggbiplot(trait_pc,groups=toCol,labels=rownames(trait_pc$x),ellipse=TRUE)

#########################
#Okay try 17 trait 
morph17 <- read.csv(paste(gitpath,"InputData//Morphology_Full.csv",sep=""),na.strings=9999)

#only want the first 21 cols and spname 
colnames(morph17)
morph17<-morph17[,c(1:21,44)]

#Name in English
colnames(morph17)<-c("MorphID","SpID","Sex","N","Bill_Length","Mass","Bill-width","Total_Culmen","WingChord","Bill_Depth","Wing_Width","Wing_Length","Aspect_Ratio","Wing_Area","Wing_Loading","Wing_Taper","Wing_Area(alArea)","Tail_Length","Foot_Extension","Tarsus_Length","Nail_Length","Species")

#just get males
morph.male<-morph17[morph17$Sex=="Macho",]

#aggregate for species, remove last column
agg.morph<-aggregate(morph.male,list(morph.male$Species),mean,na.rm=TRUE)
mon<-agg.morph[,-23]

#name first column Species
colnames(mon)[1]<-c("Species")

#make into rownames
rownames(mon)<-mon[,1]
mon<-mon[,-1]

#get traits for the species at the site, replace period with space
trait17<-mon[rownames(mon) %in% gsub(" ",".",sci.name),]

nrow(trait17)

#Which species did we miss
missing<-gsub(" ",".",sci.name)[!gsub(" ",".",sci.name) %in% rownames(mon)]

#Add in closest related species for the species we are missing
trait17<-rbind(trait17,mon[c("Heliangelus.exortis","Thalurania.furcata"),])

#take out columns unwanted in the PCA
head(trait17)
trait17<-trait17[,-c(1,2,3,4)]



#Standard the matrix to correct for different units by subtracting the means and dividing by sd
zscore <- apply(trait17, 2, function(x){
  y<-(x - mean(x))/sd(x)
  return(y)
})

trait_pc<-prcomp(zscore)

#normal plot, kinda ugly, need to zoom in
biplot(trait_pc,cex=.5)

#Try the ggplot biplot to color by clades (or later, behavioral roles)
toCol<-clades[gsub(" ",".",clades$double) %in% rownames(trait_pc$x),"Clade"]

#Label species names and clades
ggbiplot(trait_pc,groups=toCol,labels=rownames(trait_pc$x))

#optionally add in circles
ggbiplot(trait_pc,groups=toCol,labels=rownames(trait_pc$x),ellipse=TRUE)

