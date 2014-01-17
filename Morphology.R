#Data Collected by Holger Beck,K. Lohman and B. Weinstein
#R script Ben Weinstein - Stony Brook University 7/7/2013
#Under Git repository - Maquipucuna

require(vegan)
require(reshape2)
require(ggplot2)
require(grid)

#needs dev library ggbiplot
#require(devtools)
#install_github("ggbiplot", "vqv")
library(ggbiplot)
#

# #setwd to dropbox
# droppath<-"C:/Users/Jorge/Dropbox/"
# setwd(droppath)
# #Set github path
# gitpath<-"C:/Users/Jorge/Documents/Maquipicuna/"

#if not being sourced from Specialization.R, run the next line to get the env
#load("Thesis/Maquipucuna_SantaLucia/Results/Network/NetworkData.Rdata")

#Get Hummingbird Interaction - just used for rownames, hummingbird species list.
int<-read.csv("Thesis/Maquipucuna_SantaLucia/Results/Network/HummingbirdInteractions.csv")

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

#Legacy, save a version 
mon3trait<-mon

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
errors<-levels(int$Hummingbird)[!levels(int$Hummingbird) %in% clades$English]

print(paste(errors,"not matched"))
#Fix the levels
levels(int$Hummingbird)[!levels(int$Hummingbird) %in% clades$English]<-c("Green-Crowned Woodnymph")

#Add a species never seen, but known from the site
sp.list<-c(levels(int$Hummingbird))

#write species list to file

sci.name<-clades[clades$English %in% sp.list,"double"]
#write.csv(sci.name,paste(gitpath,"InputData//HummingbirdSpecies.csv",sep=""))

#The trait dataset has . instead of " "
site.trait<-zscore[rownames(zscore) %in% gsub(" ",".",sci.name),]
trait_pc<-prcomp(site.trait)

#normal plot, kinda ugly, need to zoom in
biplot(trait_pc,cex=.5)

#Try the ggplot biplot to color by clades (or later, behavioral roles)
toCol<-clades[gsub(" ",".",clades$double) %in% rownames(trait_pc$x),"Clade"]

#Label species names and clades
ggbiplot(trait_pc,groups=toCol,labels=rownames(trait_pc$x))

######################################
#PCA USING 17 TRAIT DATA FROM STILES 
######################################

morph17 <- read.csv(paste(gitpath,"InputData//Morphology_Full.csv",sep=""),na.strings=9999)

#only want the first 21 cols and spname 
colnames(morph17)
morph17<-morph17[,c(1:21,44)]

#Name in English
colnames(morph17)<-c("MorphID","SpID","Sex","N","Bill","Mass","Bill_width","Total_Culmen","WingChord","Bill_Depth","Wing_Width","Wing_Length","Aspect_Ratio","Wing_Area","Wing_Loading","Wing_Taper","Wing_Area(alArea)","Tail_Length","Foot_Extension","Tarsus_Length","Nail_Length","Species")

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

print(paste("Missing morphology:",missing))

#Add in closest related species for the species we are missing
trait17_fullsp<-rbind(trait17,mon[c("Heliangelus.exortis","Thalurania.furcata"),])
rownames(trait17_fullsp)[rownames(trait17_fullsp) %in% c("Heliangelus.exortis","Thalurania.furcata")] <- c("Heliangelus.strophianus","Thalurania.fannyi")

#Another option is just to add from the three trait data?
#head(mon3trait)

#trait17_fullsp<-rbind.fill(trait17,mon3trait[missing,])
#rownames(trait17_fullsp)<-c(rownames(trait17),rownames(mon3trait[missing,]))
#merge with clade info to write to file
clades$double.<-gsub(" ",".",clades$double)
trait17F<-merge(trait17_fullsp,clades,by.x="row.names",by.y="double.")
write.csv(trait17F,"Thesis/Maquipucuna_SantaLucia/Results/HummingbirdMorphology.csv")

#take out columns unwanted in the PCA
head(trait17_fullsp)
colnames(trait17_fullsp)
trait17_fullsp<-trait17_fullsp[,-c(1,2,3,4,7,8,10,11,12,13,14,16,17,18,19)]

#Standard the matrix to correct for different units by subtracting the means and dividing by sd
zscore <- apply(trait17_fullsp, 2, function(x){
  y<-(x - mean(x))/sd(x)
  return(y)
})

#Import role file
roles<-read.csv(paste(gitpath,"InputData//HummingbirdSpecies.csv",sep=""))

#match the roles to the trait_pc
#get order of pca from rownames

ord<-data.frame(Species=rownames(zscore))
m.ord<-merge(ord,roles,by="Species",sort=FALSE,all.x=TRUE)

trait_pc<-prcomp(zscore[!m.ord$Role %in% "UKWN",])

#normal plot, kinda ugly, need to zoom in
biplot(trait_pc,cex=.5)

#Label species names and clades
p<-ggbiplot(trait_pc,labels=rownames(trait_pc$x))
p + theme_bw() + geom_point()

#optionally add in circles
ggbiplot(trait_pc,groups=m.ord$Role[!m.ord$Role %in% "UKWN"],labels=rownames(trait_pc$x),ellipse=TRUE) + geom_point()
ggsave("Thesis/Maquipucuna_SantaLucia/Results/Morphology.svg",dpi=300)

#optionally add in circles
ggbiplot(trait_pc,groups=m.ord$Role[!m.ord$Role %in% "UKWN"],labels=rownames(trait_pc$x),ellipse=TRUE) + geom_point()
ggsave("Thesis/Maquipucuna_SantaLucia/Results/RoleMorphology.svg",dpi=300)

#Create a distance matrix of morphological similarity among all species
sp.dist<-as.matrix(dist(zscore))

#Write species distance to file
write.csv(sp.dist,"Thesis/Maquipucuna_SantaLucia/Results/HummingbirdDist.csv")

save.image("Thesis/Maquipucuna_SantaLucia/Results/Morphology.Rdata")
