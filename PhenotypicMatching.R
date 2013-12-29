#Phenotypic Matching Among Plants and Birds
require(reshape)
require(ggplot2)
require(chron)
require(stringr)

#Setwd if not run globally
droppath<-"C:/Users/Ben/Dropbox/"
setwd(droppath)

#read in flower morphology data, comes from Nectar.R
fl.morph<-read.csv(paste(droppath,"Thesis/Maquipucuna_SantaLucia/Results/FlowerMorphology.csv",sep=""))

#Bring in Hummingbird Morphology Dataset, comes from
hum.morph<-read.csv("Thesis/Maquipucuna_SantaLucia/Results/HummingbirdMorphology.csv")

#Bring in Interaction Matrix from the Network.R script
int<-read.csv("Thesis/Maquipucuna_SantaLucia/Results/Network/HummingbirdInteractions.csv")

#Melt the interaction frame and match it with the traits
m.dat<-melt(int,id.vars=c("ID","Video","Flower","Time","Hummingbird","Sex","Comments","PhotoID","VideoClip","TransectID","Transect_R","Iplant_Double","Piercing","Date","Month"))

#Fix spacing to match clades
#Which are matching
hum.morph$English
levels(m.dat$Hummingbird)[!levels(m.dat$Hummingbird) %in% hum.morph$English]

#This needs to be checked
print(paste(levels(m.dat$Hummingbird)[!levels(m.dat$Hummingbird) %in% hum.morph$English],"not matched"))
levels(m.dat$Hummingbird)[!levels(m.dat$Hummingbird) %in% hum.morph$English]<-c("Green-crowned Woodnymph","Gorgeted Sunangel","Tyrian Metaltail")
m.datH<-merge(m.dat,hum.morph, by.x="Hummingbird",by.y="English")

#Merge to flowers
levels(factor(m.datH$Iplant_Double))
m.datH<-merge(m.datH,fl.morph, by.x="Iplant_Double",by.y="X")

######Univariate Phenotype Matching##########

#Some of these observations are suspect, the booted racket-tail on the 50cm plant?
#I think we need to do the regression seperately?
p<-ggplot(m.datH,aes(x=round(Bill,1),TotalCorolla,col=factor(Hummingbird))) + geom_boxplot(aes(factor(Bill)))
+ geom_smooth(aes(group=1),method="lm")
p<- p+ geom_boxplot() 
ggsave("Thesis/Maquipucuna_SantaLucia/Results/Phenotype/TotalCorollaMatching.svg",height=8,width=11,dpi=300)

#Effective Corolla Matching
p<-ggplot(m.datH,aes(x=factor(Bill),EffectiveCorolla,col=Hummingbird)) + geom_boxplot() + geom_smooth(method="lm",aes(group=1))
p + geom_point()
ggsave("Thesis/Maquipucuna_SantaLucia/Results/Phenotype/EffectiveCorollaMatching.svg",height=8,width=11,dpi=300)

#Corolla Width Matching
ggplot(m.datH,aes(x=Bill,Corolla.Width)) + geom_jitter() + geom_smooth(method="lm")
ggplot(m.datH,aes(x=Bill,Corolla.Width,col=Clade)) + geom_point() + geom_smooth(method="lm")

#Could do a multivariate space
#Standard the matrix to correct for different units by subtracting the means and dividing by sd
zscore <- apply(fl.morph[,c("TotalCorolla","EffectiveCorolla","Corolla.Width")], 2, function(x){
  y<-(x - mean(x))/sd(x)
  return(y)
})

#Label rows
rownames(zscore)<-fl.morph$X

#Principal Components
trait_pc<-prcomp(zscore)

#bind loadings 1 and 2 to dataframe
fl_load<-trait_pc$x[,c("PC1","PC2")]
rownames(fl_load)<-rownames(zscore)

m.datH<-merge(m.datH,fl_load,by.x="Iplant_Double",by.y="row.names")

#rename columns 
colnames(m.datH)[colnames(m.datH) %in% c("PC1","PC2")] <- c("Fl.PC1","Fl.PC2")

# Hum Standardized variables, what to do about NA's?
#Standard the matrix to correct for different units by subtracting the means and dividing by sd
zscore <- apply(hum.morph[,c("Bill","Mass","WingChord","Tarsus_Length","Nail_Length","Wing_Loading")], 2, function(x){
  y<-(x - mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
  return(y)
})
rownames(zscore)<-hum.morph$English

#Need to figure out what to do about Na's, we could use closely related species?
trait_pc<-prcomp(na.omit(zscore))

#View Biplot of PC Space
biplot(trait_pc)

#bind loadings 1 and 2 to dataframe
hum_load<-trait_pc$x[,c("PC1","PC2")]
rownames(hum_load)<-rownames(zscore)

m.datH<-merge(m.datH,hum_load,by.x="Hummingbird",by.y="row.names")

#rename columns 
colnames(m.datH)[colnames(m.datH) %in% c("PC1","PC2")] <- c("H.PC1","H.PC2")

#####################################
#Visualization of multivariate space
#####################################

#polygons on trait use by hummingbirds
p<-ggplot(m.datH,aes(x=Fl.PC1,y=Fl.PC2,fill=Hummingbird)) + geom_polygon() + facet_wrap(~Clade,drop=TRUE) + scale_fill_discrete()
p + geom_point()
ggsave("Thesis/Maquipucuna_SantaLucia/Results/Phenotype/FlowerSpace.svg",height=8,width=11,dpi=300)

#Another way to look at this? getting closer.
ggplot(m.datH,aes(x=Fl.PC1,y=Fl.PC2,col=Hummingbird)) + stat_densi() + facet_wrap(~Clade)

#polygons on flower use by hummingbirds traits
#create a flower genus column?
m.datH$FLGenus<-sapply(m.datH$Iplant_Double,function(x) strsplit(as.character(x),split="_")[[1]][[1]])
p<-ggplot(m.datH,aes(x=H.PC1,y=H.PC2,fill=FLGenus,alpha=.02)) + geom_polygon() + facet_wrap(~FLGenus,drop=TRUE) + geom_point()
p + geom_text(aes(label=Hummingbird),size=3)
ggsave("Thesis/Maquipucuna_SantaLucia/Results/Phenotype/HummingbirdSpace.svg",)

####################################################################################
#Time Cycles
####################################################################################

#Any data without months?
nrow(m.datH[is.na(m.datH$Month),])

#Start by plotting monthly breaks of corolla matching
p<-ggplot(m.datH,aes(x=Bill,TotalCorolla,col=Hummingbird)) + geom_boxplot(aes(x=factor(Hummingbird))) + geom_smooth(method="lm",aes(group=1))
p + geom_point() + facet_wrap(~Month,nrow=4) + theme_bw()

ggsave("Thesis/Maquipucuna_SantaLucia/Results/Phenotype/Matching_Time.svg",)

#Start by plotting monthly breaks of corolla matching
p<-ggplot(m.datH,aes(x=factor(Bill),EffectiveCorolla,col=Hummingbird)) + geom_boxplot() + geom_smooth(method="lm",aes(group=1))
p + geom_point() + facet_wrap(~Month)



###############################################
#Difference Between Corolla and Bill Length
m.datH$BD<-m.datH$Bill-m.datH$TotalCorolla
ggplot(m.datH,aes(x=BD,fill=Hummingbird)) + geom_density(position="dodge") + facet_wrap(~Clade,scale="free")

save.image("Thesis/Maquipucuna_SantaLucia/Results/PhenotypicMatching.Rdata")
