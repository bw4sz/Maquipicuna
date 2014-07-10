#Phenotypic Matching Among Plants and Birds
require(reshape)
require(ggplot2)
require(chron)
require(stringr)
require(scales)
require(taxize)

#Setwd if not run globally
droppath<-"C:/Users/Ben/Dropbox/"
setwd(droppath)

#read in flower morphology data, comes from Nectar.R
fl.morph<-read.csv(paste(droppath,"Thesis/Maquipucuna_SantaLucia/Results/FlowerMorphology.csv",sep=""))

#First row is empty
fl.morph<-fl.morph[-1,]

#Bring in Hummingbird Morphology Dataset, comes from
hum.morph<-read.csv("Thesis/Maquipucuna_SantaLucia/Results/HummingbirdMorphology.csv")

#Bring in Interaction Matrix from the Network.R script
int<-read.csv("Thesis/Maquipucuna_SantaLucia/Results/Network/HummingbirdInteractions.csv",row.names=1)

#Melt the interaction frame and match it with the traits
m.dat<-int[colnames(int) %in% c("ID","Video","Time","Hummingbird","Sex","TransectID","Transect_R","Iplant_Double","Pierce","DateP","Month")]

#Fix spacing to match clades

#Which are matching
hum.morph$English
levels(m.dat$Hummingbird)[!levels(m.dat$Hummingbird) %in% hum.morph$English]

#This needs to be checked
print(paste(levels(m.dat$Hummingbird)[!levels(m.dat$Hummingbird) %in% hum.morph$English],"not matched"))
#levels(m.dat$Hummingbird)[!levels(m.dat$Hummingbird) %in% hum.morph$English]<-c("Green-crowned Woodnymph")
m.datH<-merge(m.dat,hum.morph, by.x="Hummingbird",by.y="English")

#Merge to flowers
int.FLlevels<-levels(factor(m.datH$Iplant_Double))
int.FLlevels

#Which flowers are we missing info for?
missingTraits<-int.FLlevels[!int.FLlevels %in% fl.morph$X]

print(paste("Missing Trait Information:",missingTraits))
m.datH<-merge(m.datH,fl.morph, by.x="Iplant_Double",by.y="X")

######Univariate Phenotype Matching##########

#Some of these observations are suspect, the booted racket-tail on the 50cm plant?
#I think we need to do the regression seperately?
p<-ggplot(m.datH,aes(x=factor(Bill),TotalCorolla,col=Hummingbird)) + geom_point() + geom_boxplot(aes(group=factor(Bill)))
p + geom_smooth(aes(group=1),method="lm")
ggsave("Thesis/Maquipucuna_SantaLucia/Results/Phenotype/TotalCorollaMatching.svg",height=8,width=11,dpi=300)
ggsave("Thesis/Maquipucuna_SantaLucia/Results/Phenotype/TotalCorollaMatching.jpeg",height=8,width=11,dpi=300,units="in")

#Effective Corolla Matching
p<-ggplot(m.datH,aes(x=factor(Bill),EffectiveCorolla,col=Hummingbird)) + geom_point() + geom_boxplot(aes(group=factor(Bill)))
p + geom_smooth(aes(group=1),method="lm") + xlab("Bill")
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

#Add year columns
m.datH$Year<-years(as.character(m.datH$DateP))

m.datH[m.datH$Year %in% 2012,"Year"]<-2013

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
p<-ggplot(m.datH,aes(x=Fl.PC1,y=Fl.PC2,fill=Hummingbird)) + geom_polygon(alpha=.8) + facet_wrap(~Clade,drop=TRUE) + scale_fill_discrete()
p + geom_point()
ggsave("Thesis/Maquipucuna_SantaLucia/Results/Phenotype/FlowerSpace.svg",height=8,width=11,dpi=300)

#Another way to look at this? getting closer.
#ggplot(m.datH,aes(x=Fl.PC1,y=Fl.PC2,col=Hummingbird)) + stat_density() + facet_wrap(~Clade)

#polygons on flower use by hummingbirds traits
#create a flower genus column?
m.datH$FLGenus<-sapply(m.datH$Iplant_Double,function(x) strsplit(as.character(x),split="_")[[1]][[1]])

##get the family for each flower
fam_r<-data.frame(Genus=levels(as.factor(m.datH$FLGenus)),Family=tax_name(query = levels(as.factor(m.datH$FLGenus)) , get = "family",db="ncbi",ask=FALSE))
fam_r[fam_r$Genus=="Werklea","family"]<-"Malvaceae"
fam_r[fam_r$Genus=="Centropogon","family"]<-"Campanulaceae"

m.datH<-merge(m.datH,fam_r,by.x="FLGenus",by.y="Genus")

p<-ggplot(m.datH,aes(x=H.PC1,y=H.PC2,fill=family,alpha=.02)) + geom_polygon() + facet_wrap(~family,drop=TRUE)
p
toFL<-table(m.datH$Hummingbird, m.datH$family)
toFL<-melt(toFL)
toFL<-toFL[!toFL$value==0,]

tolabS<-merge(toFL,hum_load,by.x="Var.1",by.y="row.names")
colnames(tolabS)<-c("Hummingbird","family","value","H.PC1","H.PC2")
p  + geom_point(size=.5,col="red") + geom_text(data=tolabS,aes(label=Hummingbird),size=2.5)
ggsave("Thesis/Maquipucuna_SantaLucia/Results/Phenotype/HummingbirdSpace.svg",height=10,width=10,dpi=300)

#Build a label dataframe
toLab<-data.frame(Species=rownames(hum_load),hum_load)
p + geom_point(size=2,col="red") + annotate("text",label=toLab$Species,x=toLab$PC1,y=toLab$PC2,size=2) + labs(col="Flower Genus")
ggsave("Thesis/Maquipucuna_SantaLucia/Results/Phenotype/HummingbirdSpace_AllLabels.svg",height=20,width=20,dpi=300)

################
####################################################################
#Time Cycles
####################################################################################

#Any data without months?
nrow(m.datH[is.na(m.datH$Month),])


#Create a cut of every two months?
m.datH$MonthG<-cut(m.datH$Month,breaks=c(0,5,9,12))

#Start by plotting monthly breaks of corolla matching

p<-ggplot(m.datH[!is.na(m.datH$MonthG),],aes(x=factor(Bill),TotalCorolla,col=Hummingbird)) + geom_point() + geom_boxplot(aes(group=factor(Bill)))
p + geom_smooth(aes(group=1),method="lm") + facet_wrap(~MonthG) + theme_bw()
ggsave("Thesis/Maquipucuna_SantaLucia/Results/Phenotype/Matching_TimeGroup.svg")

p<-ggplot(m.datH[!is.na(m.datH$MonthG),],aes(x=Bill,TotalCorolla,col=MonthG)) + geom_point() + stat_smooth(aes(group=MonthG),method="lm")
ggsave("Thesis/Maquipucuna_SantaLucia/Results/Phenotype/Matching_TimeGroup.jpeg",units="in",dpi=300)

#Start by plotting monthly breaks of corolla matching
#p<-ggplot(m.datH,aes(x=factor(Bill),EffectiveCorolla,col=Hummingbird)) + geom_boxplot() + geom_smooth(method="lm",aes(group=1))
#p + geom_point() + facet_wrap(~Month)

#####################################################################
#Difference Between Corolla and Bill Length of interactions measured
#####################################################################

m.datH$BD<-m.datH$Bill-m.datH$TotalCorolla
p<-ggplot(m.datH,aes(y=BD,x=Hummingbird)) + geom_boxplot(position="dodge")
p + coord_flip()

p<-ggplot(m.datH[!is.na(m.datH$MonthG),],aes(x=TotalCorolla,fill=MonthG)) + geom_density(alpha=.4)
p + facet_wrap(~Hummingbird,scales="free") + theme_bw()


p<-ggplot(m.datH[!is.na(m.datH$MonthG),],aes(x=BD,fill=MonthG)) + geom_density(alpha=.4,adjust=2)
p + theme_bw()


####################################################################
#Compared usage to available resources??
####################################################################

setwd(droppath)
load("Thesis/Maquipucuna_SantaLucia/Results/FlowerTransect.Rdata")

#THIS NEEDS TO BE FIXED
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

#create a linear model for each month

s.datH<-split(m.datH,list(m.datH$Month,m.datH$Year),drop=TRUE)

coeff<-rbind.fill(lapply(s.datH,function(x){
  mod<-lm(data=x,TotalCorolla~Bill)
  mod.s<-summary(mod)
  data.frame(Month=unique(x$Month),Year=as.numeric(as.character(unique(x$Year))),Slope=mod$coefficients[["Bill"]],R=mod.s$r.squared)
    }))

#How many observations each month
#nobs<-melt(as.data.frame.array(table(m.datH$Month,m.datH$Year),drop=TRUE))

#combine the flower totals and regression output
nStats<-merge(month.totals,coeff,by=c("Month","Year"))

#Quick function to turn numeric month to abbreviation
monthtr<-function(x){
  month.abb[x]
}

#Quick visualization, get rid of some months for now
p<-ggplot(data=nStats,aes(x=Flowers,y=Slope,shape=Year,col=R)) + geom_point(size=3) + stat_smooth(method="lm",aes(group=1)) + geom_text(aes(label=monthtr(Month)),size=3.5,vjust=-.5)
p+ theme_bw() + scale_color_continuous(low="blue",high="red") + labs(y="Slope (Bill~Corolla)",x="Available Resources",col="R^2")
ggsave("Thesis/Maquipucuna_SantaLucia/Results/Phenotype/MatchingSlope.jpeg",height=8,width=11,dpi=300)

#Merge with flower morphology
transectM<-merge(full.fl,fl.morph, by.x="Iplant_Double",by.y="X")
qplot(transectM$TotalCorolla,binwidth=1,geom="histogram") + theme_bw()

#Density plot of resource selection
ggplot(data=m.datH,aes(x=TotalCorolla)) + facet_wrap(~Hummingbird,scales="free") + geom_density(binwidth=1,data=transectM,aes(x=TotalCorolla),fill="black") + geom_density(fill="red",binwidth=1,alpha=.5)

#Density plot of resource selection and month
ggplot(data=m.datH,aes(x=TotalCorolla)) + geom_density(binwidth=1,data=transectM,aes(x=TotalCorolla),fill="black") + geom_density(binwidth=1,alpha=.5,aes(fill=Hummingbird)) + facet_grid(Hummingbird~Month,scales="free")


#####################
#By month grouping
######################

#The aggregate totals of the flower assemblage
head(fl.totals)

fl.totals$MonthG<-cut(fl.totals$Month,breaks=c(0,5,9,12))

#aggregate by month for now, not elev split
month.totals<-aggregate(fl.totals$TotalFlowers,list(fl.totals$MonthG,fl.totals$Year),sum,na.rm=TRUE)
colnames(month.totals)<-c("MonthG","Year","Flowers")


#create a linear model for each month
coeff<-rbind.fill(lapply(levels(m.datH$MonthG), function(x){
  mod<-lm(data=m.datH,TotalCorolla~Bill,subset=MonthG ==x)
  mod.s<-summary(mod)
  data.frame(MonthG=x,Slope=mod$coefficients[["Bill"]],R=mod.s$r.squared)
}))

#split flower month totals into the groupings

#Create a cut of every two month

#combine the flower totals and regression output
nStats<-merge(month.totals,coeff,by.x="MonthG")

#Quick function to turn numeric month to abbreviation
monthtr<-function(x){
  month.abb[x]
}

#Quick visualization, get rid of some months for now
p<-ggplot(data=nStats[nStats$Year %in% c(2013,2014),],aes(x=Flowers,y=Slope,shape=Year,col=R)) + geom_point(size=3) + geom_smooth(method="lm",aes(group=1)) #+ geom_text(size=3.5,vjust=-.5)
p+ theme_bw() + scale_color_continuous(low="blue",high="red") + labs(y="Slope (Bill~Corolla)",x="Available Resources",col="R^2")
ggsave("Thesis/Maquipucuna_SantaLucia/Results/Phenotype/MatchingSlope.jpeg",height=8,width=11,dpi=300)

######################################
#Compared to each other?
######################################
#without month percentages
require(scales)
p<-ggplot(data=m.datH,aes(x=TotalCorolla)) + facet_wrap(~Hummingbird,scale="free") + geom_bar(binwidth=1,data=transform(m.datH,Hummingbird=NULL),aes(y = (..count..)/sum(..count..)),fill="black") 
p + geom_bar(binwidth=1,aes(y = (..count..)/sum(..count..)),fill="red",alpha=.4) + scale_y_continuous("Resource Use",labels = percent_format()) + xlab("Corolla Length")
ggsave("Thesis/Maquipucuna_SantaLucia/Results/Phenotype/FLUsePercent.svg",height=8,width=9)

#One one figure?
p<-ggplot(data=m.datH,aes(x=TotalCorolla)) + geom_bar(data=transform(m.datH,Hummingbird=NULL),aes(y = (..count..)/sum(..count..)),fill="black") + facet_wrap(~Clade,scales="free")
p + geom_bar(aes(y = (..count..)/sum(..count..),fill=Hummingbird),alpha=.9,position="dodge") + scale_y_continuous("Resource Use",labels = percent_format()) + xlab("Corolla Length")
ggsave("Thesis/Maquipucuna_SantaLucia/Results/Phenotype/FLUsePercent.svg",height=8,width=9)

#One one figure?
p<-ggplot(data=m.datH,aes(x=TotalCorolla)) + geom_density(data=transform(m.datH,Clade=NULL),fill="black") + facet_wrap(~Clade)
p + geom_density(aes(fill=Hummingbird),alpha=.5,position="dodge") + ylab("Resource Use") + xlab("Corolla Length")
ggsave("Thesis/Maquipucuna_SantaLucia/Results/Phenotype/FLUsePercent.svg",height=8,width=9)


save.image("Thesis/Maquipucuna_SantaLucia/Results/PhenotypicMatching.Rdata")
