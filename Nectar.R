#Flower Morphology 

#Data Collected by Holger Beck,K. Lohman and B. Weinstein
#R script Ben Weinstein - Stony Brook University 7/7/2013
#Under Git repository - Maquipucuna

#################################
#Goals
#1
#2
##################################

#Bring in packages
require(ggplot2)
require(reshape)

#############
#setwd
#############

#Nectar Script, setwd if not running globally from specialization.R
#droppath<-"C:/Users/Jorge/Dropbox/"

#Bring in nectar data
Nectar <- read.csv(paste(droppath,"Thesis/Maquipucuna_SantaLucia/Data2013/csv/Nectar.csv",sep=""))

#Fix colnames that are ugly
colnames(Nectar)[c(8,10,11,12,13)]<-c("Height","TubeLength","Brix","EffectiveCorolla","TotalCorolla")


# #Data Cleaning
# #All family and Genus should be capitalized
# Nectar$Family<-as.factor(sapply(Nectar$Family,function(x) .simpleCap(as.character(x))))
# Nectar$Genus<-as.factor(sapply(Nectar$Genus,function(x) .simpleCap(as.character(x))))
# 
# #species should be lowercase
# Nectar$Species<-as.factor(sapply(Nectar$Species,function(x) tolower(as.character(x))))
# 
# #Create full name column
# Nectar$Full<-paste(Nectar$Family,Nectar$Genus,Nectar$Species)

#Some weird records?
toRemove<-c(567,415)
Nectar<-Nectar[-toRemove,]

######################################
#Taxonomic Correction - TBD
######################################

Families<-levels(factor(full.fl$Family))
iplant_names<-ResolveNames(names=Families)
CompareNames(Families,iplant_names)

Fam_Result<-data.frame(Families,iplant_names)
Fam_Errors<-Fam_Result[Fam_Result$iplant_names %in% "","Families"]

#Post to output which plant families need to be address
print(paste(Fam_Errors,"not found in taxonomy database"))

#Repeat for genus
Genus<-levels(factor(full.fl$Genus))
iplant_names<-ResolveNames(names=Genus)
CompareNames(Genus,iplant_names)

Genus_Result<-data.frame(Genus,iplant_names)
Genus_Errors<-Genus_Result[Genus_Result$iplant_names %in% "","Genus"]

#Post to output which plant families need to be address
print(paste(Genus_Errors,"not found in taxonomy database"))

#Set the Genus column
for (x in 1:nrow(full.fl)){
  y<-full.fl[x,]
  full.fl[x,"Iplant_Genus"]<-levels(droplevels(Genus_Result[Genus_Result$Genus %in% y$Genus,"iplant_names"] ))   
}

#Repeat for species
Species<-levels(factor(paste(full.fl$Iplant_Genus,full.fl$Species,sep="_")))
iplant_names<-ResolveNames(Species)
print(CompareNames(Species,iplant_names))
Species_Result<-data.frame(Species,iplant_names)

#Set the Species column
for (x in 1:nrow(full.fl)){
  y<-full.fl[x,]
  toMatch<-paste(y$Iplant_Genus,y$Species,sep="_")
  full.fl[x,"Iplant_Double"]<-levels(droplevels(
    Species_Result[Species_Result$Species %in% toMatch,"iplant_names"] ))   
}

#Lots of cleaning left to do, but that's a start. 
#Final levels
print(paste("Final Flower Species:", levels(factor(full.fl$Iplant_Double))))
#################################################################################

#View morphological distance
#Which morphologies do we want to visualize, include nectar later?
#get the means?
toPCA<-aggregate(Nectar[,c("TotalCorolla","EffectiveCorolla","Corolla.Width")],list(Nectar$Full),mean,na.rm=TRUE)
rownames(toPCA)<-toPCA$Group.1

#Write morphology dataset to file
write.csv(toPCA,"Thesis/Maquipucuna_SantaLucia/Results/FlowerMorphology.csv")


toPCA<-na.omit(toPCA[,-1])
#toPCA<-na.omit(Nectar[-toRemove,c("TotalCorolla","EffectiveCorolla","Corolla.Width","Brix")])
head(toPCA)
biplot(prcomp(toPCA),cex=.5)

#Some basic visualizations to check data clarity
#number of records per species
m.Nectar<-melt(table(Nectar[!is.na(Nectar$Brix),]$Full))
ggplot(m.Nectar,aes(Var.1,value)) + geom_point(size=5)+ theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

#Sugar Concentration's of 0 to NA
#No records should be 0
Nectar[Nectar$Brix==0 & is.finite(Nectar$Brix),]

#Is the tube given in diameter?
#tube column needs to have correct math.
as.numeric(Nectar$Tube.Type)/2 * 2*pi * Nectar$TubeLength
ggplot(m.Nectar,aes(x=Var.1,value)) + geom_bar() + coord_flip() + geom_text(aes(label=value),col="red",hjust=1) + theme_bw()
p<-ggplot(Nectar[!is.na(Nectar$Brix),],aes(x=Species,y=Brix)) + geom_point() + facet_wrap(~Family,scales="free_x")
p+ theme_bw() +theme(axis.text.x = element_text(angle = 90,size=10))
p<-ggplot(Nectar[!is.na(Nectar$TotalCorolla),],aes(x=Species,y=TotalCorolla)) + geom_point() + facet_wrap(~Family,scales="free_x")
p+ theme_bw() +theme(axis.text.x = element_text(angle = 90,size=10)) + geom_point()

ggplot(Nectar,aes(x=TotalCorolla,y=Brix)) + geom_point(aes(color=Family)) + stat_smooth(method="lm")
ggplot(Nectar,aes(x=EffectiveCorolla,y=Brix)) + geom_point(aes(color=Family)) + stat_smooth(method="lm") + geom_text(aes(label=Family),size=2)
ggplot(Nectar,aes(x=Corolla.Width,y=Brix)) + geom_point(aes(color=Family)) + stat_smooth(method="lm") 

