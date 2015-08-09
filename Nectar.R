#Flower Morphology 

#Data Collected by Holger Beck,K. Lohman and B. Weinstein
#R script Ben Weinstein - Stony Brook University 7/7/2013
#Under Git repository - Maquipucuna

#Bring in packages
require(ggplot2)
require(reshape)
require(taxize)
library(ggbiplot)
library(stringr)
library(dplyr)

#############
#setwd
#############

#Nectar Script, setwd if not running globally from specialization.R
droppath<-"C:/Users/Ben/Dropbox/"

setwd(droppath)

#Bring in nectar data, there is the original and the cleaned
Nectar <- read.csv(paste(droppath,"Thesis/Maquipucuna_SantaLucia/Results/Nectar_cleaned2014.csv",sep=""))

#Fix colnames that are ugly
colnames(Nectar)[c(8,10,11,12,13)]<-c("Height","TubeLength","Brix","EffectiveCorolla","TotalCorolla")

#####Data from image J photos of flowers#######
imageJ <- read.csv(paste(droppath,"Thesis/Maquipucuna_SantaLucia/Data2013//csv/ImageJ_AdditionalMorph.csv",sep=""))
colnames(imageJ)[c(8,10,11,12,13)]<-c("Height","TubeLength","Brix","EffectiveCorolla","TotalCorolla")

##Data from Santa Lucia Flower Book##########
FB <- read.csv(paste(droppath,"Thesis/Maquipucuna_SantaLucia/Data2013//csv/FlowerBookSantaLucia.csv",sep=""))
colnames(FB)<-c("Family","Genus","Species","TotalCorolla","Corolla.Width")
head(dat<-rbind.fill(Nectar,imageJ))
head(dat<-rbind.fill(dat,FB))

################
#Flower Taxonomy
################

#Go through a series of data cleaning steps, at the end remove all rows that are undesired
#Repeat for genus species
Species<-levels(factor(paste(dat$Genus,dat$Species,sep=" ")))

#look up online, skip the blank
#Doesn't like the unmanned species

Species<-Species[!Species %in% c(" pink ms"," sp.")]
tax<-gnr_resolve(names = Species, splitby=30,highestscore = T,stripauthority = T)

#Set the Species column
for (x in 1:nrow(dat)){
  y<-dat[x,]
  toMatch<-paste(y$Genus,y$Species,sep=" ")
  if(toMatch %in% tax$submittedname){
    dat[x,"Iplant_Double"]<-unique(tax$results[tax$results$submitted_name %in% toMatch,"matched_name"]   )[1]
  } else {
    next
  }}


#Lots of cleaning left to do, but that's a start. 
#Final levels

print(paste("Final Flower Species:", levels(factor(dat$Iplant_Double))))

#Write 
write.csv(levels(factor(dat$Iplant_Double)),"Thesis/Maquipucuna_SantaLucia/Results/FlowerTransects/Iplant_Names.txt")

#################################################################################

#View morphological distance
#Which morphologies do we want to visualize, include nectar later?
#get the means?
toPCA<-aggregate(dat[,c("TotalCorolla","EffectiveCorolla","Corolla.Width")],list(dat$Iplant_Double),mean,na.rm=TRUE)
rownames(toPCA)<-toPCA$Group.1

#Write morphology dataset to file
write.csv(toPCA,"Thesis/Maquipucuna_SantaLucia/Results/FlowerMorphology.csv")

#Perform PCA

toPCA<-na.omit(toPCA[,-1])
#toPCA<-na.omit(Nectar[-toRemove,c("TotalCorolla","EffectiveCorolla","Corolla.Width","Brix")])
head(toPCA)

#principal component traits and get euclidean distance matrix
zscore <- apply(toPCA, 2, function(x){
  y<-(x - mean(x))/sd(x)
  return(y)
})

pcA<-prcomp(zscore)

biplot(pcA)

#Color by genus
genera<-sapply(as.character(rownames(zscore)),function(x){
  strsplit(x,"_")[[1]][1]
})
ggbiplot(pcA,groups=genera) 
ggsave("Thesis/Maquipucuna_SantaLucia/Results/FloralPCA.svg",height=10,width=10,dpi=300)

# #Some basic visualizations to check data clarity
# #number of records per species
m.Nectar<-melt(table(dat[!is.na(Nectar$Brix),]$Iplant_Double))

#remove species with just one name.
dat<-dat[!is.na(dat$Iplant_Double),]

dat<-dat[!sapply(dat$Iplant_Double,function(x){
  length(str_split(x," ")[[1]])})==1,]

#order by brix
ord<-dat %>% group_by(Iplant_Double) %>% summarize(m=mean(Brix,na.rm=T)) %>% arrange(m) %>% select(Iplant_Double)

dat$Iplant_Double<-factor(dat$Iplant_Double,levels=ord$Iplant_Double)

#remove species without any records?
ggplot(dat[!is.na(dat$Brix) & !dat$Iplant_Double %in% "Gasteranthus leopardus",],aes(Iplant_Double,Brix)) + geom_boxplot()+ theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x="Plant Species",y=" Nectar Sucrose Concentration(%)")
ggsave("C:/Users/Ben/Documents/Selectivity/Figures/NectarSpecies.jpeg",dpi=600)


# #Sugar Concentration's of 0 to NA
#No records should be 0
Nectar[Nectar$Brix==0 & is.finite(Nectar$Brix),]


#start with nectar quality
nectar.mean<-aggregate(Nectar$Brix,list(Nectar$Iplant_Double),mean,na.rm=TRUE)

colnames(nectar.mean)<-c("Iplant_Double","Brix")
write.csv(nectar.mean,paste(droppath,"Thesis/Maquipucuna_SantaLucia/Results/nectarmean.csv",sep=""))
# #Is the tube given in diameter?
# #tube column needs to have correct math.

# as.numeric(Nectar$Tube.Type)/2 * 2*pi * Nectar$TubeLength
 ggplot(m.Nectar,aes(x=Var.1,value)) + geom_bar() + coord_flip() + geom_text(aes(label=value),col="red",hjust=1) + theme_bw()
 p<-ggplot(Nectar[!is.na(Nectar$Brix),],aes(x=Species,y=Brix)) + geom_boxplot
 p+ theme_bw() +theme(axis.text.x = element_text(angle = 90,size=10))
 p<-ggplot(Nectar[!is.na(Nectar$TotalCorolla),],aes(x=Species,y=TotalCorolla)) + geom_point() + facet_wrap(~Family,scales="free_x")
 p+ theme_bw() +theme(axis.text.x = element_text(angle = 90,size=10)) + geom_point()
# 
# ggplot(Nectar,aes(x=TotalCorolla,y=Brix)) + geom_point(aes(color=Family)) + stat_smooth(method="lm")
# ggplot(Nectar,aes(x=EffectiveCorolla,y=Brix)) + geom_point(aes(color=Family)) + stat_smooth(method="lm") + geom_text(aes(label=Family),size=2)
# ggplot(Nectar,aes(x=Corolla.Width,y=Brix)) + geom_point(aes(color=Family)) + stat_smooth(method="lm") 

save.image("Thesis/Maquipucuna_SantaLucia/Results/Nectar.Rdata")
