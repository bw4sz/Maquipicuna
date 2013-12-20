#Flower Morphology Nectar
#Data Collected by Holger Beck,K. Lohman and B. Weinstein
#R script Ben Weinstein - Stony Brook University 7/7/2013
#Under Git repository - Maquipucuna


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

#Define a function to cap
.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

#Data Cleaning
#All family and Genus should be capitalized
Nectar$Family<-as.factor(sapply(Nectar$Family,function(x) .simpleCap(as.character(x))))
Nectar$Genus<-as.factor(sapply(Nectar$Genus,function(x) .simpleCap(as.character(x))))

#species should be lowercase
Nectar$Species<-as.factor(sapply(Nectar$Species,function(x) tolower(as.character(x))))

#Create full name column
Nectar$Full<-paste(Nectar$Family,Nectar$Genus,Nectar$Species)

#Some weird records?
toRemove<-c(567,415)
Nectar<-Nectar[-toRemove,]

######################################
#Taxonomic Fixes
######################################

#Some basic visualizations to check data clarity
#number of records per species
m.Nectar<-melt(table(Nectar[!is.na(Nectar$Brix),]$Full))
ggplot(m.Nectar,aes(Var.1,value)) + geom_point(size=5)+ theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

#View morphological distance
#Which morphologies do we want to visualize, include nectar later?
#get the means?
toPCA<-aggregate(Nectar[,c("TotalCorolla","EffectiveCorolla","Corolla.Width")],list(Nectar$Full),mean,na.rm=TRUE)
rownames(toPCA)<-toPCA$Group.1
toPCA<-na.omit(toPCA[,-1])
#toPCA<-na.omit(Nectar[-toRemove,c("TotalCorolla","EffectiveCorolla","Corolla.Width","Brix")])
head(toPCA)
biplot(prcomp(toPCA),cex=.5)

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

