#Flower Diversity Transects and Nectar
#Data Collected by Holger Beck,K. Lohman and B. Weinstein
#R script Ben Weinstein - Stony Brook University 7/7/2013
#Under Git repository - Maquipucuna

#Read in required libraries
require(ggplot2)
require(reshape2)
require(maptools)
library(plyr)

#Set DropBox Working Directory

setwd("C:/Users/Ben/Dropbox/")

#Read in Flower Transect Data from summer field season
fl<-read.csv(file="Thesis/Maquipucuna_SantaLucia/Data2013/csv/FlowerTransects.csv")

#Load in holger's on going data
holger.fl<-read.csv("Thesis/Maquipucuna_SantaLucia/Data2013/csv/HolgerTransects.csv")

#Fix the column names and rbindfill
colnames(fl)<-c("Family","Genus","Species","Height","Flowers","Stalks","Inflorescences.Plants","GPS_ID","Accuracy","Hummingbird.Species","Photo","Transect.ID","Comment")

colnames(holger.fl)<-c("Transect.ID","Date","Time","Family","Genus","Species","Height","Flowers","Stalks","Inflorescences.Plants","Hummingbird.Species","GPS_ID","Comment","Photo")

#To compare transects we need the transect id page
TID<-read.csv("Thesis/Maquipucuna_SantaLucia/Data2013/csv/TransectIID.csv")
TID$Transect_R<-as.factor(paste(TID$Elevation.Begin,TID$Elevation.End,sep="_"))

#Select the flower transects
TID.f<-TID[TID$Type=="Flower",]
fl.id<-merge(fl,TID.f,by.x="Transect.ID",by.y="TransectID")

#view flower data
head(fl.id)

full.fl<-rbind.fill(fl.id,holger.fl[,1:14])

head(full.fl)
#Data cleaning and check
#Turn first letter of family to uppercase
#see toupper?
.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

full.fl$Family<-as.factor(sapply(full.fl$Family,function(x) .simpleCap(as.character(x))))
#How many families do we have?
fl.t<-melt(table(full.fl$Family))
names(fl.t)<-c("Family","Count")
ggplot(fl.t,aes(Family,Count)) + geom_bar() + coord_flip() + theme_bw()


#Turn genus to uppercase
full.fl$Genus<-as.factor(sapply(full.fl$Genus,function(x) .simpleCap(as.character(x))))
table(full.fl$Genus)
fl.g<-melt(table(full.fl$Genus))
names(fl.g)<-c("Genus","Count")
ggplot(fl.g,aes(Genus,Count)) + geom_bar() + coord_flip() + theme_bw()

#######
#MS species need to be subbed in for the best taxonomy known
full.fl<-full.fl[!full.fl$Family %in% c("Ms1","Ms2"),]
#for now just remove?

########################################################
#Combination of family genus count
fl.fg<-melt(table(paste(full.fl$Family,full.fl$Genus)))
names(fl.fg)<-c("F.Genus","Count")
ggplot(fl.fg,aes(F.Genus,Count)) + geom_bar() + coord_flip() + theme_bw()

#For species, turn all to lowercase
full.fl$Species<-as.factor(sapply(full.fl$Species,tolower))
fl.s<-melt(table(full.fl$Species))
names(fl.s)<-c("Species","Count")
ggplot(fl.s,aes(Species,Count)) + geom_bar() + coord_flip() + theme_bw()

#Family Genus Species, in future, create abreviations
fl.all<-melt(table(paste(full.fl$Family,full.fl$Genus,full.fl$Species)))
names(fl.all)<-c("Species","Count")
ggplot(fl.all,aes(Species,Count)) + geom_bar() + coord_flip() + theme_bw()
ggsave(filename="Thesis/Maquipucuna_SantaLucia/Results/SpeciesCount.jpeg",height=8,width=11)

#Use this as the marker for now
full.fl$Full<-paste(full.fl$Family,full.fl$Genus,full.fl$Species)

#Create total flower column by multiplying the Flowers per stalk, by stalks and total inflorescens
#First need to take the average flowers per stalk, and all other columns
full.fl$mean_flowerStalk<-sapply(full.fl$Flowers,function(x) {
  mean(as.numeric(strsplit(as.character(as.list(x)[[1]]),",")[[1]]),na.rm=TRUE)
})

full.fl$mean_Stalk<-sapply(full.fl$Stalks,function(x) {
  mean(as.numeric(strsplit(as.character(as.list(x)[[1]]),",")[[1]]))
})

full.fl$mean_inflorescences<-sapply(full.fl$Inflorescences.Plants,function(x) {
  mean(as.numeric(strsplit(as.character(as.list(x)[[1]]),",")[[1]]))
})

full.fl$Total_Flowers<-full.fl$mean_flowerStalk*full.fl$mean_Stalk*full.fl$mean_inflorescences
hist(fl$Total_Flowers)

#Visualize total flowers by species
ggplot(data=full.fl,aes(Full,Total_Flowers)) + geom_boxplot() + coord_flip() #+ ylim(0,200)

#visualize height by species
ggplot(data=full.fl,aes(Full,as.numeric(Height))) + geom_boxplot() + coord_flip()


#Data cleaning on hummingbirds
full.fl<-data.frame(full.fl,hum.split<-colsplit(as.character(full.fl$Hummingbird.Species),",",c("Hummingbird","Sex")))

#Count of hummingbird records
table(full.fl$Hummingbird)

full.fl$Hummingbird<-as.factor(full.fl$Hummingbird)

#Ignore blank cells
levels(full.fl$Hummingbird)[1]<-NA

#Hummingbirds on which flowers
G.hum<-melt(table(full.fl$Genus,full.fl$Hummingbird))

#remove the empty observations
G.hum[G.hum$value==0,"value"]<-NA
colnames(G.hum)<-c("Genus","Hummingbird","Observation")
GH<-ggplot(G.hum,aes(x=Genus,y=Hummingbird,fill=Observation)) + geom_tile() + theme_bw() + scale_fill_continuous(high="red",na.value="white")
GH + theme(axis.text.x=element_text(angle=90))

ggsave(filename="Thesis/Maquipucuna_SantaLucia/Results/Hummingbird_Genus.jpeg",height=8,width=11)

#########################
#Skip this section for now 10/8/2013, needs to be redone to work with holgar data
#Compare transects

#aggregate flowers by range
fl.agg<-aggregate(fl.id$Total_Flowers,by=list(fl.id$Transect_R,fl.id$Replicate.Number,fl.id$Date),sum,na.rm=TRUE)
names(fl.agg)<-c("Range","Replicate.Number","Date","Flowers")

ggplot(fl.agg,aes(y=Flowers,x=Range)) + geom_point()

#How does the distribution of genera change with elevation
g.r<-table(fl.id$Transect_R,fl.id$Genus)
Record_Total<-apply(table(fl.id$Transect_R,fl.id$Genus),1,sum)
prop.Genus<-g.r/Record_Total
prop.Genus<-melt(prop.Genus)

prop.Genus<-prop.Genus[!prop.Genus$value %in% 0,]
names(prop.Genus)<-c("Range","Genus","Proportion")

#merge in family for facet
prop.Genus$Family<-sapply(prop.Genus$Genus,function(x) {
  unique(fl[fl$Genus %in% x,"Family"])})

prop.Genus<-prop.Genus[!prop.Genus$Range %in% "NA_NA",]
#How does the proportion of records change by elevation
ggplot(prop.Genus,aes(x=Range,y=Proportion,fill=Genus)) + geom_bar(position="stack") 


## Plot proportions by family, Genera as lines within each facet, just more than 3%
p<-ggplot(prop.Genus[prop.Genus$Proportion > .05,],aes(x=Range,y=Proportion*100)) + geom_line(aes(group=Genus,col=Genus),size=1.2) + facet_wrap(~Family) + geom_point()
p+ theme_bw()+theme(axis.text.x = element_text(angle = 90))

m.TID.genus<-melt(table(fl.id$Transect_R,fl.id$Genus))
colnames(m.TID.genus)<-c("Range","Genus","Count")

ggplot(m.TID.genus,aes(x=Range,y=Genus,size=Count)) + geom_point() + coord_flip()
m.fl<-melt(fl,id.var="Transect.ID")

#########################
#Bring elevation gps data
Elev<-lapply(list.files("F:\\FieldWork2013\\KarenGPS\\shp",pattern=".shp",full.names=T),readShapePoints)
Elev<-rbind.fill(lapply(Elev,as.data.frame))

#Turn GPS ID into factor
fl$GPS_ID<-as.numeric(fl$GPS_ID)

#add to 00 to anything less than 10
fl$GPS_ID[fl$GPS_ID < 10]<-paste("00",fl$GPS_ID[fl$GPS_ID < 10],sep="")
fl$GPS_ID[as.numeric(fl$GPS_ID) < 100 & as.numeric(fl$GPS_ID) >= 10 ]<-paste("0",fl$GPS_ID[as.numeric(fl$GPS_ID) < 100 & as.numeric(fl$GPS_ID) >= 10],sep="")

#Match each point with an elevation
fl.elev<-merge(fl,Elev,by.x="GPS_ID",by.y="name")

#How records were in fl, but not matched
fl[!fl$GPS_ID %in% fl.elev$GPS_ID,]$GPS_ID

#How many flowers at each elevation
ggplot(fl.elev,aes(elevation,y=Total_Flowers)) + geom_line() + geom_point(aes(color=Family),size=2) + theme_bw() 
p<-ggplot(fl.elev,aes(x=elevation,y=Total_Flowers,col=Species),size=2) + geom_point() + theme_bw() + facet_wrap(~Family,scales='free_y') + guides(color="none")
p+stat_smooth(method='lm',aes(group=Family))
#spatially?
ggplot(fl.elev,aes(x=coords.x1,y=coords.x2,col=Total_Flowers),size=200) + theme_bw() + geom_point() + scale_color_continuous(high="red",low="blue",limits=c(0,200))

#Bring in nectar data
Nectar <- read.csv("F:/FieldWork2013/Data2013/csv/Nectar.csv")
#Fix colnames that are ugly
colnames(Nectar)[c(8,10,11,12,13)]<-c("Height","TubeLength","Brix","EffectiveCorolla","TotalCorolla")
  

#Data Cleaning
#All family and Genus should be capitalized
Nectar$Family<-as.factor(sapply(Nectar$Family,function(x) .simpleCap(as.character(x))))
Nectar$Genus<-as.factor(sapply(Nectar$Genus,function(x) .simpleCap(as.character(x))))
#species should be lowercase
Nectar$Species<-as.factor(sapply(Nectar$Species,function(x) tolower(as.character(x))))


#Create full name column
Nectar$Full<-paste(Nectar$Family,Nectar$Genus,Nectar$Species)

#Some basic visualizations to check data clarity
#number of records per species
m.Nectar<-melt(table(Nectar[!is.na(Nectar$Brix),]$Full))


#No records should be 0? turn these to NA
Nectar[Nectar$Brix==0 & is.finite(Nectar$Brix),]

#Is the tube given in diameter?
#tube column needs to have correct math.

as.numeric(Nectar$Tube.Type)/2 * 2*pi * Nectar$TubeLength

ggplot(m.Nectar,aes(x=Var.1,value)) + geom_bar() + coord_flip() + geom_text(aes(label=value),col="red",hjust=1) + theme_bw()

p<-ggplot(Nectar[!is.na(Nectar$Brix),],aes(x=Species,y=Brix)) + geom_point() + facet_wrap(~Family,scales="free_x")
p+ theme_bw() +theme(axis.text.x = element_text(angle = 90,size=10))

p<-ggplot(Nectar[!is.na(Nectar$TotalCorolla),],aes(x=Species,y=TotalCorolla)) + geom_point() + facet_wrap(~Family,scales="free_x")
p+ theme_bw() +theme(axis.text.x = element_text(angle = 90,size=10)) + geom_point()

ggplot(Nectar,aes(x=TotalCorolla,y=Brix)) + geom_point(aes(color=Family)) + stat_smooth(method="lm") + xlim(0,60)
                    
ggplot(Nectar,aes(x=EffectiveCorolla,y=Brix)) + geom_point(aes(color=Family)) + stat_smooth(method="lm") + geom_text(aes(label=Family))

ggplot(Nectar,aes(x=Corolla.Width,y=Brix)) + geom_point(aes(color=Family)) + stat_smooth(method="lm") + xlim(0,14)

#combine the elevation flower transects with Nectar data 

#First take the average nectar reading by species
Nectar.mean<-aggregate(Nectar,list(Nectar$Full),mean,na.rm=TRUE)[,c("Group.1","EffectiveCorolla","TotalCorolla","Corolla.Width","Brix")]
fl.nectar<-merge(fl.elev,Nectar.mean,by.x="Full",by.y="Group.1")

#how many transect rows didn't match nectar rows
missing<-fl.elev[!fl.elev$Full %in% Nectar.mean$Group.1,]
dim(missing)
table(missing$Full)

#okay, before we go crazy with nectar,lets look at corolla length with elevation

ggplot(fl.nectar,aes(x=elevation,y=TotalCorolla,col=Family)) + geom_point() + stat_smooth(aes(group=1),method='lm')

#Nectar concentration and elevation, there are some odd records that need to be fixed
ggplot(fl.nectar,aes(x=elevation,y=Brix,col=Family)) + geom_point() + stat_smooth(aes(group=1),method='lm') + ylim(5,30)

#create volume calculations

save.image("/FieldWork2013/Data2013/Scripts/FlowerTransect.Rdata")