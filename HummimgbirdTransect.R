#Hummingbird Transect Data
#Collected at the Maquipucuna and Santa Lucia Ecoreserve, Ecuador
#July 17th 2013
#Ben Weinstien - Stony Brook University
setwd(dir="E:/Fieldwork2013/Data2013/csv/")
Hum<-read.csv("HummingbirdTransect.csv")
head(Hum)

#Data Cleaning
#check the spelling and factor levels
table(Hum$Hummingbird.Species)

#fix the unknown upper
levels(Hum$Hummingbird.Species)[levels(Hum$Hummingbird.Species) %in% "UkWN"]<-toupper(levels(Hum$Hummingbird.Species)[levels(Hum$Hummingbird.Species) %in% "UkWN"])

head(Hum)

# Fix the caps at the plant species
table(Hum$Plant.Species)

levels(Hum$Plant.Species)[levels(Hum$Plant.Species) %in% ""]<-NA

table(Hum$Plant.Species)

#Fix the height column, turn back from factors
ggplot(Hum,aes(x=Hummingbird.Species,y=as.numeric(Height))) + geom_boxplot() + coord_flip()

#Bring the transect data
TID<-read.csv("TransectIID.csv")

#Select the flower transects
TID.f<-TID[TID$Type %in% "Hummingbird",]
TID$TransectID<-as.factor(TID$TransectID)
Hum$ID
hum.id<-merge(Hum,TID.f,by.x="ID",by.y="TransectID")

hum.id$Range<-paste(hum.id$Elevation.Begin,hum.id$Elevation.End,sep="_")
#Get rid of the NA
hum.id<-hum.id[!hum.id$Range %in% "NA_NA",]
#Overal range presence absence
ggplot(hum.id,aes(x=Range,Hummingbird.Species)) + geom_line(aes(group=Hummingbird.Species)) + geom_point() 

#Broken up by replicate
ggplot(hum.id,aes(x=Range,Hummingbird.Species)) + geom_line(aes(group=Hummingbird.Species)) + geom_point() + facet_wrap(~Replicate.Number)



################################################
#Data cleaning on hummingbirds
################################################
#Needs to be fixed!
#It looks like there are two hummingbird species columns
Hummingbird.Species<-sapply(full.fl$Hummingbird.Species,function(x){
  strsplit(as.character(x),",")[1]
})

#There are still some with double entries?
#Needs to be addressed within the data

full.fl$Hummingbird.Species<-droplevels(as.factor(full.fl$Hummingbird.Species))


#Count of hummingbird records
table(full.fl$Hummingbird.Species)

#Ignore blank cells
levels(full.fl$Hummingbird)[1]<-NA

#Fix typing errors
levels(full.fl$Hummingbird)[levels(full.fl$Hummingbird) %in% "Booted Rackettail"]<-"Booted Racket-tail"
levels(full.fl$Hummingbird)[levels(full.fl$Hummingbird) %in% "Booted Racketail"]<-"Booted Racket-tail"
levels(full.fl$Hummingbird)[levels(full.fl$Hummingbird) %in% "White-whskered hermit"]<-"White-whiskered Hermit"
levels(full.fl$Hummingbird)[levels(full.fl$Hummingbird) %in% "Gorgeted Sunangel"]<-"Gorgetted Sunangel"
levels(full.fl$Hummingbird)[levels(full.fl$Hummingbird) %in% "Violet-tailed Slyph"]<-"Violet-tailed Sylph"


levels(full.fl$Hummingbird)

#Hummingbirds on which flowers
G.hum<-melt(table(full.fl$Full,full.fl$Hummingbird))
colnames(G.hum)<-c("Flower","Hummingbird","Observation")
G.hum[G.hum$Observation==0,"Observation"]<-NA


############################################
#Bring in Holger's transect hummingbird data
############################################

holger.hum<-read.csv("C:/Users/Ben/Dropbox/Thesis//Maquipucuna_SantaLucia//Data2013//csv//HolgerTransect_Hummingbirds.csv")
#Create full name flower column
holger.hum$Family<-as.factor(sapply(holger.hum$Family,function(x) .simpleCap(as.character(x))))
holger.hum$Genus<-as.factor(sapply(holger.hum$Genus,function(x) .simpleCap(as.character(x))))
holger.hum$Species<-as.factor(sapply(holger.hum$Species,tolower))
holger.hum$Full<-with(holger.hum,paste(Family,Genus,Species))

#Check hummingbird levels

head(holger.hum)
levels(holger.hum$Hummingbird.Species)[levels(holger.hum$Hummingbird.Species) %in% "Booted Racketail"] <- "Booted Racket-tail"

G.hhum<-melt(table(holger.hum$Full,holger.hum$Hummingbird))
colnames(G.hhum)<-c("Flower","Hummingbird","Observation")

#remove the empty observations
G.hhum[G.hhum$Observation==0,"Observation"]<-NA


#merge dataframes together
flwr_bird<-merge(G.hhum,G.hum,all=TRUE)

#remove the NA and non flower rows
flwr_bird<-flwr_bird[!is.na(flwr_bird$Observation),]

flwr_bird<-flwr_bird[!flwr_bird$Flower %in% levels(flwr_bird$Flower)[[1]],]

write.csv(flwr_bird,"Thesis/Maquipucuna_SantaLucia/Results/TransectBird_Flower.csv")

GH<-ggplot(flwr_bird,aes(x=Flower,y=Hummingbird,fill=Observation)) + geom_tile() + theme_bw() + scale_fill_continuous(high="red",na.value="white")
GH + theme(axis.text.x=element_text(angle=90))

#It actually seems like it would make most sense to export the data as rows rather as a contingency table
write.csv(full.fl[is.finite(full.fl$Hummingbird),],"Thesis/Maquipucuna_SantaLucia/Results/TransectHumRows.csv")


ggsave(filename="Thesis/Maquipucuna_SantaLucia/Results/Hummingbird_Genus.jpeg",height=8,width=11)
