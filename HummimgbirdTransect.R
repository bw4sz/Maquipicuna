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
