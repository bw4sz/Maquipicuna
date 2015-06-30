#Flower Diversity Transects and Nectar
#Data Collected by Holger Beck,K. Lohman and B. Weinstein
#R script Ben Weinstein - Stony Brook University 7/7/2013
#Under Git repository - Maquipucuna

#Read in libraryd libraries
library(ggplot2)
library(reshape2)
library(maptools)
library(stringr)
library(dplyr)
library(plotKML)
library(reshape)
library(chron)
library(taxize)
library(splines)

#Set DropBox Working Directory
setwd("C:/Users/Ben/Dropbox/")

#Read in workspace if desired for quick access
#load("Thesis/Maquipucuna_SantaLucia/Results/FlowerTransect.Rdata")

#Read in Flower Transect Data from summer field season
fl<-read.csv(file="Thesis/Maquipucuna_SantaLucia/Data2013/csv/FlowerTransects.csv")

#one known id error, genus Fuchsia was mispelled fuschia, can't be rectified!

levels(fl$Genus)[levels(fl$Genus)%in% 'fuschia']<-'fuchsia'

#Load in holger's on going data
holger.fl<-read.csv("Thesis/Maquipucuna_SantaLucia/Data2013/csv/HolgerTransects.csv")

#Bring in holger transect data
holgerID<-read.csv("Thesis/Maquipucuna_SantaLucia/Data2013/csv/TransectIIDHolger.csv")

#Fix holger's ID elev columns, make them more general, transect delim
for (x in 1:6){
  holgerID[holgerID$Transect %in% x,"Elevation.Begin"]<-1100 + 200*x 
  holgerID[holgerID$Transect %in% x,"Elevation.End"]<-1300 + 200*x
}

#This is causing a real headache, for now just take the last two chracters of the year
holgerID$Date_F<-sapply(holgerID$Date,function(x){
  #grab the year
  d<-strsplit(as.character(x),split="/")[[1]]
  yr<-d[[3]]
  #get the last two characters
  yrsplit<-substr(yr, nchar(yr)-2+1, nchar(yr))
  dat_f<-as.Date(paste(paste(d[[1]],d[[2]],sep="/"),yrsplit,sep="/"),format="%d/%m/%y")
  return(as.character(dat_f))
})

holger.fl$Date_F<-sapply(holger.fl$Date,function(x){
  #grab the year
  d<-strsplit(as.character(x),split="/")[[1]]
  yr<-d[[3]]
  #get the last two characters
  yrsplit<-substr(yr, nchar(yr)-2+1, nchar(yr))
  dat_f<-as.Date(paste(paste(d[[1]],d[[2]],sep="/"),yrsplit,sep="/"),format="%d/%m/%y")
  return(as.character(dat_f))
})

#Create ID columns
holgerID$ID<-factor(paste(holgerID$Transect,holgerID$Date_F,sep="_"))
holger.fl$ID<-factor(paste(holger.fl$Transect,holger.fl$Date_F,sep="_"))
holgerID$ID[!holgerID$ID %in% holger.fl$ID]

#Which IDs are missing?
levels(droplevels(holger.fl$ID[!holger.fl$ID %in% holgerID$ID]))

levels(droplevels(holgerID$ID[!holgerID$ID %in% holger.fl$ID]))

#Merge summer dataset with annual data
holger.full<-merge(holger.fl,holgerID,"ID")

#Fix the column names and rbindfill
colnames(fl)<-c("Family","Genus","Species","Height","Flowers","Stalks","Inflorescences.Plants","GPS_ID","Accuracy","Hummingbird.Species","Photo","Transect.ID","Comment","Revise_Remove")

#Just get the desired columns and rename
holger.full<-holger.full[,colnames(holger.full) %in% c(colnames(holger.full)[1:15],"Elevation.Begin","Elevation.End") ]
colnames(holger.full)<-c("ID","Transect.ID","Date","Time","Family","Genus","Species","Height","Flowers","Stalks","Inflorescences.Plants","Hummingbird.Species","GPS_ID","Comment","Photo","Elevation.Begin","Elevation.End")

#To compare transects we need the transect id page
TID<-read.csv("Thesis/Maquipucuna_SantaLucia/Data2013/csv/TransectIID.csv")

#Select the flower transects
TID.f<-TID[TID$Type=="Flower",]

#Make factors both
fl$Transect.ID<-factor(fl$Transect.ID)
TID.f$TransectID<-factor(TID.f$TransectID)

which(!TID.f$TransectID %in% fl$Transect.ID)

#Missing level?
fl.id<-merge(fl,TID.f,by.x="Transect.ID",by.y="TransectID")

#make characters
holger.full$GPS_ID<-as.character(holger.full$GPS_ID)
full.fl<-plyr::rbind.fill(holger.full,fl.id)

#Set holger as observer
full.fl$Observer<-as.character(full.fl$Observer)
full.fl$Observer[is.na(full.fl$Observer)]<-"Holger"
full.fl$Observer<-factor(full.fl$Observer)

#Create elevation ID
full.fl$Transect_R<-factor(paste(full.fl$Elevation.Begin,full.fl$Elevation.End,sep="_"))

################
#Flower Taxonomy
################

#Fix any known ID mistakes (this is ugly.)
full.fl$Species<-as.character(full.fl$Species)
full.fl$Genus<-as.character(full.fl$Genus)
full.fl[full.fl$Genus %in%  "Hepiella" & full.fl$Species %in% "ulmifolia",c("Genus","Species")]<-c("Glossoloma","oblongicalyx")
full.fl$Species<-as.factor(full.fl$Species)
full.fl$Genus<-as.factor(full.fl$Genus)
#Go through a series of data cleaning steps, at the end remove all rows that are undesired
#Repeat for species

Families<-levels(factor(full.fl$Family))

tax<-gnr_resolve(names = Families,preferred_data_sources = c(3))
head(tax$preferred)

for (x in 1:nrow(full.fl)){
  y<-full.fl[x,]
  toMatch<-y$Family
  if(!toMatch %in% tax$preferred$submitted_name){next} else{
  full.fl[x,"Iplant_Family"]<-unique(tax$preferred[tax$preferred$submitted_name %in% toMatch,"matched_name"])
}}


#Repeat for genus species
Species<-levels(factor(paste(full.fl$Genus,full.fl$Species,sep=" ")))

#look up online, skip the blank
#remove species with just one word?

tax<-gnr_resolve(names = Species[-1], splitby=30,highestscore = T)

#Set the Species column
for (x in 1:nrow(full.fl)){
  y<-full.fl[x,]
  toMatch<-paste(y$Genus,y$Species,sep=" ")
  if(toMatch %in% tax$results$submitted_name){
  full.fl[x,"Iplant_Double"]<-unique(tax$results[tax$results$submitted_name %in% toMatch,"matched_name"]   )
} else {
  next
}}

#One known date error
#Remove levels that don'y have species names (see if they can be recovered)

numQ<-sapply(full.fl$Iplant_Double,function(x){
length(word(x))
})

#Write 
write.csv(levels(factor(full.fl$Iplant_Double)),"Thesis/Maquipucuna_SantaLucia/Results/FlowerTransects/Iplant_Names.txt")

#Error rows, though species that are not going to be 

error_rows<-full.fl[full.fl$Iplant_Double %in% "",]

#Family Genus Species, in future, create abreviations
fl.all<-melt(table(full.fl$Iplant_Double))
names(fl.all)<-c("Species","Count")
ggplot(fl.all,aes(Species,Count)) + geom_bar() + coord_flip() + theme_bw()
ggsave(filename="Thesis/Maquipucuna_SantaLucia/Results/FlowerTransects/SpeciesCount.jpeg",height=15,width=8)

#Taxonomy Check Complete
###########################

###########################
#Flower counts
###########################


#Create total flower column by multiplying the Flowers per stalk, by stalks and total inflorescens
#For now, remove any rows that have no flowers

#First need to take the average flowers per stalk, and all other columns
full.fl$mean_flowerStalk<-sapply(full.fl$Flowers,function(x) {
  mean(as.numeric(strsplit(as.character(as.list(x)[[1]]),",")[[1]]),na.rm=TRUE)
})

#remove any records that have no flowers info.
full.fl<-full.fl[!is.na(full.fl$mean_flowerStalk),]

##############################################
#Calculate Total Number of Flowers per Records
##############################################

full.fl$mean_Stalk<-sapply(full.fl$Stalks,function(x) {
  mean(as.numeric(strsplit(as.character(as.list(x)[[1]]),",")[[1]]))
})

full.fl$mean_inflorescences<-sapply(full.fl$Inflorescences.Plants,function(x) {
  mean(as.numeric(strsplit(as.character(as.list(x)[[1]]),",")[[1]]))
})

#Merge flower stalks
full.fl$Total_Flowers<-full.fl$mean_flowerStalk*full.fl$mean_Stalk*full.fl$mean_inflorescences

#Visualize total flowers by species
ggplot(data=full.fl,aes(Iplant_Double,Total_Flowers)) + geom_boxplot() + coord_flip()

################################################
#Number of Flowers at Each Elevation over Time
################################################

#Create month and year column
full.fl$month<-NA
full.fl$year<-NA
for (j in 1:nrow(full.fl)){
  x<-full.fl[j,]
  if(x[["Observer"]] %in% c("Karen","Ben")){
    full.fl[j,"month"]<-months(chron(as.character(x$Date)))
    full.fl[j,"year"]<-as.numeric(as.character(years(chron(as.character(x$Date)))))
    
  }
  if(x[["Observer"]] %in% "Holger"){
    full.fl[j,"month"]<-months(chron(as.character(x[x$Observer %in% "Holger","Date"]),format="d/m/y"))
    full.fl[j,"year"]<-as.numeric(as.character(years(chron(as.character(x[x$Observer %in% "Holger","Date"]),format="d/m/y"))))
  }
}

#year column

#Create month column
full.fl$Year<-NA
for (j in 1:nrow(full.fl)){
  x<-full.fl[j,]
  if(x[["Observer"]] %in% c("Karen","Ben")){
    full.fl[j,"Year"]<-as.character(years(chron(as.character(x$Date))))
  }
  if(x[["Observer"]] %in% "Holger"){
    full.fl[j,"Year"]<-as.character(years(chron(as.character(x[x$Observer %in% "Holger","Date"]),format="d/m/y")))
  }
}

#There is the one unique transect where holger did the transect with us. 
head(full.fl[is.na(full.fl$month),])

#plot total flowers over time
fl.totals<-aggregate(full.fl$Total_Flowers,list(full.fl$Transect_R,full.fl$month,full.fl$Date,full.fl$Year),sum,na.rm=TRUE)
colnames(fl.totals)<-c("Elev","Month","Date","Year","TotalFlowers")

#Write data to file
write.csv(full.fl,"Thesis/Maquipucuna_SantaLucia/Results/FlowerTransects/CleanedHolgerTransect.csv")

#Make month abbreviation column, with the right order
fl.totals$Month.a<-factor(month.abb[fl.totals$Month],month.abb[c(1:12)])

#Make year factor column
fl.totals$Year<-as.factor(fl.totals$Year)


####################
##Transect Counts
####################

head(full.fl)

ttotal<-group_by(full.fl,Year,month,Transect_R) %>% summarize(Date=length(unique(Date)))


#Month without elevation
p<-ggplot(fl.totals,aes(col=Elev,y=log(TotalFlowers),x=Month.a,shape=Year)) + geom_point(size=3) + theme_bw() + labs(col="Elevation") + facet_wrap(~Year,nrow=3) + stat_smooth(aes(group=1))
p + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylab("# of Hummingbird Visited Flowers") + xlab("Elevation Range (m)") #+ scale_color_brewer(palette="Paired") 
p<-ggplot(fl.totals,aes(x=Elev,TotalFlowers,color=Year)) + geom_point(size=3)  + stat_smooth(aes(group=Year),method="loess",se=FALSE) + facet_wrap(~Month.a,nrow=2,scales="free_y") + theme_bw() + labs(col="Month")
p + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylab("# of Hummingbird Visited Flowers") + xlab("Elevation Range (m)")
ggsave(filename="Thesis/Maquipucuna_SantaLucia/Results/FlowerTransects/FlowerMonths.jpeg",height=8,width=10)

#Flowers at each elevation over time
ggplot(fl.totals,aes(x=Month.a,TotalFlowers,col=Year)) + geom_point(size=3) + theme_bw()  + geom_smooth(aes(group=Year)) + facet_wrap(~Elev,scales="free_x") + ylab("Hummingbird Visited Flowers") + xlab("Month") + labs(col="Transect Elevation Range")
ggsave(filename="Thesis/Maquipucuna_SantaLucia/Results/FlowerTransects/FlowerElevations.jpeg",height=8,width=10,dpi=300)

#Flowers at each elevation over time without Karen's data
print(ggplot(fl.totals[,],aes(x=Month.a,log(TotalFlowers),shape=Year)) + geom_point(size=3) + stat_smooth(aes(group=1),method="glm", family="quasipoisson", formula = y ~ ns(x, 3)) + theme_bw()  + ylab("Available Flowers") + xlab("Month") + facet_wrap(~Elev,scales="free_y") )
ggsave(filename="Thesis/Maquipucuna_SantaLucia/Results/FlowerTransects/FlowerElevationsHolger.jpeg",height=8,width=14,dpi=300)

#Write flower totals
write.csv(fl.totals[,],"Thesis/Maquipucuna_SantaLucia/Results/FlowerTransects/FlowerAvailability.csv")

topf<-dplyr::group_by(full.fl,Iplant_Double) %>% 
  dplyr::summarize(t=sum(Total_Flowers,na.rm=T)) %>% 
  dplyr::arrange(desc(t)) %>% filter(t>quantile(t,0.75,na.rm=T)) %>% 
  dplyr::select(Iplant_Double) %>% filter(!Iplant_Double=="Palicourea")
  
topff<-full.fl[full.fl$Iplant_Double %in% topf$Iplant_Double,]

topff<-dplyr::group_by(topff,Iplant_Double,month,Year)  %>%
  dplyr::summarize(Count=sum(Total_Flowers)) %>% 
  dplyr::group_by(Iplant_Double,Year) %>%
  dplyr::mutate(Index=round(Count/max(Count),2)) %>% 
  dplyr::mutate(monthA=month.abb[month]) 

topff$monthA<-factor(topff$monthA,levels=month.abb)

p<-ggplot(topff[!is.na(topff$Iplant_Double),],aes(x=monthA,y=log(Count),col=Year)) + geom_point(size=3) + theme_bw() + facet_wrap(~Iplant_Double,ncol=4,scales="free_y")
p+ geom_smooth(aes(group=Iplant_Double),se=F)  + labs(x="Month") 
print(p+ggtitle("Phenology of Most Common Flowers"))
ggsave("C:/Users/Ben/Dropbox/Thesis/Maquipucuna_SantaLucia/Results/Phenology.svg",height=9,width=15)


#Write cleaned flower transect data
save.image("C:/Users/Ben/Dropbox/Thesis/Maquipucuna_SantaLucia/Results/FlowerTransect.Rdata")
