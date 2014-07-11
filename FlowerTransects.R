#Flower Diversity Transects and Nectar
#Data Collected by Holger Beck,K. Lohman and B. Weinstein
#R script Ben Weinstein - Stony Brook University 7/7/2013
#Under Git repository - Maquipucuna

#Read in required libraries
require(ggplot2)
require(reshape2)
require(maptools)
library(plyr)
require(plotKML)
require(reshape)
require(chron)
require(taxize)

#Set DropBox Working Directory
setwd("C:/Users/Jorge/Dropbox/")

#Read in workspace if desired for quick access
#load("Thesis/Maquipucuna_SantaLucia/Results/FlowerTransect.Rdata")

#Read in Flower Transect Data from summer field season
fl<-read.csv(file="Thesis/Maquipucuna_SantaLucia/Data2013/csv/FlowerTransects.csv")

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

###############################
#NEEDS TO ADDRESS
which(!TID.f$TransectID %in% fl$Transect.ID)

#Missing level?
fl.id<-merge(fl,TID.f,by.x="Transect.ID",by.y="TransectID")

#How many rows did we lose?
dim(fl)
dim(fl.id)

head(fl.id)

#make characters
holger.full$GPS_ID<-as.character(holger.full$GPS_ID)
full.fl<-rbind.fill(holger.full,fl.id)

#Set holger as observer
full.fl$Observer<-as.character(full.fl$Observer)
full.fl$Observer[is.na(full.fl$Observer)]<-"Holger"
full.fl$Observer<-factor(full.fl$Observer)

head(full.fl)

#Create elevation ID
full.fl$Transect_R<-factor(paste(full.fl$Elevation.Begin,full.fl$Elevation.End,sep="_"))

################
#Flower Taxonomy
################


#Go through a series of data cleaning steps, at the end remove all rows that are undesired
#Repeat for species

#one really bad mispelling couldn't be fixed
full.fl[full.fl$Family %in% "Hydranganceae","Family"]<-"Hydrangeacea"

#Set the Family column
for (x in 1:nrow(full.fl)){
  y<-full.fl[x,]
  full.fl[x,"Family"]<-levels(droplevels(Fam_Result[Fam_Result$Families %in% y$Family,"iplant_names"] ))   
}

#Repeat for genus
Genus<-levels(factor(full.fl$Genus))
iplant_names<-ResolveNames(names=Genus)
CompareNames(Genus,iplant_names)

full.fl[full.fl$Family %in% "???","Family"]<-""

Families<-levels(factor(full.fl$Family))

tax<-tnrs(query = Families, source = "iPlant_TNRS")

#Set the Species column
for (x in 1:nrow(full.fl)){
  print(x)
  y<-full.fl[x,]
  toMatch<-y$Family
  if(!toMatch %in% tax$submittedname){next} else{
  full.fl[x,"Iplant_Family"]<-unique(tax[tax$submittedname %in% toMatch,"acceptedname"])
}}


#Repeat for genus species
Species<-levels(factor(paste(full.fl$Genus,full.fl$Species,sep=" ")))

#look up online, skip the blank
tax<-tnrs(query = Species[-1], source = "iPlant_TNRS")

#Set the Species column
for (x in 1:nrow(full.fl)){
  y<-full.fl[x,]
  toMatch<-paste(y$Genus,y$Species,sep=" ")
  if(toMatch %in% tax$submittedname){
  full.fl[x,"Iplant_Double"]<-unique(tax[tax$submittedname %in% toMatch,"acceptedname"]   )
} else {
  next
}}

#Fix any known ID mistakes
full.fl[full.fl$Iplant_Double %in% "Heppiella_ulmifolia","Iplant_Double"]<-"Glossoloma_oblongicalyx"

#Final levels
print(paste("Final Flower Species:", levels(factor(full.fl$Iplant_Double))))

#Write 
write.csv(levels(factor(full.fl$Iplant_Double)),"Thesis/Maquipucuna_SantaLucia/Results/FlowerTransects/Iplant_Names.txt")

#Error rows, though species that are not going to be 
head(full.fl)

error_rows<-full.fl[full.fl$Iplant_Double %in% "",]
print("Error_Rows")
print(rownames(error_rows))

#Family Genus Species, in future, create abreviations
fl.all<-melt(table(full.fl$Iplant_Double))
names(fl.all)<-c("Species","Count")
ggplot(fl.all,aes(Species,Count)) + geom_bar() + coord_flip() + theme_bw()
ggsave(filename="Thesis/Maquipucuna_SantaLucia/Results/FlowerTransects/SpeciesCount.jpeg",height=15,width=8)

###########################
#Taxonomy Check Complete
###########################

###########################
#Flower counts
###########################

print("FlowerCounts")

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

#visualize height by species
#ggplot(data=full.fl,aes(Full,as.numeric(Height))) + geom_boxplot() + coord_flip()

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
fl.totals<-aggregate(full.fl$Total_Flowers,list(full.fl$Transect_R,full.fl$month,full.fl$Date,full.fl$Year),sum)
colnames(fl.totals)<-c("Elev","Month","Date","Year","TotalFlowers")

#One date error
#Write data to file
write.csv(full.fl,"Thesis/Maquipucuna_SantaLucia/Results/FlowerTransects/CleanedHolgerTransect.csv")

#Make month abbreviation column, with the right order
fl.totals$Month.a<-factor(month.abb[fl.totals$Month],month.abb[c(1:12)])

#Make year factor column
fl.totals$Year<-as.factor(fl.totals$Year)

##Flowers per month and elevation
p<-ggplot(fl.totals,aes(x=Elev,TotalFlowers,col=Year)) + geom_point(size=3)  + stat_smooth(aes(group=interaction(Month,Year)),method="loess",se=FALSE) + facet_wrap(~Month,nrow=2) + theme_bw() + labs(col="Month")
p + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylab("Flowers") + xlab("Elevation Range (m)") + scale_color_brewer(palette="Set2")
ggsave(filename="Thesis/Maquipucuna_SantaLucia/Results/FlowerTransects/FlowerMonths.jpeg",height=8,width=10,dpi=300)

#Month without elevation
p<-ggplot(fl.totals,aes(col=Elev,y=TotalFlowers,x=Month.a,shape=Year)) + geom_point(size=3) + theme_bw() + labs(col="Elevation") + facet_wrap(~Year,nrow=2) + stat_smooth(aes(group=1))
p + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylab("# of Hummingbird Visited Flowers") + xlab("Elevation Range (m)") #+ scale_color_brewer(palette="Paired") 
p<-ggplot(fl.totals,aes(x=Elev,TotalFlowers,color=Year)) + geom_point(size=3)  + stat_smooth(aes(group=Year),method="loess",se=FALSE) + facet_wrap(~Month.a,nrow=2,scales="free_y") + theme_bw() + labs(col="Month")

p + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylab("# of Hummingbird Visited Flowers") + xlab("Elevation Range (m)")
ggsave(filename="Thesis/Maquipucuna_SantaLucia/Results/FlowerTransects/FlowerMonths.jpeg",height=8,width=10)


#Flowers at each elevation over time
ggplot(fl.totals,aes(x=Month.a,TotalFlowers,col=Year)) + geom_point(size=3) + theme_bw()  + geom_smooth(aes(group=Year)) + facet_wrap(~Elev,scales="free_x") + ylab("Hummingbird Visited Flowers") + xlab("Month") + labs(col="Transect Elevation Range")
ggsave(filename="Thesis/Maquipucuna_SantaLucia/Results/FlowerTransects/FlowerElevations.jpeg",height=8,width=10,dpi=300)

########################
#Taxonomic analysis
#########################

#Split by flower families
#plot total flowers over time?
fl.totalsT<-aggregate(full.fl$Total_Flowers,list(full.fl$month,full.fl$Date,full.fl$year,full.fl$Family),sum)
colnames(fl.totalsT)<-c("Month","Date","Year","Family","TotalFlowers")

fl.totalsT$Month.a<-factor(month.abb[fl.totalsT$Month],month.abb[c(1:12)])

#which are the top families
familyS<-aggregate(fl.totalsT$TotalFlowers,by=list(fl.totalsT$Family),sum,na.rm=TRUE)

topF<-as.character(familyS[order(familyS$x,decreasing=TRUE),][1:6,]$Group.1)

p<-ggplot(fl.totalsT[fl.totalsT$Family %in% topF,],aes(x=Month.a,TotalFlowers,col=as.factor(Year))) + geom_point(size=3)  + facet_grid(Year~Family,scales="free_y")
p<-p + stat_smooth(aes(group=Year), method="loess",se=FALSE)  + theme_bw() + labs(col="Month")
p + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylab("Flowers") + xlab("Elevation Range (m)") 
ggsave(filename="Thesis/Maquipucuna_SantaLucia/Results/FlowerTransects/TaxonomyTimeSeries.jpeg",height=8,width=10,dpi=300)

ggplot(fl.totals,aes(x=as.factor(Month),TotalFlowers,col=Year)) + geom_point(size=3) + theme_bw()  + geom_smooth(aes(group=Elev)) + facet_wrap(~Elev,scales="free_x") + scale_y_continuous(limits=c(0,5500),breaks=seq(0,5000,1000))
ggsave(filename="Thesis/Maquipucuna_SantaLucia/Results/FlowerTransects/FlowerElevations.jpeg",height=8,width=10)

#Write cleaned flower transect data
save.image("Thesis/Maquipucuna_SantaLucia/Results/FlowerTransect.Rdata")
