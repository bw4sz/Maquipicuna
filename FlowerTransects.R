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
require(rPlant)

#Set DropBox Working Directory
setwd("C:/Users/Ben/Dropbox/")

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

#Write 
write.csv(levels(factor(full.fl$Iplant_Double)),"Thesis/Maquipucuna_SantaLucia/Results/FlowerTransects/Iplant_Names.txt")

#Error rows, though species that are not going to be 
head(full.fl)

error_rows<-full.fl[full.fl$Iplant_Double %in% "",]
print("Error_Rows")
print(rownames(error_rows))

# GS<-levels(factor(full.fl$Iplant_Double))
# 
# uids<-get_uid(GS)
# 
# out <- classification(uids)
# taxize_N<-sapply(out, function(x){
#   if(is.na(x)){return(NA)}
#   print(x)
#   paste(x[x$Rank %in% "family","ScientificName"],x[x$Rank %in% "species","ScientificName"])
# })
# 
# data.frame(GS,taxize_N)
# 
# taxize_Name<-sapply(out, function(x){
#   if(is.na(x)){return(NA)}
#   x[x$Rank %in% "family","ScientificName"]
# })
# 
# data.frame(levels(factor(full.fl$Family)),taxize_Name)
# 
#Family Genus Species, in future, create abreviations
fl.all<-melt(table(full.fl$Iplant_Double))
names(fl.all)<-c("Species","Count")
ggplot(fl.all,aes(Species,Count)) + geom_bar() + coord_flip() + theme_bw()
ggsave(filename="Thesis/Maquipucuna_SantaLucia/Results/FlowerTransects/SpeciesCount.jpeg",height=15,width=8)

###########################
#Taxonomy Complete
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

full.fl$Total_Flowers<-full.fl$mean_flowerStalk*full.fl$mean_Stalk*full.fl$mean_inflorescences
hist(full.fl$Total_Flowers)

#Visualize total flowers by species
#ggplot(data=full.fl,aes(Full,Total_Flowers)) + geom_boxplot() + coord_flip()

#visualize height by species
#ggplot(data=full.fl,aes(Full,as.numeric(Height))) + geom_boxplot() + coord_flip()

################################################
#Number of Flowers at Each Elevation over Time
################################################

#Create month column
full.fl$month<-NA
for (j in 1:nrow(full.fl)){
  x<-full.fl[j,]
  if(x[["Observer"]] %in% c("Karen","Ben")){
    full.fl[j,"month"]<-months(chron(as.character(x$Date)))
  }
    if(x[["Observer"]] %in% "Holger"){
      full.fl[j,"month"]<-months(chron(as.character(x[x$Observer %in% "Holger","Date"]),format="d/m/y"))
    }
  }

#There is the one unique transect where holger did the transect with us. 
head(full.fl[is.na(full.fl$month),])

#plot total flowers over time?
fl.totals<-aggregate(full.fl$Total_Flowers,list(full.fl$Transect_R,full.fl$month,full.fl$Date),sum)
colnames(fl.totals)<-c("Elev","Month","Date","TotalFlowers")

##Flowers per month and elevation
p<-ggplot(fl.totals,aes(x=Elev,TotalFlowers,col=as.factor(Month))) + geom_point(size=3) + geom_smooth(aes(group=Month)) + facet_wrap(~Month,nrow=2) + theme_bw() + labs(col="Month")
p + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
ggsave(filename="Thesis/Maquipucuna_SantaLucia/Results/FlowerTransects/FlowerMonths.jpeg",height=8,width=10)

#Flowers at each elevation over time
ggplot(fl.totals,aes(x=Month,TotalFlowers,col=Elev)) + geom_point(size=3) + theme_bw()  + geom_smooth(aes(group=Elev)) + facet_wrap(~Elev,scales="free_x") + scale_y_continuous(limits=c(0,5500),breaks=seq(0,5000,1000))
ggsave(filename="Thesis/Maquipucuna_SantaLucia/Results/FlowerTransects/FlowerElevations.jpeg",height=8,width=10)

#Brief look at time series and taxonomy
tax<-aggregate(full.fl[,c("Iplant_Double","Iplant_Genus","Family")],list(full.fl$month,full.fl$Transect_R,full.fl$Date),function(x){
  nlevels(factor(x))})

colnames(tax)[1:3]<-c("month","Elev","Date")
tax.m<-melt(tax,id.var=c("month","Elev","Date"))

#Taxonomy over time
ggplot(tax.m,aes(month,value,col=variable)) + geom_point(size=2) + geom_smooth(aes(group=variable),se=FALSE) + facet_grid(.~Elev) + theme_bw() + labs(col="Tax. Level")
ggsave(filename="Thesis/Maquipucuna_SantaLucia/Results/FlowerTransects/TimeSeriesTaxonomy.jpeg",height=5,width=10)

#Genus to Species Ratios
ggplot(tax,aes(x=month,col=Elev,y=Iplant_Double/Iplant_Genus)) + geom_point() + geom_smooth(aes(group=1)) + xlab("Month") + facet_grid(~Elev,margins=TRUE) + theme_bw()
ggsave(filename="Thesis/Maquipucuna_SantaLucia/Results/FlowerTransects/TimeSeriesGSratio.jpeg",height=7,width=12)

#This needs to be abundane based...

# ##############################################
# #Read in Spatial Data, still needs to be fixed. 
# ##############################################
# 
# #Read and convert gpx points to a single dataframe and save it as a shapefile
# f<-list.files("Holger/Transect_Protocol_Holger/WayPoints/",full.names=TRUE)
# 
# #loop through input files and find the errors. 
# gpx<-list()
# for (x in 1:length(f)){
#   print(x)
#   try(
#     gpx[[x]]<-readGPX(f[x],waypoints=TRUE)$waypoints)
# }
# 
# ##Repeat for Karen's GPS data, label Karen
# f<-list.files("F:\\KarenGPS\\KarenFirstgps/",full.names=TRUE)
# 
# gpx2<-list()
# for (x in 1:length(f)){
#   print(x)
#   try(
#     gpx2[[x]]<-readGPX(f[x],waypoints=TRUE)$waypoints)
# }
# 
# #Bind together the days that contain data
# #Label Observer
# holger.gps<-data.frame(rbind.fill(gpx[sapply(gpx,class)=="data.frame"]),Observer="Holger")
# karen.gps<-data.frame(rbind.fill(gpx2[sapply(gpx2,class)=="data.frame"]),Observer="Karen")
# 
# #Combine data
# gpx.dat<-rbind.fill(holger.gps,karen.gps)
# 
# full.fl$GPS_ID<-as.numeric(full.fl$GPS_ID)
# 
# #Match each point with an elevation
# fl.elev<-merge(full.fl,gpx.dat,by.x=c("GPS_ID","Observer"),by.y=c("name","Observer"))
# 
# #How records were in fl, but not matched
# full.fl[!full.fl$GPS_ID %in% fl.elev$GPS_ID,]$GPS_ID
# 
# 
# #spatially
# ggplot(fl.elev,aes(x=coords.x1,y=coords.x2,col=Total_Flowers),size=200) + theme_bw() + geom_point() + scale_color_continuous(high="red",low="blue",limits=c(0,200))
# 
# #How many flowers at each elevation
# ggplot(fl.elev,aes(ele,y=Total_Flowers)) + geom_line() + geom_point(aes(color=Family),size=2) + theme_bw() 
# 
# p<-ggplot(fl.elev,aes(x=ele,y=Total_Flowers,col=Species),size=2) + geom_point() + theme_bw() + facet_wrap(~Family,scales='free_y') + guides(color="none")
# p+stat_smooth(method='lm',aes(group=Family))

save.image("Thesis/Maquipucuna_SantaLucia/Results/FlowerTransect.Rdata")
