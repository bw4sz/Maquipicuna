##Time Series Analysis of Hummingbird Presences
require(stringr)
require(maptools)
require(ggplot2)
require(scales)
library(plyr)
library(dplyr)
library(chron)

#Set Dropbox Location
#setwd to dropbox
droppath<-"C:/Users/Ben/Dropbox/"
setwd(droppath)
#Set github path
gitpath<-"C:/Users/Ben/Documents/Maquipicuna/"

###Read in Flower Camera Dataset####
datf<-read.csv("C:/Users/Ben/Dropbox/Thesis/Maquipucuna_SantaLucia/Data2013/csv/FlowerVideoClean.csv",row.names=1)

#Get desired columns
datf<-datf[,colnames(datf) %in% c("ID","Video","Date","Iplant_Double","Time","Hummingbird","Sex","Temp","Pierce","lon","lat","ele")]

#Fix date format
datf$Month<-as.numeric(format(as.Date(datf$Date,"%m/%d/%Y"),"%m"))

####Bring in interaction matrix for the flower transects, see FlowerTransects.R
transect.FL<-read.csv("C:/Users/Ben/Dropbox/Thesis/Maquipucuna_SantaLucia/Results/HummingbirdTransects/HumTransectRows.csv",row.names=1)

#make the columns as similiar as possible to videodata
colnames(transect.FL)<-c("GPS.ID","TransectID","Hummingbird","Date","Month","Transect_R","Iplant_Double","lat","lon","ele")

transect.FL$Iplant_Double<-gsub("_"," ",transect.FL$Iplant_Double)
##############Data Imported#####################

#Bind in the transect rows to the bottom of datf?
datf<-as.data.frame(rbind_list(datf,transect.FL))

datf$Iplant_Double<-as.factor(datf$Iplant_Double)
############################################
##############Data Import Complete##########
############################################

#Create Universal Date Stamp

datf$DateP<-sapply(datf$Date,function(x){
  if(is.na(x)){
    return(NA)
  }
  if(str_detect(x,"-")){
    toR<-as.character(strptime(x,"%Y-%m-%d"))
    #print(toR)
    return(toR)
  }
  
  if(str_detect(x,"/")){
    toR<-as.character(strptime(x,format="%m/%d/%Y"))
    #print(toR)
    return(toR)
  }
})

datf$DateP<-as.POSIXlt(datf$DateP)

# year position
datf$Year<-years(datf$DateP)

#one error
datf[datf$Year %in% 2012,"Year"]<-2013

###########################
#Hummingbird Data Cleaning 
###########################
.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

#Caps Hummingbird
datf$Hummingbird<-factor(sapply(datf$Hummingbird,function(x) {.simpleCap(as.character(x))}))

#make a object, just to save typing
h<-levels(datf$Hummingbird)

missp<-h[!h %in% clades$English]

paste("misspelled levels",missp)
h[h %in% missp]

spellC<-c("Booted Racket-tail","Green-crowned Woodnymph","Rufous-tailed Hummingbird","UKWN","UKWN","UKWN","Violet-tailed Sylph","White-sided Flowerpiercer")

paste("Spelling Correction",spellC)

h[h %in% missp]<-spellC

head(clades)
#can taxize do english names? 

levels(datf$Hummingbird) <- h

#Take our any bad data
dat_e<-droplevels(datf[!datf$Hummingbird %in% c("","NANA","UKWN","Ukwn","Western Emerald"),])
# 
# # #Remove out piercing events for now?
# # table(datf$Pierce)
# # datPierce<-dat_e[dat_e$Pierce%in% c("Yes","YES","y","Y"),]
# 
# #Piercing over time
# ggplot(datPierce,aes(x=DateP,y=Hummingbird)) + geom_point(size=3)
# 
# dat_e<-dat_e[!dat_e$Pierce %in% c("Yes","YES","y","Y"),]

#Drop any unused factors?
dat_e<-droplevels(dat_e)

#Drop any observations without plants
dat_e<-droplevels(dat_e[!dat_e$Iplant_Double %in% c("",NA),])

#drop species without double name

sp_l<-levels(dat_e$Iplant_Double)

l<-sapply(sp_l,function(x){
  str2 <- gsub(' {2,}',' ',x)
  length(strsplit(str2,' ')[[1]])
})

sp_r<-names(which(l==1))

hdat<-droplevels(dat_e[!dat_e$Iplant_Double %in% sp_r,])

#label Camera and Transect
hdat[!is.na(hdat$ID),"Type"]<-"Camera"
hdat[is.na(hdat$ID),"Type"]<-"Transect"

write.csv(hdat,"C:/Users/Ben/Dropbox/Thesis/Maquipucuna_SantaLucia/Results/Network/HummingbirdInteractions.csv")

#reformat date column
hdat$DateP<-as.POSIXlt(hdat$DateP)

#one mislabled year?
hdat[which(hdat$Date =="6/4/2012"),"DateP"]<-"2013-06-04"

hdat[hdat$Hummingbird %in% "Western Emerald","Hummingbird"]<-"Andean Emerald"

#Overall Month_Day and Elevation
p<-ggplot(hdat,aes(y=ele,x=DateP)) + geom_point(size=3) + facet_wrap(~Hummingbird,scales="free")
p<-p + scale_x_datetime(breaks = date_breaks("3 months"),labels = date_format("%b")) + scale_y_continuous(breaks=seq(1300,2500,200),labels=seq(1300,2500,200)) + xlab("Month") + ylab("Elevation(m)")
p + stat_smooth()
ggsave("Thesis//Maquipucuna_SantaLucia/Results/DateElevation.jpeg",height=10,width=17,dpi=350)
#ggsave("Thesis//Maquipucuna_SantaLucia/Results/DateElevation.svg",height=8,width=13,dpi=300)

#Boxplot of elevation ranges
#add # of observations for each
obs<-as.data.frame.array(table(hdat$Hummingbird))

p<-ggplot(hdat,aes(y=ele,x=Hummingbird,fill=Hummingbird)) + ylim(1300,2600)
p<-p + geom_boxplot(varwidth=TRUE) + coord_flip() + labs(y="Elevation(m)",x="") + scale_fill_discrete(guide='none') + theme_bw()
print(p)
ggsave("Thesis//Maquipucuna_SantaLucia/Results/ElevationRanges.pdf",dpi=600)



# ############Bring in temperature
# tdata<-read.csv("Thesis//Maquipucuna_SantaLucia/Results/Ibuttons/TempData.csv")
# 
# 
# 
# #########Doesn't quite work yet.
# #Flowers per month
# fltotal<-read.csv("Thesis/Maquipucuna_SantaLucia/Results/FlowerTransects/FlowerAvailability.csv",row.names=1)
# fltotal$Month.n<-month.name[fltotal$Month]
# 
# monthtemp<-aggregate(tdata$Temp,list(tdata$elevation,tdata$Month,tdata$Year),mean)
# colnames(monthtemp)<-c("Elevation","Month","Year","Temp")
# 
# #Overlay temperature and flowers
# fltemp<-merge(monthtemp,fltotal,by.x=c("Month","Year"),by.y=c("Month.n","Year"))
# 
# #Standardize to max
# fltemp$TempD<-fltemp$Temp/max(fltemp$Temp,na.rm=TRUE)
# fltemp$Flower<-fltemp$TotalFlowers/max(fltemp$TotalFlowers,na.rm=TRUE)
# 
# #Get 2 week running average of temperature before transect
# 
# head(fltemp)
