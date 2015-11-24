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

#bring in clade data
clades<-read.csv(paste(gitpath,"InputData//CladeList.txt",sep=""),header=FALSE)[,-1]
colnames(clades)<-c("Clade","Genus","Species","double","English")
clades<-clades[,1:5]

###Read in Flower Camera Dataset####
datf<-read.csv("C:/Users/Ben/Dropbox/Thesis/Maquipucuna_SantaLucia/Data2013/csv/FlowerVideoClean.csv",row.names=1)

#Get desired columns
datf<-datf[,colnames(datf) %in% c("ID","Video","Date","Iplant_Double","Flower","Time","Hummingbird","Sex","Temp","Pierce","lon","lat","ele")]

#Fix date format
datf$Month<-as.numeric(format(as.Date(datf$Date,"%m/%d/%Y"),"%m"))

#make a object, just to save typing
h<-levels(datf$Hummingbird)

#remove levels of flowerpiercers
datf<-datf[levels(datf$Hummingbird)[!str_detect(levels(datf$Hummingbird),"piercer")] %in% datf$Hummingbird,]

####Bring in interaction matrix for the flower transects, see FlowerTransects.R
transect.FL<-read.csv("C:/Users/Ben/Dropbox/Thesis/Maquipucuna_SantaLucia/Results/HummingbirdTransects/HumTransectRows.csv",row.names=1)

#make the columns as similiar as possible to videodata
colnames(transect.FL)<-c("GPS.ID","TransectID","Hummingbird","Date","Month","Transect_R","Iplant_Double","lat","lon","ele")

##############Data Imported#####################

#Bind in the transect rows to the bottom of datf?
datf<-as.data.frame(rbind_list(datf,transect.FL))
datf$Iplant_Double<-as.factor(datf$Iplant_Double)
datf$Hummingbird<-as.factor(datf$Hummingbird)

#fix spelling of violet tailed sylph
levels(datf$Hummingbird)[levels(datf$Hummingbird) %in%  "Violet-tailed Slyph"]<-"Violet-tailed Sylph"

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

#Take our any bad data
hdat<-droplevels(datf[!datf$Hummingbird %in% c("","NANA","UKWN","Ukwn","Western Emerald"),])

#one mislabled year?
hdat[which(hdat$Date =="6/4/2012"),"DateP"]<-"2013-06-04"

#Overall Month_Day and Elevation
p<-ggplot(hdat,aes(y=ele,x=DateP)) + geom_point(size=3) + facet_wrap(~Hummingbird,scales="free")
p<-p + scale_x_datetime(breaks = date_breaks("3 months"),labels = date_format("%b")) + scale_y_continuous(breaks=seq(1300,2500,200),labels=seq(1300,2500,200)) + xlab("Month") + ylab("Elevation(m)")
p + stat_smooth()
#ggsave("Thesis//Maquipucuna_SantaLucia/Results/DateElevation.jpeg",height=10,width=17,dpi=350)
#ggsave("Thesis//Maquipucuna_SantaLucia/Results/DateElevation.svg",height=8,width=13,dpi=300)

#
#Boxplot of elevation ranges
#add # of observations for each
keep<-names(which(table(hdat$Hummingbird) > 5))

hdat<-hdat[hdat$Hummingbird %in% keep,]

obs<-as.data.frame.array(table(hdat$Hummingbird))

#order 
humord<-hdat%>% select(Hummingbird,ele)%>% group_by(Hummingbird) %>% summarize(m=mean(ele,na.rm=T)) %>% arrange(m) %>% select(Hummingbird) 
hdat$Hummingbird<-factor(hdat$Hummingbird,levels=humord$Hummingbird)
p<-ggplot(hdat,aes(y=ele,x=Hummingbird,fill=Hummingbird)) + ylim(1300,2600)
p<-p + geom_boxplot(varwidth=TRUE) + coord_flip() + labs(y="Elevation(m)",x="") + scale_fill_discrete(guide='none') + theme_bw()
print(p)
ggsave("Thesis//Maquipucuna_SantaLucia/Results/ElevationRanges.jpg",dpi=600,height=7,width=10)

#write hummingbird interactions and remove empty cells
hdat<-hdat[!is.na(hdat$Iplant_Double) & !hdat$Iplant_Double=="",]

write.csv(hdat,"C:/Users/Ben/Dropbox/Thesis/Maquipucuna_SantaLucia/Results/Network/HummingbirdInteractions.csv")

#defined elevation groups for occupancy model
eleIndex<-hdat[,-18] %>% group_by(Hummingbird) %>% summarize(Low=quantile(ele,0.1,na.rm=T),m=mean(ele,na.rm=T),High=quantile(ele,0.9,na.rm=T))

#1 for low elevation, 2 for high elevation, 3 for both
#high elevation
eleIndex[eleIndex$Low > 1700,"Index"]<-2

#low elevation
eleIndex[eleIndex$High < 1700,"Index"]<-1

#The result are both
eleIndex[is.na(eleIndex$Index),"Index"]<-3

#view result
as.data.frame(eleIndex)

write.csv(eleIndex,paste(gitpath,"OutData/HummingbirdElevation.csv",sep=""))


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
