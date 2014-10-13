##Time Series Analysis of Hummingbird Presences
require(stringr)
require(maptools)
require(ggplot2)
require(scales)

#If not run globally.

#Set Dropbox Location
#setwd to dropbox
droppath<-"C:/Users/Ben/Dropbox/"
setwd(droppath)
#Set github path
gitpath<-"C:/Users/Ben/Documents/Maquipicuna/"

##Bird interaction matrix
dat<-read.csv("Thesis/Maquipucuna_SantaLucia/Results/Network/HummingbirdInteractions.csv")

#reformat date column
dat$DateP<-as.POSIXlt(dat$DateP)

#one mislabled year?
dat[which(dat$Date =="6/4/2012"),"DateP"]<-"2013-06-04"

dat[dat$Hummingbird %in% "Western Emerald","Hummingbird"]<-"Andean Emerald"

#Overall Month_Day and Elevation
p<-ggplot(dat,aes(y=ele,x=DateP)) + geom_point(size=3) + facet_wrap(~Hummingbird,scales="free")
p<-p + scale_x_datetime(breaks = date_breaks("3 months"),labels = date_format("%b")) + scale_y_continuous(breaks=seq(1300,2500,200),labels=seq(1300,2500,200)) + xlab("Month") + ylab("Elevation(m)")
p + stat_smooth()
ggsave("Thesis//Maquipucuna_SantaLucia/Results/DateElevation.jpeg",height=10,width=17,dpi=350)
ggsave("Thesis//Maquipucuna_SantaLucia/Results/DateElevation.svg",height=8,width=13,dpi=300)

#Boxplot of elevation ranges
#add # of observations for each
obs<-as.data.frame.array(table(dat$Hummingbird))

p<-ggplot(dat,aes(y=ele,x=Hummingbird,fill=Hummingbird))
p<-p + geom_boxplot(varwidth=TRUE) + coord_flip() + labs(y="Elevation(m)",x="") + scale_fill_discrete(guide='none') + theme_bw()
p
ggsave("Thesis//Maquipucuna_SantaLucia/Results/ElevationRanges.pdf",dpi=600)

############Bring in temperature
tdata<-read.csv("Thesis//Maquipucuna_SantaLucia/Results/Ibuttons/TempData.csv")



#########Doesn't quite work yet.
#Flowers per month
fltotal<-read.csv("Thesis/Maquipucuna_SantaLucia/Results/FlowerTransects/FlowerAvailability.csv",row.names=1)
fltotal$Month.n<-month.name[fltotal$Month]

monthtemp<-aggregate(tdata$Temp,list(tdata$elevation,tdata$Month,tdata$Year),mean)
colnames(monthtemp)<-c("Elevation","Month","Year","Temp")

#Overlay temperature and flowers
fltemp<-merge(monthtemp,fltotal,by.x=c("Month","Year"),by.y=c("Month.n","Year"))

#Standardize to max
fltemp$TempD<-fltemp$Temp/max(fltemp$Temp,na.rm=TRUE)
fltemp$Flower<-fltemp$TotalFlowers/max(fltemp$TotalFlowers,na.rm=TRUE)

#Get 2 week running average of temperature before transect

head(fltemp)
