##Time Series Analysis of Hummingbird Presences
require(stringr)
require(maptools)
require(ggplot2)
require(scales)

#If not run globally.

#setwd("C:/Users/Jorge/Dropbox/")


setwd("C:/Documents and Settings/Administrator/My Documents/Dropbox")

##Bird interaction matrix
dat<-read.csv("Thesis/Maquipucuna_SantaLucia/Results/Network/HummingbirdInteractions.csv")

#reformat date column
dat$DateP<-as.POSIXlt(dat$DateP)

#one mislabled year?
dat[which(dat$Date =="6/4/2012"),"DateP"]<-"2013-06-04"

dat[dat$Hummingbird %in% "Western Emerald","Hummingbird"]<-"Andean Emerald"

#Overall Month_Day and Elevation
p<-ggplot(dat,aes(y=ele,x=DateP)) + geom_point(size=3) + facet_wrap(~Hummingbird)
p<-p + scale_x_datetime(breaks = date_breaks("1 months"),labels = date_format("%b")) + geom_smooth(method="lm") + scale_y_continuous(breaks=seq(1300,2500,200),labels=seq(1300,2500,200)) + xlab("Month") + ylab("Elevation(m)")
p
ggsave("Thesis//Maquipucuna_SantaLucia/Results/DateElevation.jpeg",height=10,width=17,dpi=350)
ggsave("Thesis//Maquipucuna_SantaLucia/Results/DateElevation.svg",height=8,width=13,dpi=300)


p<-ggplot(dat,aes(y=ele,x=Hummingbird,fill=Hummingbird))
p + geom_boxplot() + coord_flip() + labs(y="Elevation(m)") + scale_fill_discrete(guide='none') + theme_bw()
#ggsave("Thesis//Maquipucuna_SantaLucia/Results/ElevationRanges.jpeg",height=8,width=13,dpi=300,units="in")

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
