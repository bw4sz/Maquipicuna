##Time Series Analysis of Hummingbird Presences
require(stringr)
require(maptools)
require(ggplot2)
require(scales)
#If not run globally.

#setwd("C:/Users/Jorge/Dropbox/")
setwd(droppat)
##Read in:
dat<-read.csv("Thesis/Maquipucuna_SantaLucia/Results/Network/HummingbirdInteractions.csv")

#reformat date column
dat$DateP<-as.POSIXlt(dat$DateP)

#one mislabled year?
dat[which(dat$Date =="6/4/2012"),"DateP"]<-"2013-06-04"

dat[dat$Hummingbird %in% "Western Emerald","Hummingbird"]<-"Andean Emerald"

#Overall Month_Day and Elevation
p<-ggplot(dat,aes(y=ele,x=DateP,col=Hummingbird)) + geom_point(size=3) + facet_wrap(~Hummingbird)
p + scale_x_datetime(breaks = date_breaks("1 months"),labels = date_format("%b")) + geom_smooth(method="lm") + scale_y_continuous(breaks=seq(1300,2500,200),labels=seq(1300,2500,200)) + xlab("Month") + ylab("Elevation(m)")
print(p)
ggsave("Thesis//Maquipucuna_SantaLucia/Results/DateElevation.jpeg",height=10,width=17,dpi=350)
ggsave("Thesis//Maquipucuna_SantaLucia/Results/DateElevation.svg",height=8,width=13,dpi=300)

#Just the Summer
p<-ggplot(dat[dat$Month %in% c(6,7,8),],aes(y=ele,x=DateP,col=Hummingbird)) + geom_point(size=3) + facet_wrap(~Hummingbird)
p<-p+ scale_y_continuous(breaks=seq(1300,2500,200),labels=seq(1300,2500,200),limits=c(1300,2500))
p<-p + scale_x_datetime(breaks = date_breaks("1 months"),labels = date_format("%b")) 
p+ geom_smooth(method="lm")  
print(p) 
ggsave("Thesis//Maquipucuna_SantaLucia/Results/DateElevation.svg",height=11,width=8,dpi=300)

#Range by months

#p<-ggplot(dat,aes(y=ele,x=Month,col=Hummingbird)) + geom_point() + facet_wrap(~Hummingbird) + geom_boxplot(aes(group=Month))
#p
#p + scale_x_datetime(breaks = date_breaks("1 months"),labels = date_format("%b")) + stat_boxplot(width=)
# ############################
# #Feeders
# ############################
# #Create overall date stamp
# dat$Time_Stamp<-as.POSIXct(chron(dates=as.character(dat$Date),dat$Time.Begin))
# 
# #Time and species occurence, facetted by elevation
# ggplot(dat,aes(x=strptime(dat$Time.Begin,"%H:%M"),fill=Species)) + geom_histogram(position="dodge") + facet_wrap(~Elevation)
# ggsave("Thesis//Maquipucuna_SantaLucia/Results/TimeofDayElevation.svg",height=11,width=8,dpi=300)
# 
# #Overall Month_Day and Elevation
# ggplot(dat,aes(y=factor(Elevation),x=dat$Time_Stamp,col=Species)) + geom_point(size=3) + scale_x_datetime() + facet_wrap(~Species)
# ggsave("Thesis//Maquipucuna_SantaLucia/Results/DateElevation.svg",height=11,width=8,dpi=300)
# 
# ggplot(dat,aes(x=dat$Time_Stamp,fill=Species)) + geom_histogram(position="dodge") + facet_wrap(~Elevation)
# ggsave("Thesis//Maquipucuna_SantaLucia/Results/TimeofDayElevation.svg",height=11,width=8,dpi=300)
