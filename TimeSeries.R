##Time Series Analysis of Hummingbird Presences
require(stringr)
require(maptools)
require(ggplot2)
require(scales)
#If not run globally.

setwd("C:/Users/Jorge/Dropbox/")
##Read in:
dat<-read.csv("Thesis/Maquipucuna_SantaLucia/Results/Network/HummingbirdInteractions.csv")

#reformat date column
dat$DateP<-as.POSIXlt(dat$DateP)

#one mislabled year?
dat[which(dat$Date =="6/4/2012"),"DateP"]<-"2013-06-04"


#Overall Month_Day and Elevation
p<-ggplot(dat,aes(y=ele,x=DateP,col=Hummingbird)) + geom_point(size=3) + facet_wrap(~Hummingbird)
p + scale_x_datetime(breaks = date_breaks("1 months"),labels = date_format("%b"))
ggsave("Thesis//Maquipucuna_SantaLucia/Results/DateElevation.svg",height=11,width=8,dpi=300)

############################
#Feeders
############################
#Create overall date stamp
dat$Time_Stamp<-as.POSIXct(chron(dates=as.character(dat$Date),dat$Time.Begin))

#Time and species occurence, facetted by elevation
ggplot(dat,aes(x=strptime(dat$Time.Begin,"%H:%M"),fill=Species)) + geom_histogram(position="dodge") + facet_wrap(~Elevation)
ggsave("Thesis//Maquipucuna_SantaLucia/Results/TimeofDayElevation.svg",height=11,width=8,dpi=300)

#Overall Month_Day and Elevation
ggplot(dat,aes(y=factor(Elevation),x=dat$Time_Stamp,col=Species)) + geom_point(size=3) + scale_x_datetime() + facet_wrap(~Species)
ggsave("Thesis//Maquipucuna_SantaLucia/Results/DateElevation.svg",height=11,width=8,dpi=300)

ggplot(dat,aes(x=dat$Time_Stamp,fill=Species)) + geom_histogram(position="dodge") + facet_wrap(~Elevation)
ggsave("Thesis//Maquipucuna_SantaLucia/Results/TimeofDayElevation.svg",height=11,width=8,dpi=300)
