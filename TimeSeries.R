##Time Series Analysis of Hummingbird Presences
require(stringr)
require(maptools)
require(ggplot2)
require(scales)
library(dplyr)
library(chron)

# ############Bring in temperature
 tdata<-read.csv("C:/Users/Ben/Dropbox/Thesis//Maquipucuna_SantaLucia/Results/Ibuttons/TempData.csv")
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
