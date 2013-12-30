##Time Series Analysis of Hummingbird Presences
require(stringr)
require(maptools)

#If not run globally.

setwd("C:/Users/Jorge/Dropbox/")
##Read in:

#GeospatialData
gps<-readShapePoints(fn="Thesis\\Maquipucuna_SantaLucia\\Data2013\\Shapefiles\\GPSshape.shp")

#################################
#Species Presence and Time
##################################

#Bring in Transect Data and Camera Data
dat<-read.csv("Thesis/Maquipucuna_SantaLucia/Results/Network/HummingbirdInteractions.csv",row.names=1)

#make a date column
dat$POSIXd<-NULL

dat$DateP<-sapply(dat$Date,function(x){
  #print(x)
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

dat$DateP<-as.POSIXlt(dat$DateP)

head(dat)

#What kind of records match?
dat[dat$ID %in% gps$name,]

#What kind of records dont' match?
dat[!dat$ID %in% gps$name,]$ID

#missing alot of records right now
print(paste("Missing Spatial Data",levels(factor(dat[!dat$ID %in% gps$name,]$ID))))

#need to create double label column, by date and ID?
#Merge data
dat.e<-merge(dat,gps,by.x="ID",by.y="name")

#elevation read as factor?, plus round to nearest 10?
dat.e$ele<-round(as.numeric(as.character(dat.e$ele)),-1)

#Overall Month_Day and Elevation
ggplot(dat.e,aes(y=ele,x=DateP,col=Hummingbird)) + geom_point(size=3) + scale_x_datetime() + facet_wrap(~Hummingbird)
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
