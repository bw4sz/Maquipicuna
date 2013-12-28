##Time Series Analysis of Hummingbird Presences

require(stringr)
##Read in data

#################################
#Species Presence and Time
##################################

#Bring in Transect Data and Camera Data
dat<-read.csv("Thesis/Maquipucuna_SantaLucia/Results/Network/HummingbirdInteractions.csv")

#make a date column
dat$POSIXd<-NULL

dat$DateP<-sapply(dat$Date,function(x){
  if(is.na(x)){
    return(NA)
  }
  if(str_detect(x,"-")){
    return(as.character(strptime(x,"%Y-%m-%d")))
  }
  
  if(str_detect(x,"/")){
    return(as.character(strptime(x,format="%m/%d/%y")))
  }
})

dat$DateP<-as.POSIXlt(dat$DateP)

head(dat)

######################
#Get Elevation GPS
######################


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


#Overall Month_Day and Elevation
ggplot(dat,aes(y=factor(Elevation),x=dat$Time_Stamp,col=Species)) + geom_point(size=3) + scale_x_datetime() + facet_wrap(~Species)
ggsave("Thesis//Maquipucuna_SantaLucia/Results/DateElevation.svg",height=11,width=8,dpi=300)

ggplot(dat,aes(x=dat$Time_Stamp,fill=Species)) + geom_histogram(position="dodge") + facet_wrap(~Elevation)
ggsave("Thesis//Maquipucuna_SantaLucia/Results/TimeofDayElevation.svg",height=11,width=8,dpi=300)


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
