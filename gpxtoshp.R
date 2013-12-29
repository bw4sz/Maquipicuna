#Load GPS
#Read and convert gpx points to a single dataframe and save it as a shapefile
require(plotKML)
require(reshape2)
require(maptools)

#Set working directory
droppath<-"C:/Users/Ben/Dropbox/"
setwd(droppath)

###############################################
#Read in GPS Data from Summer 2013 Field Season
###############################################

f<-list.files("Thesis\\Maquipucuna_SantaLucia\\Data2013\\GPS",full.names=TRUE,pattern=".gpx",recursive=TRUE)

gpx<-list()
for (x in 1:length(f)){
  print(x)
  try(
  gpx[[x]]<-readGPX(f[x],waypoints=TRUE)$waypoints)
}

#################################################
#Read in GPS Data from Holger Field Season
#################################################

##Repeat for first gps
g<-list.files("Holger\\Transect_Protocol_Holger\\WayPoints",full.names=TRUE,pattern=".gpx",recursive=TRUE)

#loop through input files and find the errors. 
gpx2<-list()
for (x in 1:length(g)){
  print(x)
  try(
    gpx2[[x]]<-readGPX(g[x],waypoints=TRUE)$waypoints)
}

#Need to remind nelly to upload her gps. 

#Bind into one dataframe
gpx.dat<-rbind.fill(rbind.fill(gpx[sapply(gpx,class)=="data.frame"]),rbind.fill(gpx2[sapply(gpx2,class)=="data.frame"]))

#create  spatial object
dat.sp<-SpatialPointsDataFrame(coords=cbind(gpx.dat$lon,gpx.dat$lat),gpx.dat)

#create shapefile
writePointsShape(dat.sp,"Thesis\\Maquipucuna_SantaLucia\\Data2013\\Shapefiles\\GPSshape.shp")
