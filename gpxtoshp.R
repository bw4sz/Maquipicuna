#Read and convert gpx points to a single dataframe and save it as a shapefile
require(plotKML)
require(reshape)
f<-list.files("F:\\KarenGPS\\8_14_2013_Anushas",full.names=TRUE)

#loop through input files and find the errors. 
gpx<-list()
for (x in 1:length(f)){
  print(x)
  try(
  gpx[[x]]<-readGPX(f[x],waypoints=TRUE)$waypoints)
}

##Repeat for first gps
f<-list.files("F:\\KarenGPS\\KarenFirstgps/",full.names=TRUE)

#loop through input files and find the errors. 
gpx2<-list()
for (x in 1:length(f)){
  print(x)
  try(
    gpx2[[x]]<-readGPX(f[x],waypoints=TRUE)$waypoints)
}
#Bind together the days taht contain data
gpx.dat<-rbind(rbind.fill(gpx[sapply(gpx,class)=="data.frame"]),rbind.fill(gpx2[sapply(gpx2,class)=="data.frame"]))

#create  spatial object
dat.sp<-SpatialPointsDataFrame(coords=cbind(gpx.dat$lon,gpx.dat$lat),gpx.dat)

#create shapefile
writePointsShape(dat.sp,"C:\\Users\\Jorge\\Dropbox\\Thesis\\Maquipucuna_SantaLucia\\Data2013\\Shapefiles\\AnushaGPS.shp")
