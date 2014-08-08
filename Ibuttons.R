##Ibutton data

require(stringr)
require(reshape2)
require(ggplot2)
require(chron)
require(sp)
require(scales)
require(gstat)

setwd("C:/Documents and Settings/Administrator/My Documents/Dropbox")

#List ibutton files
fil<-list.files("C:/Documents and Settings/Administrator/My Documents/Ibuttons",full.names=TRUE,recursive=TRUE)

tdat<-lapply(fil,function(g){
  
  #Read in raw file lines
  x<-readLines(g)
  head(x)
  
  #Where is teh date time header
  line<-which(str_detect(x,"Date/Time"))
  
  dat<-x[-c(1:line-1)]
  
  #read in a csv format
  dat<-read.csv(text=dat)
  
})

#Get filename for elevation and replicate

names(tdat)<-sapply(fil,function(x){
  regex1<-str_extract(x,"Ibuttons/\\w+/\\w+")
  
  regex2<-str_split(regex1,"/")[[1]][3]
  
})

lapply(tdat,head)

for ( x in 1:length(tdat)){
  colnames(tdat[[x]])[3]<-"Temp"
}



##remove first couple rows
tdat<-lapply(tdat,function(x){
  x[-c(1:48),]
})


tdata<-melt(tdat)

tdata<-tdata[,!colnames(tdata) %in% "variable"]

colnames(tdata)[3]<-"Temp"

tdata$elevation<-as.numeric(substr(tdata$L1,1,4))

tdata$replicate<-substr(tdata$L1,5,5)


########There is a bizairre ramification of the timestamp, that changes the afternoon of new years eve to the next year, change manually?
tdata[tdata$Month == "December" & tdata$Year==2014,]

tdata[tdata$Month == "December" & tdata$Year==2014,"Year"]<-factor("2013")

##########Create Time Stamp#################
tdata$TimeStamp<-as.POSIXct(tdata$Date.Time,format="%m/%d/%y %I:%M:%S %p")

##Extract information from date
tdata$Month<-months(tdata$TimeStamp)

tdata$Month<-factor(tdata$Month,levels=month.name)

tdata$Year<-years(tdata$TimeStamp)

tdata$Hour<-as.numeric(strftime(tdata$TimeStamp,format="%H"))

tdata$Day<-days(tdata$TimeStamp)

############Formatting Complete#############

###Write data to file
write.csv(tdata,"Thesis//Maquipucuna_SantaLucia/Results/Ibuttons/TempData.csv")

#first plot
#ggplot(tdata,aes(factor(elevation),Temp,col=replicate)) + geom_boxplot() + labs(x="Elevation")

####Temperature per month and elevation

ggplot(tdata,aes(x=TimeStamp,y=Temp,col=factor(elevation))) + stat_smooth() + labs(x="Month",y="Temp (C)",col="Elev.") + theme_bw() + scale_x_datetime(breaks=date_breaks("1 month"),labels = date_format("%b,%y"))
ggsave("Thesis//Maquipucuna_SantaLucia/Results/Ibuttons/Temperature_Month.jpeg",unit="in",height=8,width=8,dpi=300)
ggsave("Thesis//Maquipucuna_SantaLucia/Results/Ibuttons/Temperature_Month.pdf",height=8,width=10,dpi=300)

###Temperature per day and elevation
ggplot(tdata,aes(x=Hour,y=Temp,col=factor(elevation))) + stat_smooth() + labs(x="Hour",y="Temp (C)",col="Elev.") + theme_bw() + facet_wrap(~Month)
ggsave("Thesis//Maquipucuna_SantaLucia/Results/Ibuttons/Temperature_Day.pdf",height=8,width=10,dpi=300)

###Sets of stats to do,

#It appears that 4 pm , 16:00 is the warmest part of the day, create average temp/month at 4pm

#It appears at 10 am is the coldest part of the day, average temp at 10am/month

#Within day, max - min temperature

#Within month variance

##########Stat table

Month.var<-aggregate(tdata$Temp,list(tdata$elevation,tdata$Month,tdata$Year),var)
colnames(Month.var)<-c("Elevation","Month","Year","Var")

ggplot(Month.var,aes(col=factor(Elevation),Var,x=Month)) + geom_line(aes(group=Elevation)) + theme_bw()

####################### Daily range

range_day<-aggregate(tdata$Temp,list(tdata$elevation,tdata$Day,tdata$Month,tdata$Year),function(x){
  max(x)-min(x)
})

colnames(range_day)<-c("Elevation","Day","Month","Year","Range")

ggplot(range_day,aes(x=Month,y=Range,col=factor(Elevation))) + stat_smooth(aes(group=Elevation)) + theme_bw() + labs(col="Elevation")
ggsave("Thesis//Maquipucuna_SantaLucia/Results/Ibuttons/Temperature_Range.jpeg",unit="in",height=10,width=6,dpi=300)
ggsave("Thesis//Maquipucuna_SantaLucia/Results/Ibuttons/Temperature_Range.pdf",height=8,width=10,dpi=300)

#################Create a dataframe for montly bioclim variables.

#Mean diurnal temperature
daytemp<-tdata[tdata$Hour %in% 6:18,]

bio1_a<-aggregate(daytemp$Temp,list(daytemp$elevation,daytemp$Day,daytemp$Month,daytemp$Year),mean,na.rm=TRUE)
bio1<-aggregate(bio1_a$x,list(bio1_a$Group.1,bio1_a$Group.3,bio1_a$Group.4),mean)

colnames(bio1)<-c("elevation","Month","Year","Temp")



range_day<-aggregate(tdata$Temp,list(tdata$elevation,tdata$Day,tdata$Month,tdata$Year),function(x){
  max(x)-min(x)
})

colnames(range_day)<-c("Elevation","Day","Month","Year","Range")

##Daytime temp versus nighttime
tdata$Day_Night<-cut(tdata$Hour,c(0,6,19,25),include.lowest=TRUE,c("Nig","Day","Night"))

#fix the night labels
tdata$Day_Night[tdata$Day_Night %in% "Nig"]<-as.factor("Night")


ggplot(tdata,aes(x=TimeStamp,y=Temp,col=factor(elevation))) + stat_smooth() + labs(x="Month",y="Temp (C)",col="Elev.") + theme_bw() + scale_x_datetime(breaks=date_breaks("1 month"),labels = date_format("%b,%y")) + facet_wrap(~Day_Night)
ggsave("Thesis//Maquipucuna_SantaLucia/Results/Ibuttons/Day_Night.jpeg",unit="in",height=9,width=12,dpi=300)



getTemp<-function(Elev,D,H){
  
  
  #format input date and time
  dm<-months(chron(D),abbreviate=FALSE)
  dd<-days(chron(D))
  dy<-years(chron(D))
  
  #Get the closest data points. 
  timeH<-chron(times=as.character(H))
  time_bracket<-c(hours(timeH),hours(timeH)+1)
  elev_bracket<-c(floor(Elev/100)*100,floor(Elev/100)*100 + 100)
  
  #subset the dataset
  ddat<-tdata[tdata$Month %in% dm & tdata$Year %in% dy & tdata$Day %in% dd & tdata$Hour %in% time_bracket & tdata$elevation %in% elev_bracket,]
  
  #take mean of replicate
  meanT<-aggregate(ddat$Temp,list(ddat$elevation,ddat$Hour),mean,na.rm=TRUE)
  colnames(meanT)<-c("Elev","H","Temp")
  
  #Get the time weight and mean temp
  tweights=c(minutes(timeH)/60,1-minutes(timeH)/60)
  meanH<-aggregate(meanT$Temp,list(meanT$H),mean)
  temp_mean<-weighted.mean(meanH$x,tweights)
  
  
  #get the elev weight and temp
  as.numeric(substring(Elev,3))/100
  eweight=c(as.numeric(substring(Elev,3))/100,1-as.numeric(substring(Elev,3))/100)
  meanE<-aggregate(meanT$Temp,list(meanT$Elev),mean)
  Elevmean<-weighted.mean(meanE$x,eweight)
  
  #take mean of both, this is the current temp estimate
  interpolated_temp<-mean(c(Elevmean,temp_mean))
  return(interpolated_temp)
}

#test one
getTemp(Elev=1410,D="3/2/2014",H="8:15:00")

#test series
plot(sapply(seq(1400,1800,10),function(x){
  getTemp(Elev=x,D="3/2/2014",H="10:30:00")
}))
