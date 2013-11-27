#Ben Weinstein - Code and Project Design
#Lisa Dittmar reviewed the videos

#Graham Lab, Stony Brook University 10/27/2013

##Competition Feeder Experiments
# A High value resource is placed alongside a low value resource
#Our goal is measure selectivity of each species at each elevation

#Set working directory
droppath<-"C:/Users/Jorge/Dropbox/Thesis//Maquipucuna_SantaLucia/"

##Read in data
dat<-read.csv(paste(droppath,"Data2013/csv/CompetitionFeeders.csv",sep=""))

#How many videos do we have for each elevation and treatment?
vid_totals<-aggregate(dat$Video,list(dat$Elevation,dat$Treatment),function(x) nlevels(droplevels(x)))

#Species richness and identity at each elevation
sp_matrixHL<-(table(dat$Species,dat$Elevation,dat$Treatment) >= 1) * 1

#View species at each elevation and treatment
m.sp_m<-melt(sp_matrixHL)
colnames(m.sp_m)<-c("Species","Elevation","Treatment","Presence")

#turn 0's to NA's just for plotting

m.sp_m[m.sp_m$Presence==0,"Presence"]<-NA

#View as tiles
p<-ggplot(m.sp_m,aes(y=Species,x=factor(Elevation),fill=as.factor(Presence)))+ geom_tile()  + facet_wrap(~Treatment) + theme_bw() + scale_fill_discrete(na.value="white")
p + labs(fill="Present",x="Elevation")
#ggsave()

#richness across feeders
p<-ggplot(m.sp_m,aes(y=Species,x=factor(Elevation),fill=as.factor(Presence)))+ geom_tile() + theme_bw() + scale_fill_discrete(na.value="white")
p + labs(fill="Present",x="Elevation")