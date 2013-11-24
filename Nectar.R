#Nectar Script


#Bring in nectar data
Nectar <- read.csv("Thesis/Maquipucuna_SantaLucia/Data2013/csv/Nectar.csv")

#Fix colnames that are ugly
colnames(Nectar)[c(8,10,11,12,13)]<-c("Height","TubeLength","Brix","EffectiveCorolla","TotalCorolla")

#Data Cleaning
#All family and Genus should be capitalized
Nectar$Family<-as.factor(sapply(Nectar$Family,function(x) .simpleCap(as.character(x))))
Nectar$Genus<-as.factor(sapply(Nectar$Genus,function(x) .simpleCap(as.character(x))))

#species should be lowercase
Nectar$Species<-as.factor(sapply(Nectar$Species,function(x) tolower(as.character(x))))

#Create full name column
Nectar$Full<-paste(Nectar$Family,Nectar$Genus,Nectar$Species)

#Some basic visualizations to check data clarity
#number of records per species
m.Nectar<-melt(table(Nectar[!is.na(Nectar$Brix),]$Full))

#No records should be 0? turn these to NA
Nectar[Nectar$Brix==0 & is.finite(Nectar$Brix),]

#Is the tube given in diameter?
#tube column needs to have correct math.

as.numeric(Nectar$Tube.Type)/2 * 2*pi * Nectar$TubeLength

ggplot(m.Nectar,aes(x=Var.1,value)) + geom_bar() + coord_flip() + geom_text(aes(label=value),col="red",hjust=1) + theme_bw()

p<-ggplot(Nectar[!is.na(Nectar$Brix),],aes(x=Species,y=Brix)) + geom_point() + facet_wrap(~Family,scales="free_x")
p+ theme_bw() +theme(axis.text.x = element_text(angle = 90,size=10))

p<-ggplot(Nectar[!is.na(Nectar$TotalCorolla),],aes(x=Species,y=TotalCorolla)) + geom_point() + facet_wrap(~Family,scales="free_x")
p+ theme_bw() +theme(axis.text.x = element_text(angle = 90,size=10)) + geom_point()

ggplot(Nectar,aes(x=TotalCorolla,y=Brix)) + geom_point(aes(color=Family)) + stat_smooth(method="lm") + xlim(0,60)

ggplot(Nectar,aes(x=EffectiveCorolla,y=Brix)) + geom_point(aes(color=Family)) + stat_smooth(method="lm") + geom_text(aes(label=Family))

ggplot(Nectar,aes(x=Corolla.Width,y=Brix)) + geom_point(aes(color=Family)) + stat_smooth(method="lm") + xlim(0,14)

#combine the elevation flower transects with Nectar data 

#First take the average nectar reading by species
Nectar.mean<-aggregate(Nectar,list(Nectar$Full),mean,na.rm=TRUE)[,c("Group.1","EffectiveCorolla","TotalCorolla","Corolla.Width","Brix")]
fl.nectar<-merge(fl.elev,Nectar.mean,by.x="Full",by.y="Group.1")

#how many transect rows didn't match nectar rows
missing<-fl.elev[!fl.elev$Full %in% Nectar.mean$Group.1,]
dim(missing)
table(missing$Full)

#okay, before we go crazy with nectar,lets look at corolla length with elevation
ggplot(fl.nectar,aes(x=ele,y=TotalCorolla,col=Family)) + geom_point(position="jitter") + stat_smooth(aes(group=1),method='lm') 

#Nectar concentration and elevation, there are some odd records that need to be fixed
ggplot(fl.nectar,aes(x=ele,y=Brix,col=Family)) + geom_point() + stat_smooth(aes(group=1),method='lm') + ylim(5,30)

#create volume calculations

#Flower bloomign through time
head(fl.nectar)
