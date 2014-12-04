#Goal to create randomized networks for each month and redo the entire analysis
require(picante)
require(igraph)
require(bipartite)
require(ggplot2)
require(reshape2)
require(plyr)
require(chron)

#Read in interaction data
#setwd to dropbox
droppath<-"C:/Users/Ben/Dropbox/"
setwd(droppath)
#Set github path
gitpath<-"C:/Users/Ben/Documents/Maquipicuna/"

int<-read.csv("Thesis/Maquipucuna_SantaLucia/Results/Network/HummingbirdInteractions.csv",row.names=1)
int$Year<-years(int$DateP)
int$Year[int$Year %in% 2012]<-2013

int[int$Year %in% 2013 & int$Month %in% 1,"Year"]<-2014

#break up network by month
int.M<-split(int,list(int$Month,int$Year),drop=TRUE)

#design null function
nullC<-function(x){
  x$Iplant<-droplevels(x$Iplant)
  month.inter<-table(x$Hummingbird,x$Iplant_Double)
true_stat<-networklevel(month.inter,c("H2","connectance","cluster coefficient","nestedness"))[1:4]

true_stat<-melt(true_stat)

true_stat$variable<-rownames(true_stat)

# Create 199 random matrixes
r.int<-permatfull(month.inter,times=199)

#compute connectance and clustering on each

#Create a function that computes network metrics
null_total<-as.data.frame(t(sapply(r.int$perm,function(x){
#Compute netowkr statistics
networklevel(x,c("connectance","cluster coefficient","nestedness","H2"))[1:4]
})))

#get the upper and lower 2.5th quantiles

nullq<-melt(apply(null_total,2,quantile,c(.025,.975)))

#add month
nullq$Month<-unique(x$Month)
#add Year
nullq$Year<-unique(x$Year)

#cast into desired format
castq<-dcast(nullq,Year+Month+X2~X1)

#append true information
out<-merge(castq,true_stat,by.x="X2",by.y="variable")
colnames(out)[colnames(out) %in% "value"]<-"True"

return(out)}


#apply function to each month

dat<-rbind.fill(lapply(int.M,nullC))

#name the column of the percentages
colnames(dat)[4]<-"Lower"
colnames(dat)[5]<-"Upper"


#format date
dat$Date<-as.Date(paste(1,dat$Month,dat$Year,sep="/"),format="%d/%m/%Y")
write.csv(dat,"Thesis/Maquipucuna_SantaLucia/Results/NullNetwork.csv")

#plot distribution of  values
p<-ggplot(dat,aes(x=Date,fill=X2)) + geom_line(aes(y=True)) 
p<-p + geom_ribbon(aes(ymin=Lower,ymax=Upper),alpha=.4)+ facet_wrap(~X2,nrow=2,scales="free") + theme_bw()  + labs(col="Metric of Specialization") 
plot(p+scale_fill_discrete(guide="none") + scale_x_date(breaks="3 months",label=date_format("%m/%y")))

ggsave("Thesis/Maquipucuna_SantaLucia/Results/NullNetwork.jpeg",dpi=300,height=8,width=8)
