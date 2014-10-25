#Goal to create randomized networks for each month and redo the entire analysis
require(picante)
require(igraph)
require(bipartite)
require(ggplot2)
require(reshape2)
require(plyr)

#Read in interaction data
#setwd to dropbox
droppath<-"C:/Users/Ben/Dropbox/"
setwd(droppath)
#Set github path
gitpath<-"C:/Users/Ben/Documents/Maquipicuna/"

int<-read.csv("Thesis/Maquipucuna_SantaLucia/Results/Network/HummingbirdInteractions.csv",row.names=1)

#break up network by month
int.M<-split(int,int$Month,drop=TRUE)

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

#cast into desired format
castq<-dcast(nullq,Month+X2~X1)

#append true information
out<-merge(castq,true_stat,by.x="X2",by.y="variable")
colnames(out)[colnames(out) %in% "value"]<-"True"

return(out)}

#test function
nullC(x=int.M[[1]])


#apply function to each month

dat<-rbind.fill(lapply(int.M,nullC))

#name the column of the percentages
colnames(dat)[3]<-"Lower"
colnames(dat)[4]<-"Upper"

write.csv(dat,"Thesis/Maquipucuna_SantaLucia/Results/NullNetwork.csv")

#plot distribution of  values
p<-ggplot(dat,aes(x=Month,fill=X2)) + geom_line(aes(y=True)) 
p + geom_ribbon(aes(ymin=Lower,ymax=Upper),alpha=.4)+ facet_wrap(~X2,nrow=2,scales="free") + theme_bw() + scale_x_continuous(breaks=1:12) + labs(col="Metric of Specialization")

ggsave("Thesis/Maquipucuna_SantaLucia/Results/NullNetwork.jpeg",dpi=300,height=8,width=8)
