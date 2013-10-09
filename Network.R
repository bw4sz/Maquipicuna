###############################################################################
#Hummingbird Flower Interactions at the Santa Lucia and Maquipucuna EcoReserves
###############################################################################
#Ben Weinstein - Stony Brook University - Department of Ecology and Evolution

require(chron)
require(bipartite)
require(ggplot2)
require(ape)
require(reshape)
require(sna)
require(stringr)

#############################
#Set Dropbox Location
#Read in flower videos
home<-"C:/Users/Ben/Dropbox/"
setwd(home)
#############################

#############
#Read in Data
#############

#Read in Flower Dataset
dat<-read.csv("Thesis/Maquipucuna_SantaLucia/Data2013/csv/FlowerVideo.csv")

#Bring in the phylogeny
#Read in phylogeny
tree<-read.nexus("Shared Ben and Catherine/DimDivEntire/Files for Analysis/ColombiaPhylogenyUM.tre")

#Read in names file to replace names in Nexis file
spnames<-read.table(file="Shared Ben and Catherine/DimDivEntire/Files for Analysis/SpNameTree.txt" , sep = "\t", header = TRUE)

#Replace tip.label with Spnames#
tree$tip.label<-as.character(spnames$SpName) 
head(dat)

#For the sake of simplicity, make everything lowercase
.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

#bring in clade data
clades<-read.csv("Shared Ben and Catherine/DimDivEntire/Files for Analysis/CladeList.txt",header=FALSE)[,-1]
colnames(clades)<-c("Clade","Genus","Species","double","English")
clades<-clades[,1:5]

####################################################
#Analysis of Flower Usage for each Hummingbird Species
####################################################

##############
#Data Cleaning 
##############

#Capitalize Flowers
dat$Flower<-factor(sapply(dat$Flower,function(x) {.simpleCap(as.character(x))}))

#Caps Hummingbird
dat$Hummingbird<-factor(sapply(dat$Hummingbird,function(x) {.simpleCap(as.character(x))}))

#Take our any bad data
dat<-droplevels(dat[!dat$Hummingbird %in% c("","NANA"),])

#Overall statistics
#How many flowers Species
paste("Number of Flower Species:",nlevels(dat$Flower))

#How many Birds Species
paste("Number of Hummingbird Species:",nlevels(dat$Hummingbird))

#Create function to compute network parameters
#The general strategy is to write all metrics to file, and develop call statements at the end to retrieve them
NetworkC<-function(datf,naming){
  
  #Set a working directory, create a folder for each run
  setwd(home)
  toset<-paste("Thesis/Maquipucuna_SantaLucia/Results/Network/",naming,sep="")
  dir.create(toset,showWarnings=FALSE)
  setwd(toset)

  
#Interaction of flowers and birds
F_H<-as.data.frame.array(table(datf$Flower,datf$Hummingbird))

#Save Input Matrix
write.csv(F_H,"BirdXFlower.csv")

#View Web
svg(filename="WebPlot.svg",height=7,width=12)
plotweb(F_H)
dev.off()

#Plot matrix of interactions
#There was an error on the svg
jpeg(filename="MatrixPlot.jpeg",height=8,width=8,units="in",res=300)
visweb(F_H)
dev.off()

#Metrics across entire
birds.prop<-data.frame(HummingbirdNetwork=networklevel(F_H,level="higher"))
plants.prop<-data.frame(PlantNetwork=networklevel(F_H,level="lower"))

#Merge networks
NetworkProp<-data.frame(birds.prop,plants.prop)
#Write to file
write.csv(NetworkProp,"NetworkProperties.csv")
  
#Metrics across species, write to file
H.species.prop<-specieslevel(F_H,level="higher")

#Hummingbird Properties
write.csv(H.species.prop,"HummingbirdMetrics.csv")

#Plant Network Metrics  
P.species.prop<-specieslevel(F_H,level="lower")
write.csv(P.species.prop,"PlantMetrics.csv")
  
##################################################
#Specialization for each species
##################################################
  
#For birds
birds.special<-dfun(t(F_H))
birds.spl<-data.frame(lapply(birds.special,data.frame))
colnames(birds.spl)<-names(birds.special)
ggplot(birds.spl[1:22,],aes(x=rownames(birds.spl)[1:22],y=dprime)) + geom_point() + theme_bw() + theme(axis.text.x=element_text(angle=90))
ggsave("Specialization.svg",height=8,width=9)

#############################################
#Resource overlap between Hummingbird species
#############################################

#Collapse Matrix into Hummingbird by Hummingbird Matrix
#Hummingbird
H_H<-as.one.mode(F_H,project="higher")
diag(H_H)<-NA
H_H[upper.tri(H_H)]<-NA
m.HH<-melt(H_H)
  
#Plot Resource overlap between hummingbird Species
ggplot(m.HH,aes(X1,X2,fill=value)) + geom_tile() + scale_fill_continuous(low="blue",high="red",na.value="white") + theme(axis.text.x = element_text(angle = 90, hjust = 1),panel.background=element_rect(color="white"))
ggsave("ResourceOverlap.svg",height=8,width=11)

#Relatedness and flower overlap, very rudimentary test so far
ctrx<-cophenetic(tree)
         
    ER<-function(x){
    y<-m.HH[x,]
    if(sum(clades$English %in% y[[1]])==0) {return("NA")}
    if(sum(clades$English %in% y[[2]])==0) {return("NA")}
    sp1<-gsub(" ","_",clades[clades$English %in% y[[1]],"double"])
    sp2<-gsub(" ","_",clades[clades$English %in% y[[2]],"double"])
    
      return(tryCatch(ctrx[sp1,sp2],error=function(e) print("NA")))
    }

#get cophenetic distance between species
m.HH$Relatedness<-sapply(1:nrow(m.HH),ER)

#Relatedness and plant overlap
ggplot(m.HH[m.HH$value>0,],aes(y=value,x=as.numeric(Relatedness),)) + geom_point() + geom_smooth(method="lm") + theme_bw() + ylab("Resource Overlap") + xlab("Relatedness")
ggsave("Relatedness_Overlap.svg",height=8,width=11)

#Plants 
P_P<-as.one.mode(F_H,project="lower")
diag(P_P)<-NA
P_P[upper.tri(P_P)]<-NA
m.PP<-melt(P_P)

#
ggplot(m.PP,aes(X1,X2,fill=value)) + geom_tile() + scale_fill_continuous(low="blue",high="red",na.value="white") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("PollinatorOverlap.svg",height=8,width=11)
  
#In the future this is where you consider relatedness among species among plants
#Plot 3d visualization of the hummingbird network
svg("Hummingbird3d.jpeg")
gplot(H_H)
dev.off()
}

############################################
#Run Network Function for the entire dataset
NetworkC(dat,"Total")
############################################

####################################################
#Temporal Change in Network Structure
####################################################

#Split input data into desired paths
dat$Month<-months(chron(as.character(dat$Date)))
dat.split<-split(dat,dat$Month,drop=TRUE)

#Compute metrics for each month
for (x in 1:length(dat.split)){
  NetworkC(dat.split[[x]],names(dat.split)[[x]])
}

#Retrieve Classes, name them, melt 

#Start with networkwide properties
netPath<-paste(home,"Thesis/Maquipucuna_SantaLucia/Results/Network/",sep="")

#Get the desired files from paths
fil.list<-list.files(netPath,pattern="NetworkProperties.csv",recursive=TRUE,full.names=TRUE)
#I'm terrible at regex, the strsplit statement, may need to change depending on your path


fil<-list()
#Read and name each file
for (x in 1:length(fil.list)){
  fil[[x]]<-read.csv(fil.list[[x]])
  names(fil)[x]<-strsplit(fil.list[[x]],"/")[[1]][10]
}

#melt the outputs to a single dataframe
m.Prop<-melt(fil)
colnames(m.Prop)<-c("Metric","Level","value","Time")

#Correct the naming of levels
levels(m.Prop$Level)<-c("Hummingbirds","Plants")

#If you want to remove overall metrics
month.Prop<-m.Prop[!m.Prop$Time=="Total",]

#For each metric plot them with time
dir.create(paste(netPath,"TimeFigures",sep=""))
setwd(paste(netPath,"TimeFigures",sep=""))

#Quick and dirty look at all metrics
p<-ggplot(na.omit(month.Prop),aes(x=Time,y=value,col=Level)) + geom_point() + geom_line(linetype="dashed",aes(group=Level)) + facet_wrap(~Metric,scales="free_y")
p + theme_bw()
ggsave("MetricsFacet.svg",height=8,width=11)

dir.create("Metric_TimePlots")
setwd("Metric_TimePlots")

#Individual plots
for(x in levels(month.Prop$Metric)) {
  print(x)
  toplot<-na.omit(month.Prop[month.Prop$Metric %in% x,])
  
  #If there are no records, go to next metric
  if(nrow(toplot)==0) next
  
  #Plot and Save
  p<-ggplot(toplot,aes(x=Time,y=value,col=Level)) + geom_point() + geom_line(linetype="dashed",aes(group=Level))
  p + theme_bw() + ylab(x)
  ggsave(paste(x,".svg",sep=""),height=8,width=11)
}

#Compute Metrics for each Humminbird species

#Get the desired files from paths
fil.list<-list.files(netPath,pattern="HummingbirdMetrics.csv",recursive=TRUE,full.names=TRUE)

fil<-list()
#Read and name each file
for (x in 1:length(fil.list)){
  fil[[x]]<-read.csv(fil.list[[x]])
  names(fil)[x]<-strsplit(fil.list[[x]],"/")[[1]][10]
}

Hum.Time<-melt(fil)
colnames(Hum.Time)<-c("Species","Metric","value","Time")

#Quick and dirty look across species 
ggplot(Hum.Time,aes(Time,value)) + facet_grid(Species~Metric,scales="free") + geom_line(linetype="dashed",aes(group=Species)) + geom_point() + theme_bw()
ggsave(paste(netPath,"TimeFigures/HumSpecies_Time.svg",sep=""),height=8,width=11)

#Clearly in the future, need to zoom in on the metrics we actually care about.

#Plot for each species, or for each metric?


setwd(home)


save.image("Thesis/Maquipucuna_SantaLucia/Results/Network/NetworkData.Rdata")