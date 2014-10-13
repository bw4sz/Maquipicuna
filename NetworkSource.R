#Network Source Functions
#Create function to compute network parameters
#The general strategy is to write all metrics to file, and develop call statements at the end to retrieve them
NetworkC<-function(datf,naming){
  print(naming)
  #Set a working directory, create a folder for each run
  setwd(droppath)
  toset<-paste(paste(droppath,"Thesis/Maquipucuna_SantaLucia/Results/Network/",sep=""),naming,sep="")
  
  dir.create(toset,recursive=TRUE)
  setwd(toset)
  
  #Drop any unused factors?
  datf<-droplevels(datf)
  
  #Drop any observations without plants
  datf<-droplevels(datf[!datf$Iplant_Double %in% "",])
  
  #Interaction of flowers and birds
  F_H<-as.data.frame.array(table(datf$Iplant_Double,datf$Hummingbird))
  
  #Save Input Matrix
  write.csv(F_H,"BirdXFlower.csv")
  
  #View Web
  svg(filename="WebPlot.svg",height=7,width=15)
  plot(F_H)
  dev.off()
  
  #create a order for hummingbirds
  toOrd<-merge(clades,data.frame(English=colnames(F_H)),sort=FALSE,all.y=TRUE)$English
  
  #create a order for plants
  Hlab<-names(which(!apply(F_H,2,sum)==0))
  
  toOrd<-as.character(merge(clades,data.frame(English=Hlab),sort=FALSE,all.y=TRUE)$English)
  
  Plab<-names(which(!apply(F_H,1,sum)==0))
  
  sequ<-list(seq.high=toOrd,seq.low=Plab)
  
  svg(filename="WebPlotOrder.svg",height=7,width=15)
  plot(F_H,sequence=sequ)
  dev.off()
  
  #Interaction matrix
  
  orderflowers<-names(sort(apply(F_H,1,sum),decreasing=FALSE))
  
  orderbirds<-names(sort(apply(F_H,2,sum),decreasing=TRUE))
  
  a<-melt(as.matrix(F_H))
  
  colnames(a)<-c("Flowers","Birds","value")
  
  a$Flowers<-factor(a$Flowers,levels=orderflowers)
  a$Birds<-factor(a$Birds,levels=orderbirds)
  
  ggplot(a[a$value>0,],aes(x=Birds,y=Flowers,fill=value)) + geom_tile()+ theme_bw() + scale_fill_continuous(low="blue",high="red") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(fill="# of Visits")
  ggsave("MatrixPlot.jpeg",dpi=300,height=8,width=8)
  ggsave("MatrixPlot.eps",dpi=300,height=8,width=8)
  
  #Metrics across entire
  tryCatch(birds.prop<-data.frame(HummingbirdNetwork=networklevel(F_H,level="higher")),error=function(e)
    print(e))
  
  #Add in a flag if the network is just too small. 
  if(!exists("birds.prop")){
    return("Not Enough Hummingbird Species")
  }
  
  #Network prop for plants
  #Metrics across entire
  tryCatch(plants.prop<-data.frame(HummingbirdNetwork=networklevel(F_H,level="lower")),error=function(e)
    print(e))
  
  #Add in a flag if the network is just too small. 
  if(!exists("plants.prop")){
    return("Not Enough Plant Species")
  }
  
  #Merge networks
  NetworkProp<-data.frame(birds.prop,plants.prop)
  
  #Write to file
  write.csv(NetworkProp,"NetworkProperties.csv")
  
  #Metrics across species, write to file
  tryCatch(H.species.prop<-specieslevel(F_H,level="higher"),error = function(e) {
    print(paste("Not enough Species:",e))})
  
  if(!exists("H.species.prop")){
    return("Not Enough Hummingbird Species")
  }
  
  #Hummingbird Properties
  
  write.csv(H.species.prop,"HummingbirdMetrics.csv")
  
  
  #Plant Network Metrics  
  #Metrics across species, write to file
  tryCatch(P.species.prop<-specieslevel(F_H,level="lower"),error = function(e) {
    print(paste("Not enough plant Species:",e))})
  
  if(!exists("P.species.prop")){
    return("Not Enough Plant Species")
  }
  
  write.csv(P.species.prop,"PlantMetrics.csv")
  
  ##################################################
  #Specialization for each species
  ##################################################
  
  birds.special<-dfun(t(F_H))
  birds.spl<-data.frame(lapply(birds.special,data.frame))
  colnames(birds.spl)<-names(birds.special)
  birds.spl$Species<-rownames(birds.spl)
  
  #size by sample size?
  
  ggplot(birds.spl,aes(x=Species,y=dprime)) + geom_point() + theme_bw() + theme(axis.text.x=element_text(angle=90))
  ggsave("Specialization.svg",height=8,width=9)
  
  #############################################
  #Resource overlap between Hummingbird species
  #############################################
  
  #Collapse Matrix into Hummingbird by Hummingbird Matrix
  #Hummingbird
  H_H<-as.one.mode(F_H,project="higher")
  
  #Bird Bray Distance
  m.HH<-as.matrix(vegdist(t(F_H),"bray"))
  diag(m.HH)<-NA
  m.HH<-melt(as.matrix(m.HH))
  #Plot Resource overlap between hummingbird Species
  ggplot(m.HH,aes(X1,X2,fill=value)) + geom_tile() + scale_fill_continuous(low="blue",high="red",na.value="white") + theme(axis.text.x = element_text(angle = 90, hjust = 1),panel.background=element_rect(color="white"))
  ggsave("ResourceOverlap.svg",height=8,width=11)
  
  #Relatedness and flower overlap, very rudimentary test so far
  ctrx<-cophenetic(tree)
  
  ER<-function(x){
    y<-m.HH[x,]
    if(sum(clades$English %in% y[[1]])==0) {return(NA)}
    if(sum(clades$English %in% y[[2]])==0) {return(NA)}
    sp1<-gsub(" ","_",clades[clades$English %in% y[[1]],"double"])
    sp2<-gsub(" ","_",clades[clades$English %in% y[[2]],"double"])
    
    return(
      tryCatch(ctrx[sp1,sp2],error=function(e) {
        #print(c(sp1,sp2))
        return(NA)}
      )
    )
  }
  
  #get cophenetic distance between species, might need to the new phylogeny, which species don't match?
  m.HH$Relatedness<-sapply(1:nrow(m.HH),ER)
  colnames(m.HH)[1:2]<-c("To","From")
  hist(m.HH$value)
  #Phylogenetic Relatedness and plant overlap
  p<-ggplot(m.HH[,],aes(y=value,x=as.numeric(Relatedness),)) + geom_jitter() + geom_smooth() + theme_bw() + ylab("Resource Overlap") + xlab("Relatedness") 
  p+ geom_text(aes(label=paste(To,From)),size=2,vjust=1)
  ggsave("Relatedness_Overlap.svg",height=8,width=11)
  
  ##########################################
  #Phylogenetic niche breadth of pollinators
  ##########################################
  #replace the colnames with english names?
  scinames<-sapply(colnames(F_H),function(x) gsub(" ","_",clades[clades$English %in% x,"double"]))
  colnames(F_H)<-scinames
  colnames(F_H)[!colnames(F_H) %in% colnames(ctrx)]<-"Phaethornis_longuemareus"
  
  phyloDiversity<-mpd(samp=F_H,dis=ctrx,abundance.weighted=TRUE)
  phyloDiversity<-data.frame(Plants=rownames(F_H),Hummingbirds=phyloDiversity)
  
  write.csv(phyloDiversity,"PollinatorPhyloDiversity.csv")
  
  ggplot(phyloDiversity,aes(y=Hummingbirds,x=Plants)) + geom_bar(stat="identity") + theme(axis.text.x=element_text(angle = -90, hjust = 0)) + ylab("Pollinator Phylodiversity")
  ggsave("PollinatorPhyloDiversity.svg",dpi=300)
  
  ##############Trait Relatedness####################
  
  #get cophenetic distance between species
  m.HH$RelatednessT<-NA
  
  #can't figure out why this throwing a weird flag
  for (x in 1:nrow(m.HH)){
    y<-m.HH[x,]
    if(sum(clades$English %in% y[[1]])==0) {next}
    if(sum(clades$English %in% y[[2]])==0) {next}
    sp1<-gsub(" ",".",clades[clades$English %in% y[[1]],"double"])
    sp2<-gsub(" ",".",clades[clades$English %in% y[[2]],"double"])
    if(is.null(sp.dist[sp1,sp2])){next}
    m.HH[x,"RelatednessT"]<-as.numeric(sp.dist[sp1,sp2])
  }
  
  #Trait Relatedness and plant overlap
  ggplot(m.HH[,],aes(y=value,x=RelatednessT)) + geom_point() + geom_smooth() + theme_bw() + ylab("Resource Overlap") + xlab("Relatedness") #+ geom_text(aes(label=paste(To,From)),size=3)
  ggsave("TraitRelatedness_Overlap.svg",height=8,width=11)
  
  #Plants 
  P_P<-as.one.mode(F_H,project="lower")
  diag(P_P)<-NA
  P_P[upper.tri(P_P)]<-NA
  m.PP<-melt(P_P)
  
  write.csv(m.PP,"Hummingbird_Resource_Overlap.csv")
  #
  ggplot(m.PP,aes(X1,X2,fill=value)) + geom_tile() + scale_fill_continuous(low="blue",high="red",na.value="white") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggsave("PollinatorOverlap.svg",height=8,width=11)
  
  #In the future this is where you consider relatedness among species among plants
  #Plot 3d visualization of the hummingbird network
  svg("Hummingbird3d.jpeg")
  gplot(H_H)
  dev.off()
  
  #Plot matrix of interactions
  #There was an error on the svg
  jpeg(filename="MatrixPlot.jpeg",height=8,width=8,units="in",res=300)
  visweb(F_H)
  dev.off()
  
  
  
  ########################################
  #Phylogenetic niche breadth of plants
  ########################################
  
  F_H[rownames(F_H) %in% rownames(pco),]
  #####################################################3
  #Phylogenetic Diversity of Each network compartment
  ######################################################3
  #get the phylogenetic diversity of pollinators for each plant.
  
  setwd(droppath)
}
print("Function Defined")

#For the sake of simplicity, make everything lowercase
.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

