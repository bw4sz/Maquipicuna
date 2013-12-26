#Network Source Functions
#Create function to compute network parameters
#The general strategy is to write all metrics to file, and develop call statements at the end to retrieve them
NetworkC<-function(datf,naming){
  
  #Set a working directory, create a folder for each run
  setwd(droppath)
  toset<-paste("Thesis/Maquipucuna_SantaLucia/Results/Network/",naming,sep="")
  dir.create(toset,showWarnings=FALSE)
  setwd(toset)
  
  #Interaction of flowers and birds
  F_H<-as.data.frame.array(table(datf$Iplant_Double,datf$Hummingbird))
  
  #Save Input Matrix
  write.csv(F_H,"BirdXFlower.csv")
  
  #View Web
  svg(filename="WebPlot.svg",height=7,width=12)
  plotweb(F_H)
  dev.off()
  
 
  jpeg(filename="MatrixPlotCompartments.jpeg",height=8,width=8,units="in",res=300)
  visweb(F_H,"compartment")
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
   
  birds.special<-dfun(t(F_H))
  birds.spl<-data.frame(lapply(birds.special,data.frame))
  colnames(birds.spl)<-names(birds.special)
  birds.spl$Species<-rownames(birds.spl)
  
  ggplot(birds.spl,aes(x=Species,y=dprime)) + geom_point() + theme_bw() + theme(axis.text.x=element_text(angle=90))
  ggsave("Specialization.svg",height=8,width=9)
  
  #############################################
  #Resource overlap between Hummingbird species
  #############################################
  
  #Boris suggest that we consider a distance based framework for the Y axis (consider Niche overlap from Schoener?)
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
    
    return(
      tryCatch(ctrx[sp1,sp2],error=function(e) {
        print(c(sp1,sp2))
        return("NA")}
      )
    )
  }
  
  #get cophenetic distance between species, might need to the new phylogeny, which species don't match?
  m.HH$Relatedness<-sapply(1:nrow(m.HH),ER)
  colnames(m.HH)[1:2]<-c("To","From")
  
  #Relatedness and plant overlap
  ggplot(m.HH[m.HH$value>1,],aes(y=value,x=as.numeric(Relatedness),)) + geom_point() + geom_smooth(method="lm") + theme_bw() + ylab("Resource Overlap") + xlab("Relatedness") + geom_text(aes(label=paste(To,From)),size=3)
  ggsave("Relatedness_Overlap.svg",height=8,width=11)
  
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
  
}
print("Function Defined")

#For the sake of simplicity, make everything lowercase
.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
