###Dissimilairity in Ecological Networks###########

require(ggplot2)
droppath<-"C:/Users/Ben/Dropbox/"
netPath<-paste(droppath,"Thesis/Maquipucuna_SantaLucia/Results/Network/",sep="")

#Following TIm Poiset
#library(devtools); install_github("tpoisot/betalink")

require(betalink)
require(stringr)

#read in networks from file.
fil.list<-list.files(paste(netPath,"Temporal",sep=""),pattern="BirdXFlower",recursive=TRUE,full.names=TRUE)

networks<-lapply(fil.list,read.csv)

nam<-str_match(pattern="C:/\\w+/\\w+/\\w+/\\w+/\\w+/\\w+/\\w+/\\w+/(\\d+)",fil.list)[,2]

#name with month 
names(networks)<-month.name[as.numeric(nam)]

#one example
betalink(networks[[1]],networks[[2]])

extract.localwebs(networks[[1]])
network.diss<-betalink.dist(networks)

