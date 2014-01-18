###Dissimilairity in Ecological Networks###########

require(ggplot2)
droppath<-"C:/Users/Ben/Dropbox/"
netPath<-paste(droppath,"Thesis/Maquipucuna_SantaLucia/Results/Network/",sep="")

#Following TIm Poiset
#library(devtools); install_github("tpoisot/betalink")

require(betalink)

#read in networks from file.
fil.list<-list.files(paste(netPath,"Temporal",sep="/"),pattern="BirdXFlower",recursive=TRUE,full.names=TRUE)

networks<-lapply(fil.list,read.csv)
