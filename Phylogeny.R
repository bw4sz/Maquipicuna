#Flower Phenology and Phylogenetics
#R script Ben Weinstein - Stony Brook University 7/7/2013
#Under Git repository - Maquipucuna

#Read in required libraries
require(ggplot2)
require(reshape2)
require(maptools)
library(plyr)
require(plotKML)
require(reshape)
require(chron)
require(stringr)
require(picante)
require(rPlant)

#Set DropBox Working Directory
setwd("C:/Users/Ben/Dropbox/")

#need to run flowerTransects.R, get flower names from file

fl.names<-read.csv("Thesis/Maquipucuna_SantaLucia/Results/FlowerTransects/Iplant_Names.txt",row.names=1)

#must have family genus and species
numWords<-sapply(strsplit(as.character(fl.names$x),"_"),length)
#Which species have three words in the them?
initN<-droplevels(fl.names$x[numWords==2])
initGenus<-droplevels(fl.names$x[numWords==1])

#For the moment, while there are some errors with taxize, just grab those species that taxize likes, not a long term solution?
uids<-get_uid(initN)

class.taxize<-classification(uids)

taxize.Sp<-sapply(class.taxize,function(x){
  if(is.na(x)){return(NA)}
  x[x$Rank %in% "species","ScientificName"]
})

#just out of curosity which species match up?
data.frame(initN,taxize.Sp)

#Grab the species taxize sees
toPhylo<-taxize.Sp[!is.na(taxize.Sp)]

#Species attempt.
tree <- phylomatic_tree(taxa=toPhylo, get = 'POST', informat='newick',
                        method = "phylomatic", storedtree = "smith2011",
                        outformat = "newick", clean = "true")

#lots of ugliness there
try2<-tree$tip.label[!is.na(tree$tip.label)][-c(17,18)]

tree <- phylomatic_tree(taxa=try2, get = 'POST', informat='newick',
                        method = "phylomatic", storedtree = "smith2011",
                        outformat = "newick", clean = "true")
#Genus attempt.
tree <- phylomatic_tree(taxa= c(as.character(initGenus))[5:8], get = 'POST', informat='newick',
                        method = "phylomatic", storedtree = "smith2011",
                        outformat = "newick", clean = "true")


plot(tree)
