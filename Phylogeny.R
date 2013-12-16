#Flower Phenology 
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

#Set DropBox Working Directory
setwd("C:/Users/Jorge/Dropbox/")

#need to run flowerTransects.R
#Read in workspace if desired for quick access
load("Thesis/Maquipucuna_SantaLucia/Results/FlowerTransect.Rdata")

#Clean flowers for intial try at resolution
#must have family genus and species
numWords<-sapply(gregexpr("\\W+", fl.all$Species), length) + 1

#Which species have three words in the them?
initN<-droplevels(fl.all$Species[numWords==3])

#Get rid of records with sp.
toRemove<-str_detect(initN,"sp")

#Remove sp with ?
initN<-initN[!toRemove]
toRemove<-str_detect(initN,fixed('?'))

#Remove species with spaces
initN<-as.character(initN[!toRemove])
toRemove<-str_detect(initN,' ')

#Still has some spaces
threeSp<-sapply(initN,function(x){
  length(strsplit(x," ")[[1]])
}) 

initN<-initN[threeSp == 3]

#which species have the name in the tropicos database
new.N<-ResolveNames(initN, max.per.call=100, verbose=TRUE)
CompareNames(initN, new.N, verbose=TRUE)

#conservatively, use these matches
inTrop<-sapply(strsplit(initN," "),function(x){  paste(x[2],x[3],sep="_")}) %in% new.N

initN<-initN[inTrop]

#manually take out any nonsense species
namesFl<-initN[-c(4,17,41,54)]

#sorry looks like it just wants genus and species

write.table(sapply(strsplit(initN," "),function(x){  paste(x[2],x[3],sep="_")}),"names.txt",row.names=FALSE,col.names=FALSE)


#After run of phyloGenerator
tre<-read.tree("C:/Program Files (x86)/phyloGeneratorPC/phyloGenerator/test_phylogeny.tre")
plot(tre)
