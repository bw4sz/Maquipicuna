#Master Script - Specialization and Niche Overlap in Andean Hummingbirds

#Data Collected by Holger Beck,K. Lohman and B. Weinstein
#R script Ben Weinstein - Stony Brook University 7/7/2013
#Under Git repository - Maquipucuna

#setwd to dropbox
droppath<-"C:/Users/Ben/Dropbox/"
setwd(droppath)
#Set github path
gitpath<-"C:/Users/Ben/Documents/Maquipicuna/"

###############
#The latest version of the input files need to be set at 
list.files(paste(droppath,"Thesis/Maquipucuna_SantaLucia/Data2013/csv",sep=""))

#################
#Flowers
#################
source(paste(gitpath,"FlowerTransects.R",sep=""))

################
#Hummingbirds
################
source(paste(gitpath,"HummingbirdTransect.R",sep=""))

#################
#Network Analysis
#################
source(paste(gitpath,"Network.R",sep=""))

########################
#Phylogenetic Analysis?
########################


#########################
#Morphological Analysis
#########################
source(paste(gitpath,"Morphology.R",sep=""))
