#Master Script - Specialization and Niche Overlap in Andean Hummingbirds

#Data Collected by Holger Beck,K. Lohman and B. Weinstein
#R script Ben Weinstein - Stony Brook University 7/7/2013
#Under Git repository - Maquipucuna

#setwd to dropbox
droppath<-"C:/Users/Ben/Dropbox/"
setwd(droppath)
#Set github path
gitpath<-"C:/Users/Ben/Documents/Maquipicuna/"

##########################
#The latest version of the input files need to be set at 
list.files(paste(droppath,"Thesis/Maquipucuna_SantaLucia/Data2013/csv",sep=""))

#First set of scripts are desciptive that clean, aggregate and quantify flowers, morphology, and hummingbird interactions

##########################
#Format Geospatial Data - 
#read in gps and create spatial objects
##########################

source(paste(gitpath,"GPSMatching.R",sep=""))

#########################
#Flowers
#########################

source(paste(gitpath,"FlowerTransects.R",sep=""))
#Load from source:
#load("Thesis/Maquipucuna_SantaLucia/Results/FlowerTransect.Rdata")

#########################
#Flower GPS
#########################

source(paste(gitpath,"GPSFlowerTransects.R",sep=""))

#########################
#Flower Morphological
#########################

source(paste(gitpath,"Nectar.R",sep=""))
load("Thesis/Maquipucuna_SantaLucia/Results/Nectar.Rdata")

########################
#Flower Phylogeny using Phylomatic
########################

#source(paste(gitpath,"Phylogeny.R",sep=""))
#load("Thesis/Maquipucuna_SantaLucia/Results/Phylogeny/PhylogenyTropicos.Rdata")

########################
#Hummingbirds
########################

source(paste(gitpath,"HummingbirdTransect.R",sep=""))
#load("Thesis/Maquipucuna_SantaLucia/Results/HummingbirdTransect.Rdata")

#################
#Network Analysis
#################

source(paste(gitpath,"Network.R",sep=""))

#Or Load from source:
#load("Thesis/Maquipucuna_SantaLucia/Results/Network/NetworkData.Rdata")

#########################
#Hummingbird Morphological Analysis
#########################

source(paste(gitpath,"Morphology.R",sep=""))
load("Thesis/Maquipucuna_SantaLucia/Results/Morphology.Rdata")

#########################
#Time Series Analysis of Hummingbird Presence along Elevation
#########################

source(paste(gitpath,"TimeSeries.R",sep=""))

#########################
#Phenotypic Matching 
#########################

source(paste(gitpath,"PhenotypicMatching.R",sep=""))
#load("Thesis/Maquipucuna_SantaLucia/Results/PhenotypicMatching.Rdata")

########################
#Null Models
########################

source(paste(gitpath,"NullModels.R",sep=""))

########################
#Network Topology
#######################

#source(paste(gitpath,"Topology.R",sep=""))

########################
#Network Dissimilarity
#######################

#source(paste(gitpath,"NetworkDiss.R",sep=""))

