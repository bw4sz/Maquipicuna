#Sampling
library(reshape2)
library(dplyr)
library(ggplot2)

#Observed Interactions
datg<-read.csv(datg,"Thesis/Maquipucuna_SantaLucia/Data2013/csv/FlowerVideoClean.csv")

#Potential non-observed interactions, doing to need to go backwards
#Nelly
nelldat<-read.csv("C:/Users/Ben/Dropbox/Thesis/Maquipucuna_SantaLucia/Data2013/CameraData Nelly Ultima version.csv")[-83,]

#Holger
holgcam<-read.csv(file = "C:/Users/Ben/Dropbox/Thesis/Maquipucuna_SantaLucia/Data2013/csv/Camera_Protocol.csv")

#Ben
ben<-read.csv("C:/Users/Ben/Dropbox/Thesis/Maquipucuna_SantaLucia/Data2013/csv/FlowerVideoempty.csv")
