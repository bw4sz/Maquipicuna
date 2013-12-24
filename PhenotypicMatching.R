#Phenotypic Matching Among Plants and Birds

#read in flower morphology data, comes from Nectar.R
fl.morph<-read.csv("Thesis/Maquipucuna_SantaLucia/Results/FlowerMorphology.csv")

#Bring in Hummingbird Morphology Dataset, comes from
hum.morph<-read.csv("Thesis/Maquipucuna_SantaLucia/Results/HummingbirdMorphology.csv")

#Bring in Interaction Matrix from the Network.R script
int<-write.csv("Thesis/Maquipucuna_SantaLucia/Results/Network/HummingbirdInteractions.csv")

#Melt the interaction frame and match it with the traits
m.dat<-melt(int)
colnames(m.dat)<-c("Plant","Bird","int")

#Fix spacing to match clades
m.dat$Bird<-gsub("//."," ",m.dat$Bird)
m.datH<-merge(m.dat,hum.morph, by.x="Bird",by.y="")

#Merge to flowers
m.datH<-merge(m.datH,fl.morph, by.x="Plant",by.y="X")

#Difference Between Corolla and Bill Length


