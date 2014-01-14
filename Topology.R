#Network Topology
require(ggplot2)

gitpath<-"C:/Users/Jorge/Maquipicuna/"
droppath<-"C:/Users/Jorge/Dropbox"

#Read in interaction matrix from Network.R
dat<-read.csv("Thesis/Maquipucuna_SantaLucia/Results/Network/HummingbirdInteractions.csv")

#Drop any unused factors?
dat<-droplevels(dat)

#Drop any observations without plants
dat<-dat[!dat$Iplant_Double %in% "",]

#Interaction of flowers and birds
F_H<-as.data.frame.array(table(dat$Iplant_Double,dat$Hummingbird))

d<-as.matrix(F_H)

##########################
#Descriptive Statistcs
##########################

#Range
range(d)
#Histogram
hist(d,breaks=100) # hard to see

table(d)
plot(table(d))

#Dimensions
dim(d)
#Sum
sum(d)
#Cumulative distributioon 
plot(ecdf(d))

mean(d)

median(d)

###################
#Fit distributions
####################

#Normal qqplot
#Standardized data
z.norm<-(d-mean(d))/sd(d) ## standardized data
qqnorm(z.norm)
abline(0,1,col="red")

#Plus there are no negative quantiles...
qqnorm(z.norm,xlim=c(-3,3),ylim=c(-3,3))
abline(0,1,col="red")

#I don't even know how to interpret a plot that weird.

##############Poisson############