#Network Topology
require(ggplot2)

gitpath<-"C:/Users/Ben/Maquipicuna/"
droppath<-"C:/Users/Ben/Dropbox"

setwd(droppath)

#Read in interaction matrix from Network.R
dat<-read.csv("Thesis/Maquipucuna_SantaLucia/Results/Network/HummingbirdInteractions.csv",row.names=1)

#Drop any unused factors?
dat<-droplevels(dat)

#Drop any observations without plants
dat<-dat[!dat$Iplant_Double %in% "",]

#just take the camera data?
datC<-dat[!is.finite(dat$Video),]

#Interaction of flowers and birds
F_H<-as.data.frame.array(table(datC$Iplant_Double,datC$Hummingbird))

d<-as.matrix(F_H)

#Still appears there are blank records?
d<-d[!rownames(d) %in% "",]

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

hist(d,freq=FALSE,breaks=100)
curve(dnorm(x,mean=mean(d),sd=sd(d)),add=TRUE,col="red")

#Ks test
ks.test(d,rnorm(n=length(d),mean=mean(d),sd=sd(d)))

##############Poisson############
ks.test(d,rpois(n=length(d),lambda=mean(d)))

hist(d,freq=FALSE,breaks=100)
curve(dpois(x,lambda=mean(d)),add=TRUE,col="red",type="l")

##############ZeroInflatedPoisson############

#Fit a binomial to 0/1
binomD<-(d>0)*1

#Find the probability of success
probD<-table(binomD)[[2]]/length(d)

hist(binomD,breaks=100)
curve(dbinom(x,size=1,prob=probD)*length(binomD),add=TRUE,col="red",type="b")

#Fit the 0's
ks.test(binomD,rbinom(n=length(binomD),size=1,prob=probD))

#Fit the 1's
binomP<-d[!d==0]

hist(binomP,breaks=100,freq=FALSE,xlim=c(0,10))
curve(dpois(x,lambda=mean(binomP)),add=TRUE,col="red",type="l")

ks.test(binomP,rpois(n=length(binomP),lambda=mean(binomP)))

#Log transform, add +.001 to not get infinite values?
rlnorm(n=ld,meanlog=mean(ld),sdlog=sd(ld))

ld<-log(d+.0001)



