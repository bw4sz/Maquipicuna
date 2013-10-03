require(bipartite)
require(ggplot2)
require(ape)
require(reshape)
require(sna)
#Read in flower videos
setwd("C:/Users/Jorge/Dropbox/Thesis/Maquipucuna_SantaLucia/Data2013/csv/")

dat<-read.csv("FlowerVideo.csv")

#Bring in the phylogeny

#Read in phylogeny
tree<-read.nexus("C:/Users/Jorge/Dropbox/Shared Ben and Catherine/DimDivEntire/Files for Analysis/ColombiaPhylogenyUM.tre")

#Read in names file to replace names in Nexis file
spnames<-read.table(file="C:/Users/Jorge/Dropbox/Shared Ben and Catherine/DimDivEntire/Files for Analysis/SpNameTree.txt" , sep = "\t", header = TRUE)

#Replace tip.label with Spnames#
tree$tip.label<-as.character(spnames$SpName) 
head(dat)

#For the sake of simplicity, make everything lowercase

.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}


#bring in clade data
clades<-read.csv("C:\\Users/Jorge/Dropbox/Shared Ben and Catherine/DimDivEntire/Files for Analysis/CladeList.txt",header=FALSE)[,-1]
colnames(clades)<-c("Clade","Genus","Species","double","English")
clades<-clades[,1:5]

####################################################
#Begin Analysis
####################################################

#Capitalize Flowers
dat$Flower<-factor(sapply(dat$Flower,function(x) {.simpleCap(as.character(x))}))

#Caps Hummingbird
dat$Hummingbird<-factor(sapply(dat$Hummingbird,function(x) {.simpleCap(as.character(x))}))

#Overall statistics
#How many flowers Species
nlevels(dat$Flower)

#How many Birds Species
nlevels(dat$Hummingbird)

#Interaction of flowers and birds
F_H<-as.data.frame.array(table(dat$Flower,dat$Hummingbird))

require(bipartite)
?bipartite

#View Web
plotweb(F_H)

#In a different sense
visweb(F_H)

#Metrics across entire
birds.prop<-networklevel(F_H,level="higher")
plants.prop<-networklevel(F_H,level="lower")

#Metrics across species
H.species.prop<-specieslevel(F_H,level="higher")
P.species.prop<-specieslevel(F_H,level="lower")

#Specialization, but note the insane comment box ?nodespec, ask ppl who know better. 
nodespec(F_H)
#For plants
dfun(F_H)
#For birds
birds.special<-dfun(t(F_H))
birds.spl<-data.frame(lapply(birds.special,data.frame))
colnames(birds.spl)<-names(birds.special)
ggplot(birds.spl,aes(x=rownames(birds.spl),y=dprime)) + geom_point() + theme_bw() + theme(axis.text.x=element_text(angle=90))

#How old is the taxa, color the above specialization graph.
graph.bipartite(F_H)
#As one way matrices 
#Hummingbird
H_H<-as.one.mode(F_H,project="higher")

diag(H_H)<-NA
m.HH<-melt(H_H)
ggplot(m.HH,aes(X1,X2,fill=value)) + geom_tile() + scale_fill_continuous(low="blue",high="red") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Relatedness and flower overlap, very rudimentary test so far
ctrx<-cophenetic(tree)
         
    ER<-function(x){
      print(x)
    y<-m.HH[x,]
    if(sum(clades$English %in% y[[1]])==0) {return("NA")}
    if(sum(clades$English %in% y[[2]])==0) {return("NA")}
    sp1<-gsub(" ","_",clades[clades$English %in% y[[1]],"double"])
    sp2<-gsub(" ","_",clades[clades$English %in% y[[2]],"double"])
    
      return(tryCatch(ctrx[sp1,sp2],error=function(e) print("NA")))
    }

#get cophenetic distance between species
m.HH$Relatedness<-sapply(1:nrow(m.HH),ER)

#Relatedness and plant overlap
ggplot(m.HH[m.HH$value>0,],aes(y=value,x=as.numeric(Relatedness),)) + geom_point() + geom_smooth(method="lm") + theme_bw()


#Plants 
P_P<-as.one.mode(F_H,project="lower")
m.PP<-melt(P_P)
#
ggplot(m.PP,aes(X1,X2,fill=value)) + geom_tile() + scale_fill_continuous(low="blue",high="red") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#package sna, gplot produces some nice networks

gplot(H_H)
