#Flower Phenology and Phylogenetics
#R script Ben Weinstein - Stony Brook University 7/7/2013
#Under Git repository - Maquipucuna

#Read in required libraries
require(taxize)
require(reshape2)
require(phytools)

#Set DropBox Working Directory
setwd("C:/Users/Jorge/Dropbox/")

gitpath<-"C:/Users/Jorge/Documents/Maquipicuna/"

#need to run flowerTransects.R, get flower names from file
fl.names<-read.csv("Thesis/Maquipucuna_SantaLucia/Results/FlowerTransects/Iplant_Names.txt",row.names=1)

#Grab those species that taxize recognizes

#Unfortunately, using the tropicos dp leads to lots of conflicts because of the number of subspecies, we just want rank = sp.
#Not a big deal, just can't show here.

uids<-get_ids(gsub("_"," ",fl.names$x[-1]),db="tropicos",ask=FALSE)

#Save image, so we don't have to manually do that everytime.
save.image("Thesis/Maquipucuna_SantaLucia/Results/Phylogeny/PhylogenyTropicos.Rdata")

#Optional start here on restart
load("Thesis/Maquipucuna_SantaLucia/Results/Phylogeny/PhylogenyTropicos.Rdata")

#Classify
class.taxize<-classification(uids$tropicos)

#name the output
names(class.taxize)<-names(uids$tropicos)

phyloN<-vector()
for (i in 1:length(class.taxize)){
  y<-class.taxize[[i]]  
  
  #Skip if taxize didn't find a hit
  if(is.na(y)){next}
  
  GS<-gsub(" ","_",names(class.taxize[i]))
  
  #If there is not a species name
  if(length(y[y$Rank %in% "genus","name"])==0){
    phyloN[i]<-paste(y[y$rank %in% "family","name"],GS,sep="/")}
  
  #If is genus, no species name
  if(!length(y[y$rank %in% "genus","name"])==0){
    str1<-paste(y[y$rank %in% "genus","name"],GS,sep="/")
    phyloN[i]<-paste(y[y$rank %in% "family","name"],str1,sep="/")}
}

#remove Na's
phyloN[!is.na(phyloN)]

#View
head(phyloN)

#Write to phylomatic output
write.table(phyloN,paste(gitpath,"names.txt",sep=""),row.names=FALSE, col.names=FALSE,quote=FALSE)
phyloN.f<-phyloN[!is.na(phyloN)]
       
#Just for fun, try genus only, in case its a species name problem?
# count number of words in a string
GenusOnly<-sapply(phyloN.f,function(x){
    split_x<-strsplit(x,split="/")
    split_x[[1]][2]
  })

#Format lists

#Phylomatic Genus Tree - no branch Lengths
tree_APG3 <- phylomatic_tree(taxa=GenusOnly, storedtree='R20120829', get='POST')
tree_APG3
plot(tree_APG3)

#Phylomatic Species Tree, does it matter, it places polytomies as branches
tree_APG3 <- phylomatic_tree(taxa=phyloN[!is.na(phyloN)], storedtree='R20120829', get='POST',taxnames=FALSE)
tree_APG3
plot(tree_APG3,cex=.7)

#write tree to file
write.tree(tree_APG3,paste(gitpath,"InputData/FlowerSPPhylogeny.tre",sep=""))

save.image("Thesis/Maquipucuna_SantaLucia/Results/Phylogeny/PhylogenyTropicos.Rdata")

#load already made tree?
load("Thesis/Maquipucuna_SantaLucia/Results/Phylogeny/PhylogenyTropicos.Rdata")

# read, then write files to where phylocom executable is
age <- read.table(ages)
write.tree(tree_APG3, paste(gitpath,"InputData/phylo",sep=""))
write.table(age, file = paste(gitpath,"ages",sep=""), sep = "\t", col.names = F, row.names = F, quote = F)

#Run phylocom's bladj, need to set in the PATH!
bladjT<-system(paste("phylocom bladj > ",sep=gitpath,"OutData/phyloout.txt"),intern=TRUE) 

#create tree
tree<-read.tree(text=bladjT[[1]])

plot(tree,cex=.5)

#relatedness among plants
pco<-cophenetic(tree)

write.csv(pco,paste(gitpath,"InputData/PlantRelatedness.csv",sep=""))
