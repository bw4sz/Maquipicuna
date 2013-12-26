#Flower Phenology and Phylogenetics
#R script Ben Weinstein - Stony Brook University 7/7/2013
#Under Git repository - Maquipucuna

#Read in required libraries
require(taxize)
require(reshape2)
require(phytools)

#Set DropBox Working Directory
setwd("C:/Users/Ben/Dropbox/")

gitpath<-"C:/Users/Ben/Documents/Maquipicuna/"

#need to run flowerTransects.R, get flower names from file
fl.names<-read.csv("Thesis/Maquipucuna_SantaLucia/Results/FlowerTransects/Iplant_Names.txt",row.names=1)

#Grab those species that taxize recognizes

#Unfortunately, using the tropicos dp leads to lots of conflicts because of the number of subspecies, we just want rank = sp.
#Not a big deal, just can't show here.

uids<-get_ids(gsub("_"," ",fl.names$x[-1]),db="tropicos",ask=TRUE)

#Save image, so we don't have to manually do that everytime.
save.image("Thesis/Maquipucuna_SantaLucia/Results/Phylogeny/PhylogenyTropicos.Rdata")

#Optional start here on restart
#load("Thesis/Maquipucuna_SantaLucia/Results/Phylogeny/PhylogenyTropicos.Rdata")

#Classify
class.taxize<-classification(uids$tropicos)

#Format into a dataframe
m.t<-melt(class.taxize,"ScientificName")

#Remove species with Na
m.t<-m.t[complete.cases(m.t),]

#Create list of plants to enter into Phylomatic
fl.leveln<-levels(factor(m.t$L1))

head(fl.leveln)

#Okay now we have the taxa list, let's first try the taxize:: function to access the phylomatic API
tree_smith <- phylomatic_tree(taxa=fl.leveln, storedtree='smith2011', get='POST')
tree_smith
plot(tree_smith)

tree_APG3 <- phylomatic_tree(taxa=fl.leveln, storedtree='R20120829"', get='POST')
tree_APG3

#Just for fun, try genus only, in case its a species name problem?
# count number of words in a string
GenusOnly<-sapply(fl.leveln,function(x){
    split_x<-strsplit(as.character(x),split=" ")
    split_x[[1]][1]
  })

Gtree_smith <- phylomatic_tree(GenusOnly, storedtree='smith2011', get='POST')
Gtree_smith

tree_APG3 <- phylomatic_tree(taxa=GenusOnly, storedtree='R20120829"', get='POST')
tree_APG3

#Not successful
##Okay, no big deal, i'll try to do it online.
#I'm struggling a bit with the phylocom syntax since the sample is inconsistant, and it also disagrees with the documentation

#Phylomatic sample input reads: malvaceae/durio/durio_zibethinus, which to me looks like family/genus/genus_species

##Formatting for Phylomatic Online
#Family Name

#Go back to taxize classification, for each entry, format in Family/Genus/Genus_Species
class.taxize[[5]]
names(class.taxize[5])

phyloN<-vector()
for (i in 1:length(class.taxize)){
  y<-class.taxize[[i]]  
  
  #Skip if taxize didn't find a hit
  if(is.na(y)){next}
  
  GS<-gsub(" ","_",names(class.taxize[i]))
  
  #If there is not a species name
  if(length(y[y$Rank %in% "genus","ScientificName"])==0){
    phyloN[i]<-paste(y[y$Rank %in% "family","ScientificName"],GS,sep="/")}
  
  #If is genus, no species name
  if(!length(y[y$Rank %in% "genus","ScientificName"])==0){
    str1<-paste(y[y$Rank %in% "genus","ScientificName"],GS,sep="/")
    phyloN[i]<-paste(y[y$Rank %in% "family","ScientificName"],str1,sep="/")}
}

#remove Na's
phyloN[!is.na(phyloN)]

#View
head(phyloN)

#Write to phylomatic output
write.table(phyloO,paste(gitpath,"names.txt",sep=""),row.names=FALSE, col.names=FALSE,quote=FALSE)

#Read in phylomatic output? Try both the phytools and ape options, the phytools just hangs.
#tree<-read.newick(paste(gitpath,"PhyloM.txt",sep=""))
tree<-read.tree(paste(gitpath,"PhyloM.txt",sep=""))

#view the file
