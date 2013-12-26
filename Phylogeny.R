#Flower Phenology and Phylogenetics
#R script Ben Weinstein - Stony Brook University 7/7/2013
#Under Git repository - Maquipucuna

#Read in required libraries
require(taxize)
require(reshape2)
require(phytools)

#Set DropBox Working Directory
setwd("C:/Users/Ben/Dropbox/")

#need to run flowerTransects.R, get flower names from file
fl.names<-read.csv("Thesis/Maquipucuna_SantaLucia/Results/FlowerTransects/Iplant_Names.txt",row.names=1)

# #must have family genus and species
numWords<-sapply(strsplit(as.character(fl.names$x),"_"),length)
# #Which species have three words in the them?
initN<-droplevels(fl.names$x[numWords==2])
initGenus<-droplevels(fl.names$x[numWords==1])

#For the moment, while there are some errors with taxize, just grab those species that taxize likes, not a long term solution?

uids<-get_ids(gsub("_"," ",fl.names$x[-1]),db="tropicos",ask=FALSE)

#remove Na's
ui<-uids[[1]][!is.na(uids[[1]])]
class.taxize<-classification(uids,db="tropicos")

m.t<-melt(class.taxize,"ScientificName")

#Remove species with Na
m.t<-m.t[complete.cases(m.t),]
fl.leveln<-levels(factor(m.t$L2))

#got get the family name?
uids<-get_ids(fl.leveln,db="tropicos",ask=FALSE)
class.taxize<-classification(uids,db="tropicos")

familys<-sapply(class.taxize[[1]],function(x){
  x[x$Rank %in% "family","ScientificName"]
})

genera<-sapply(class.taxize[[1]],function(x){
  x[x$Rank %in% "genus","ScientificName"]
})

nam.df<-data.frame(F=familys,GS=fl.leveln)

phyloO<-gsub(" ","/",paste(nam.df$F,nam.df$GS,sep="/"))
#Write to phylomatic output

write.table(phyloO,paste(gitpath,"names.txt",sep=""),row.names=FALSE, col.names=FALSE,quote=FALSE)

#Read in phylomatic output?
tree<-read.newick(paste(gitpath,"PhyloM.txt",sep=""))
tree<-read.tree(paste(gitpath,"Phylomatic_out.new",sep=""))

read.newick(text=tree)
  c.tree<-collapse.singles(tree)
plot(c.tree)
       complete.tax<-split(m.t,m.t$L2)

fl.leveln<-levels(factor(m.t$L2))

taxa <- c("Collomia grandiflora", "Lilium lankongense", "Phlox diffusa",
          "Iteadaphne caudata", "Nicotiana tomentosa", "Gagea sarmentosa")
tree <- phylomatic_tree(taxa=as.character(initGenus)[-c(10:15)], get = 'POST', informat='newick', method = "phylomatic", storedtree = "R20120829",outformat = "newick", clean = "true")
plot(tree)
tree <- phylomatic_tree(taxa=taxa, get = 'POST', informat='newick', method = "phylomatic", storedtree = "smith2011",outformat = "newick", clean = "false")

tree <- phylomatic_tree(taxa=fl.leveln, storedtree='smith2011', get='POST')


#Species attempt.
for (x in 1:length(fl.leveln)){
  print(x)
tree <- phylomatic_tree(taxa=fl.leveln[1:x])
  print(tree)
try(plot(tree))
}

plot(tree)

#Write a for loop to track errors?
#has problem with 8,10,22?

#lots of ugliness there
try2<-tree$tip.label[!is.na(tree$tip.label)][-c(17,18)]

tree <- phylomatic_tree(taxa=try2, get = 'POST', informat='newick',
                        method = "phylomatic", storedtree = "smith2011",
                        outformat = "newick", clean = "true")
#Genus attempt.
tree <- phylomatic_tree(taxa= c(as.character(initGenus))[5:8], get = 'POST', informat='newick',
                        method = "phylomatic", storedtree = "smith2011",
                        outformat = "newick", clean = "true")

nodelabels(tree)
plot(tree)

#save.image(paste(droppath))