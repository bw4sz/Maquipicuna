Using Taxize and Phylomatic
========================================================

For the script, i'm loading in an image, since the get_id call from topicos needs some user input, just to settle subspecies declarations in the database.
To avoid this in the markdown, just load an image that has previously performed this first block of code


```r
# Flower Phenology and Phylogenetics R script Ben Weinstein - Stony Brook
# University 7/7/2013 Under Git repository - Maquipucuna

# Read in required libraries
require(taxize)
```

```
## Loading required package: taxize
## 
## 
## New to taxize? Tutorial at http://ropensci.org/tutorials/taxize_tutorial.html 
## API key names have changed. Use tropicosApiKey, eolApiKey, ubioApiKey, and pmApiKey in your .Rprofile file. 
## Use suppressPackageStartupMessages() to suppress these startup messages in the future
```

```r
require(reshape2)
```

```
## Loading required package: reshape2
```

```r
require(phytools)
```

```
## Loading required package: phytools
## Loading required package: ape
## Loading required package: maps
## Loading required package: rgl
```

```r

# Set DropBox Working Directory
setwd("C:/Users/Ben/Dropbox/")

# gitpath<-'C:/Users/Ben/Documents/Maquipicuna/'

# need to run flowerTransects.R, get flower names from file
# fl.names<-read.csv('Thesis/Maquipucuna_SantaLucia/Results/FlowerTransects/Iplant_Names.txt',row.names=1)

# Grab those species that taxize recognizes

# Unfortunately, using the tropicos dp leads to lots of conflicts because of
# the number of subspecies, we just want rank = sp. Not a big deal, just
# can't show here.

# uids<-get_ids(gsub('_',' ',fl.names$x[-1]),db='tropicos',ask=TRUE)

# Save image, so we don't have to manually do that everytime.
# save.image('Thesis/Maquipucuna_SantaLucia/Results/Phylogeny/PhylogenyTropicos.Rdata')
```


Start code from here


```r
setwd("C:/Users/Ben/Dropbox/")

# Optional start here on restart
load("Thesis/Maquipucuna_SantaLucia/Results/Phylogeny/PhylogenyTropicos.Rdata")

# Classify
class.taxize <- classification(uids$tropicos)

# Format into a dataframe
m.t <- melt(class.taxize, "ScientificName")

# Remove species with Na
m.t <- m.t[complete.cases(m.t), ]

# Create list of plants to enter into Phylomatic
fl.leveln <- levels(factor(m.t$L1))

head(fl.leveln)
```

```
## [1] "Alloplectus"               "Alloplectus purpureus"    
## [3] "Alloplectus tenuis"        "Alloplectus tetragonoides"
## [5] "Alloplectus teuscheri"     "Begonia longirostris"
```

```r

# Okay now we have the taxa list, let's first try the taxize:: function to
# access the phylomatic API
tree_smith <- phylomatic_tree(taxa = fl.leveln, storedtree = "smith2011", get = "POST")
```

```
## Warning: NAs introduced by coercion
## Warning: NAs introduced by coercion
```

```r
tree_smith
```

```
## 
## Phylogenetic tree with 38 tips and 12 internal nodes.
## 
## Tip labels:
## 	Gasteranthus_leopardus, Gasteranthus_quitensis, Besleria_solanoides, Columnea_picta, Palicourea_lineata, Centropogon_solanifolius, ...
## Node labels:
## 	Erythrina_megistophylla, Columnea_cinerea, Alloplectus_tetragonoides, Alloplectus_purpureus, Bomarea_multiflora, Columnea_strigosa, ...
## 
## Rooted; includes branch lengths.
```

```r
plot(tree_smith)
```

```
## Error: tree badly conformed; cannot plot. Check the edge matrix.
```

```r

tree_APG3 <- phylomatic_tree(taxa = fl.leveln, storedtree = "R20120829\"", get = "POST")
tree_APG3
```

```
## 
## Phylogenetic tree with 2 tips and 1 internal nodes.
## 
## Tip labels:
## [1] "``" "``"
## Node labels:
## [1] "``"
## 
## Rooted; no branch lengths.
```


Okay, that didn't work, i'm not entirely sure if it was because the taxa don't match, there is an error in syntax, or that there isn't another resolution.


```r
# Just for fun, try genus only, in case its a species name problem?  count
# number of words in a string
GenusOnly <- sapply(fl.leveln, function(x) {
    split_x <- strsplit(as.character(x), split = " ")
    split_x[[1]][1]
})

Gtree_smith <- phylomatic_tree(GenusOnly, storedtree = "smith2011", get = "POST")
Gtree_smith
```

```
## NULL
```

```r

tree_APG3 <- phylomatic_tree(taxa = GenusOnly, storedtree = "R20120829\"", get = "POST")
tree_APG3
```

```
## 
## Phylogenetic tree with 2 tips and 1 internal nodes.
## 
## Tip labels:
## [1] "``" "``"
## Node labels:
## [1] "``"
## 
## Rooted; no branch lengths.
```


Not successful

**Try using the Phylomatic Website.**

I'm struggling a bit with the phylocom syntax since the sample is inconsistant, and it also disagrees with the documentation

Phylomatic sample input reads: malvaceae/durio/durio_zibethinus, which to me looks like family/genus/genus_species
======================
Formatting for Phylomatic Online
---------------------------------
#Go back to taxize classification, for each entry, format in Family/Genus/Genus_Species

```r
class.taxize[[5]]
```

```
##      NameId ScientificName       Rank
## 1  43000109  Equisetopsida      class
## 2  43000013    Magnoliidae   subclass
## 3 100352415      Asteranae superorder
## 4  43000074       Lamiales      order
## 5  42000302   Gesneriaceae     family
## 6  40010389    Alloplectus      genus
```

```r
names(class.taxize[5])
```

```
## [1] "Alloplectus teuscheri"
```


Loop through the classification, collecting the family genus and species (if there is a species name given)
===========


```r
phyloN <- vector()
for (i in 1:length(class.taxize)) {
    y <- class.taxize[[i]]
    
    # Skip if taxize didn't find a hit
    if (is.na(y)) {
        next
    }
    
    GS <- gsub(" ", "_", names(class.taxize[i]))
    
    # If there is not a species name
    if (length(y[y$Rank %in% "genus", "ScientificName"]) == 0) {
        phyloN[i] <- paste(y[y$Rank %in% "family", "ScientificName"], GS, sep = "/")
    }
    
    # If is genus, no species name
    if (!length(y[y$Rank %in% "genus", "ScientificName"]) == 0) {
        str1 <- paste(y[y$Rank %in% "genus", "ScientificName"], GS, sep = "/")
        phyloN[i] <- paste(y[y$Rank %in% "family", "ScientificName"], str1, 
            sep = "/")
    }
}

# remove Na's
phyloN <- phyloN[!is.na(phyloN)]

# View
head(phyloN)
```

```
## [1] "Gesneriaceae/Alloplectus"                          
## [2] "Gesneriaceae/Alloplectus/Alloplectus_purpureus"    
## [3] "Gesneriaceae/Alloplectus/Alloplectus_tenuis"       
## [4] "Gesneriaceae/Alloplectus/Alloplectus_tetragonoides"
## [5] "Gesneriaceae/Alloplectus/Alloplectus_teuscheri"    
## [6] "Begoniaceae/Begonia/Begonia_longirostris"
```


Write to phylomatic output

```r
write.table(phyloN, paste(gitpath, "names.txt", sep = ""), row.names = FALSE, 
    col.names = FALSE, quote = FALSE)
```


#Read in phylomatic output 
---------------------------


```r
# Try both the phytools and ape options, the phytools just hangs.
# tree<-read.newick(paste(gitpath,'PhyloM.txt',sep=''))
tree <- read.tree(paste(gitpath, "PhyloM.txt", sep = ""))
```



Actual output file can be download and seen: 
------------------------------------
https://github.com/bw4sz/Maquipicuna/blob/master/phyloM.txt

```
