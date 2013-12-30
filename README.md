Evolutionary Ecology of Andean Hummingbirds
===========================================

*Ben Weinstein - Stony Brook University*

**Location:** Maquipucuna and Santa Lucia Ecoreserves, Pichincha Ecuador.

**Field Data**: Collected by B. Weinstein, H. Beck, K. Lohman, A. Shankar, and N. Munoz

**Trait Data**: F. Gary Stiles

**Phylogenetic Data**: Jimmy McGuire

Goals: 

> 1. Resource niche overlap and hummingbird phylogenetic relatedness
> 2. Phenotypic matching between flower corolla and hummingbird bill morphology
> 3. Temporal cycles of hummingbird resource specialization

Potential Analysis
> 4. Hummingbird food plant phenology as a function of competition for shared pollinaters
> 5. Hummingbird elevation range movements as a function of abiotic and biotic conditions 

Contents
----------

This repository is best run using the Specialization.R wrapper script, which calls all other scripts. 

**Specialization.R**
-------------------
FlowerTransects.R: Computes number of flowers at each elevation transect over time

Nectar.R: Clean flower morphology

Phylogeny.R: Create a genus-level phylogeny using taxize, phylomatic and bladj for smoothed branch lengths

HummingbirdTransects.R: Plot species presence as well as all interactions among plants and birds

Morphology.R: Hummingbird morphology for each of species at the site, perform PCA

Network.R: Compute series of statistics on hummingbird plant interactions

PhenotypicMatching.R: Compare bill and flower corolla traits

TimeSeries.R: Analyze range extent over the sampling period


**Required Packages**
=====================
ggplot2
reshape2
stringr
maptools
plotKML
Rplant
taxize
picante
chron
