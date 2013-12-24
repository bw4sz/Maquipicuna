Comparison of rPLant and Taxize in resolving plant names from Maquipucuna Ecuador
========================================================

I'm having some trouble understanding the differences in outputs from rPlant's resolveNames with calls.

I really love the tools in taxize, and am very eager to try the phylomatic options. For my workflow, i was first using the rPlant ResolveNames fuction because of its nice fuzzy matching of misspelled species (lots of field assistants!). I would then run the outputs of the rPlant search into taxize to check if there are have been taxonomy changes. I'm finding that many records in rPlant, and corroborated online as good species names, are failing to be found within taxize. 

As a quick reference here are the eol pages for five sample species
http://eol.org/pages/1118538/overview
http://eol.org/pages/8767588/overview
http://eol.org/pages/1118438/overview
http://eol.org/pages/8767461/overview (although ironically i can see that the images for this species are of the wrong genus on eol)
http://eol.org/pages/1107632/overview

Also works from another source:
http://www.theplantlist.org/tpl/record/kew-84010



```r
require(rPlant)
```

```
## Loading required package: rPlant
## Loading required package: rjson
## Loading required package: RCurl
## Loading required package: bitops
## Loading required package: seqinr
## Loading required package: knitcitations
## Loading required package: bibtex
## 
## Attaching package: 'knitcitations'
## 
## The following object is masked from 'package:utils':
## 
##     cite
```

```r
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

# Create some test names, some that match, some that disagree, etc. I can
# push a file to github if you would like to see the full naming lists (has
# some ugly NA species).


nam <- c("Heliconia_virginalis", "Guzmania_amplectens", "Macleania_bullata", 
    "psamisia_ulbrichiana", "faramae calyptrata")  # In this list, the first three are good species, the last two are intentionally misspelled ('psammisia', 'faramea')

# First rplant
rPlant_N <- ResolveNames(nam)
```

```
## [1] "Heliconia_virginalis"  "Guzmania_amplectens"   "Macleania_bullata"    
## [4] "Psammisia_ulbrichiana" "Faramea_calyptrata"
```

```r
CompareNames(nam, rPlant_N)
```

```
## 
## [1] "psamisia_ulbrichiana  was changed to  Psammisia_ulbrichiana "
## 
## [1] "faramae calyptrata  was changed to  Faramea_calyptrata "
## [1] "2 taxa changed names according to TNRS"
```

```r
# That was a success.

# Taxize to reflect current taxonomy.
uids <- get_uid(rPlant_N)
```

```
## 
## Retrieving data for taxon 'Heliconia_virginalis'
## 
## Not found. Consider checking the spelling or alternate classification
## 
## Retrieving data for taxon 'Guzmania_amplectens'
## 
## Not found. Consider checking the spelling or alternate classification
## 
## Retrieving data for taxon 'Macleania_bullata'
## 
## 
## Retrieving data for taxon 'Psammisia_ulbrichiana'
## 
## 
## Retrieving data for taxon 'Faramea_calyptrata'
## 
## Not found. Consider checking the spelling or alternate classification
```

```r
classification(uids)
```

```
## [[1]]
## [1] NA
## 
## [[2]]
## [1] NA
## 
## [[3]]
##        ScientificName         Rank     UID
## 1  cellular organisms      no rank  131567
## 2           Eukaryota superkingdom    2759
## 3       Viridiplantae      kingdom   33090
## 4        Streptophyta       phylum   35493
## 5      Streptophytina      no rank  131221
## 6         Embryophyta      no rank    3193
## 7        Tracheophyta      no rank   58023
## 8       Euphyllophyta      no rank   78536
## 9       Spermatophyta      no rank   58024
## 10      Magnoliophyta      no rank    3398
## 11    Mesangiospermae      no rank 1437183
## 12     eudicotyledons      no rank   71240
## 13       Pentapetalae      no rank 1437201
## 14           asterids     subclass   71274
## 15           Ericales        order   41945
## 16          Ericaceae       family    4345
## 17      Vaccinioideae    subfamily  217037
## 18         Vaccinieae        tribe  217062
## 19          Macleania        genus   57528
## 20  Macleania bullata      species   57529
## 
## [[4]]
##           ScientificName         Rank     UID
## 1     cellular organisms      no rank  131567
## 2              Eukaryota superkingdom    2759
## 3          Viridiplantae      kingdom   33090
## 4           Streptophyta       phylum   35493
## 5         Streptophytina      no rank  131221
## 6            Embryophyta      no rank    3193
## 7           Tracheophyta      no rank   58023
## 8          Euphyllophyta      no rank   78536
## 9          Spermatophyta      no rank   58024
## 10         Magnoliophyta      no rank    3398
## 11       Mesangiospermae      no rank 1437183
## 12        eudicotyledons      no rank   71240
## 13          Pentapetalae      no rank 1437201
## 14              asterids     subclass   71274
## 15              Ericales        order   41945
## 16             Ericaceae       family    4345
## 17         Vaccinioideae    subfamily  217037
## 18            Vaccinieae        tribe  217062
## 19             Psammisia        genus  180722
## 20 Psammisia ulbrichiana      species  249297
## 
## [[5]]
## [1] NA
```

```r
# Flags two errors
```



```r
gnr_resolve("Guzmania jaramilloi")
```

```
##        submitted_name        matched_name data_source_title score
## 1 Guzmania jaramilloi Guzmania jaramilloi          Freebase 0.988
## 2 Guzmania jaramilloi Guzmania jaramilloi               EOL 0.988
## 3 Guzmania jaramilloi Guzmania jaramilloi     uBio NameBank 0.988
```

```r
gnr_resolve("Guzmania_jaramilloi")
```

```
## Error: arguments imply differing number of rows: 1, 0
```

```r

get_uid("Guzmania jaramilloi")
```

```
## 
## Retrieving data for taxon 'Guzmania jaramilloi'
## 
## Not found. Consider checking the spelling or alternate classification
```

```
## [1] NA
## attr(,"match")
## [1] "not found"
## attr(,"class")
## [1] "uid"
```

```r
get_uid("Guzmania_jaramilloi")
```

```
## 
## Retrieving data for taxon 'Guzmania_jaramilloi'
## 
## Not found. Consider checking the spelling or alternate classification
```

```
## [1] NA
## attr(,"match")
## [1] "not found"
## attr(,"class")
## [1] "uid"
```


My ultimate goal is to get a species list of accepted names, such that i can input them into the phylomatic_tree, i'm aiming for a genus level phylogeny, but that's the next question.

As always i appreciate your work, insight, and leadership. The RopenSci community is an incredible project.
