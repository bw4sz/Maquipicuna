###Flower Video Cleaning

#Read in Flower Camera Dataset
dat<-read.csv("Thesis/Maquipucuna_SantaLucia/Data2013/csv/FlowerVideo.csv")


#Fix date format
dat$Month<-as.numeric(format(as.Date(dat$Date,"%m/%d/%Y"),"%m"))

###########################
#Hummingbird Data Cleaning 
###########################

#Caps Hummingbird
dat$Hummingbird<-factor(sapply(dat$Hummingbird,function(x) {.simpleCap(as.character(x))}))

#make a object, just to save typing
h<-levels(dat$Hummingbird)

#can taxize do english names? 

#Fix common mistakes
h[h %in% "Fawn Breasted Brilliant"] <- "Fawn-breasted Brilliant"
h[h %in% "Gorgetted Sunangel"]<-"Gorgetted Sunangel"
h[h %in% "Violet-tailed Slyph"]<-"Violet-tailed Sylph"
h[h %in% "Booted Racketail"]<-"Booted Racket-tail"
h[h %in% "Green-crowned Woodnymph"]<-"Crowned Woodnymph" 

levels(dat$Hummingbird) <- h

#Take our any bad data
dat_e<-droplevels(dat[!dat_e$Hummingbird %in% c("","NANA","UKWN","Ukwn"),])

#Remove out piercing events for now?
table(dat$Piercing)
datPierce<-dat[dat_e$Piercing %in% c("Yes","YES"),]
dat_e<-dat[!dat$Piercing %in% c("Yes","YES"),]
