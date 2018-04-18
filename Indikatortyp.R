#####################################
#Inhalt:  Macro Micro Arzt 	        #
#Datum: 	Januar 2018			          #
#Autor: 	mape			 	              #
#####################################

##--Import Packages--##
library(rgdal)
library(plyr)

#--Achtung!!!: In Zeile 33 zu beruecksichtigen bei der Umbenennung der Spalten.
# E.Arzt$Fahrzeit_OEV = E.Arzt$Minuten_OEV-E.Arzt$Anbindungszeit-E.Arzt$Abgangszeit
# E.Arzt[E.Arzt$Fahrzeit_OEV<0,]$Fahrzeit_OEV <- 10
# E.OZ$Fahrzeit_OEV = E.OZ$Minuten_OEV-E.OZ$Anbindungszeit-E.OZ$Abgangszeit
# E.OZ[E.OZ$Fahrzeit_OEV<0,]$Fahrzeit_OEV <- 10


mean(E.Arzt.erg500[E.Arzt.erg500$OEV.ctg!='missing',]$OEV.cv)
mean(E.Arzt.erg[E.Arzt.erg$Pkw.ctg!='missing',]$Pkw.cv)
mean(E.Arzt.erg[E.Arzt.erg$Fuss.ctg!='missing',]$Fuss.cv)
mean(E.Arzt.erg500[E.Arzt.erg500$Rad.ctg!='missing',]$Rad.cv)
# 
# mean(E.Arzt[E.Arzt$Minuten_Pkw<100,]$Minuten_Pkw)
# mean(E.Arzt[E.Arzt$Minuten_Fuss<100,]$Minuten_Fuss)
# mean(E.Arzt[E.Arzt$Minuten_Rad<100,]$Minuten_Rad)
# mean(E.Arzt[E.Arzt$Minuten_OEV<100,]$Minuten_OEV)



#--Distanz Indikatoren--#
#------Arzt
E.Arzt.erg <- E.Arzt
E.Arzt.erg <- rename(E.Arzt.erg, c("Minuten_OEV"="OEV","Minuten_Pkw"="Pkw","Minuten_Fuss"="Fuss","Minuten_Rad"="Rad"))
E.Arzt.erg$Pkw <- E.Arzt.erg$Pkw-5
E.Arzt.erg[E.Arzt.erg$OEV>120,]$OEV <- 120 ##Um eine bessere Vergleichbarkeit zu erzielen.
E.Arzt.erg[E.Arzt.erg$OEV<1,]$OEV <- 1 ##Um eine bessere Vergleichbarkeit zu erzielen.
E.Arzt.erg[E.Arzt.erg$Rad<2,]$Rad <- 2 ##Um eine bessere Vergleichbarkeit zu erzielen.
E.Arzt.erg[E.Arzt.erg$Fuss<2,]$Fuss <- 2 ##Um eine bessere Vergleichbarkeit zu erzielen.
E.Arzt.erg[E.Arzt.erg$Meter_NMIV<50,]$Meter_NMIV <- 50 ##Um eine bessere Vergleichbarkeit zu erzielen.
E.Arzt.erg[E.Arzt.erg$Meter_Pkw<1,]$Meter_Pkw <- 50 ##Um eine bessere Vergleichbarkeit zu erzielen.
OEVArzt.erg <- do.call(data.frame,aggregate(OEV ~ IDVZelle+TYPENO, E.Arzt.erg,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
PkwArzt.erg <- do.call(data.frame,aggregate(Pkw ~ IDVZelle+TYPENO, E.Arzt.erg,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
Pkw_MeterArzt.erg <- do.call(data.frame,aggregate(Meter_Pkw ~ IDVZelle+TYPENO, E.Arzt.erg,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
FussArzt.erg <- do.call(data.frame,aggregate(Fuss ~ IDVZelle+TYPENO, E.Arzt.erg,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
RadArzt.erg <- do.call(data.frame,aggregate(Rad ~ IDVZelle+TYPENO, E.Arzt.erg,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
NMIV_MeterArzt.erg <- do.call(data.frame,aggregate(Meter_NMIV ~ IDVZelle+TYPENO, E.Arzt.erg,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))

E.Arzt.erg <- merge(OEVArzt.erg,PkwArzt.erg)
E.Arzt.erg <- merge(E.Arzt.erg,FussArzt.erg)
E.Arzt.erg <- merge(E.Arzt.erg,RadArzt.erg)
E.Arzt.erg <- merge(E.Arzt.erg,Pkw_MeterArzt.erg)
E.Arzt.erg <- merge(E.Arzt.erg,NMIV_MeterArzt.erg)
E.Arzt.erg <- merge(E.Arzt.erg,EWVZ)
remove(OEVArzt.erg,PkwArzt.erg,FussArzt.erg,RadArzt.erg,Pkw_MeterArzt.erg,NMIV_MeterArzt.erg)

#Berechnung Varianzkoeffizient
E.Arzt.erg$OEV.cv <- with(E.Arzt.erg,OEV.sd/OEV.mean)
E.Arzt.erg$Pkw.cv <- with(E.Arzt.erg,Pkw.sd/Pkw.mean)
E.Arzt.erg$Fuss.cv <- with(E.Arzt.erg,Fuss.sd/Fuss.mean)
E.Arzt.erg$Rad.cv <- with(E.Arzt.erg,Rad.sd/Rad.mean)
E.Arzt.erg$Meter_Pkw.cv <- with(E.Arzt.erg,Meter_Pkw.sd/Meter_Pkw.mean)
E.Arzt.erg$Meter_NMIV.cv <- with(E.Arzt.erg,Meter_NMIV.sd/Meter_NMIV.mean)

#Kategorisierung
#OEV
E.Arzt.erg$OEV.ctg <- cut(E.Arzt.erg$OEV.cv, breaks = breaks.ctg,labels = labels.ctg) ##Um nicht als 'factor' zu speichern
levels(E.Arzt.erg$OEV.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
E.Arzt.erg$OEV.ctg[E.Arzt.erg$OEV.len<5] <- 'missing'
#Pkw
E.Arzt.erg$Pkw.ctg <- cut(E.Arzt.erg$Pkw.cv, breaks = breaks.ctg,labels = labels.ctg) ##Um nicht als 'factor' zu speichern
levels(E.Arzt.erg$Pkw.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
E.Arzt.erg$Pkw.ctg[E.Arzt.erg$Pkw.len<5] <- 'missing'
#Fuss
E.Arzt.erg$Fuss.ctg <- cut(E.Arzt.erg$Fuss.cv, breaks = breaks.ctg,labels = labels.ctg) ##Um nicht als 'factor' zu speichern
levels(E.Arzt.erg$Fuss.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
E.Arzt.erg$Fuss.ctg[E.Arzt.erg$Fuss.len<5] <- 'missing'
#Rad
E.Arzt.erg$Rad.ctg <- cut(E.Arzt.erg$Rad.cv, breaks = breaks.ctg,labels = labels.ctg) ##Um nicht als 'factor' zu speichern
levels(E.Arzt.erg$Rad.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
E.Arzt.erg$Rad.ctg[E.Arzt.erg$Rad.len<5] <- 'missing'
#Meter
E.Arzt.erg$Meter_Pkw.ctg <- cut(E.Arzt.erg$Meter_Pkw.cv, breaks = breaks.ctg,labels = labels.ctg) ##Um nicht als 'factor' zu speichern
levels(E.Arzt.erg$Meter_Pkw.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
E.Arzt.erg$Meter_Pkw.ctg[E.Arzt.erg$Meter_Pkw.len<5] <- 'missing'
E.Arzt.erg$Meter_NMIV.ctg <- cut(E.Arzt.erg$Meter_NMIV.cv, breaks = breaks.ctg,labels = labels.ctg) ##Um nicht als 'factor' zu speichern
levels(E.Arzt.erg$Meter_NMIV.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
E.Arzt.erg$Meter_NMIV.ctg[E.Arzt.erg$Meter_NMIV.len<5] <- 'missing'

#------OZ
E.OZ.erg <- E.OZ
E.OZ.erg <- rename(E.OZ.erg, c("Minuten_OEV"="OEV","Minuten_Pkw"="Pkw","Minuten_Fuss"="Fuss","Minuten_Rad"="Rad"))
E.OZ.erg[E.OZ.erg$OEV>120,]$OEV <- 120 ##Um eine bessere Vergleichbarkeit zu erzielen.
E.OZ.erg[E.OZ.erg$OEV<1,]$OEV <- 1 ##Um eine bessere Vergleichbarkeit zu erzielen.
E.OZ.erg[E.OZ.erg$Rad<2,]$Rad <- 2 ##Um eine bessere Vergleichbarkeit zu erzielen.
E.OZ.erg[E.OZ.erg$Fuss<2,]$Fuss <- 2 ##Um eine bessere Vergleichbarkeit zu erzielen.
OEVOZ.erg <- do.call(data.frame,aggregate(OEV ~ IDVZelle+TYPENO, E.OZ.erg,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
PkwOZ.erg <- do.call(data.frame,aggregate(Pkw ~ IDVZelle+TYPENO, E.OZ.erg,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
RadOZ.erg <- do.call(data.frame,aggregate(Rad ~ IDVZelle+TYPENO, E.OZ.erg,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))

E.OZ.erg <- merge(OEVOZ.erg,PkwOZ.erg)
E.OZ.erg <- merge(E.OZ.erg,RadOZ.erg)
E.OZ.erg <- merge(E.OZ.erg,EWVZ)
remove(OEVOZ.erg,PkwOZ.erg,RadOZ.erg)

#Berechnung Varianzkoeffizient
E.OZ.erg$OEV.cv <- with(E.OZ.erg,OEV.sd/OEV.mean)
E.OZ.erg$Pkw.cv <- with(E.OZ.erg,Pkw.sd/Pkw.mean)
E.OZ.erg$Rad.cv <- with(E.OZ.erg,Rad.sd/Rad.mean)

#Kategorisierung
#OEV
E.OZ.erg$OEV.ctg <- cut(E.OZ.erg$OEV.cv, breaks = breaks.ctg,labels = labels.ctg) ##Um nicht als 'factor' zu speichern
levels(E.OZ.erg$OEV.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
E.OZ.erg$OEV.ctg[E.OZ.erg$OEV.len<5] <- 'missing'
#Pkw
E.OZ.erg$Pkw.ctg <- cut(E.OZ.erg$Pkw.cv, breaks = breaks.ctg,labels = labels.ctg) ##Um nicht als 'factor' zu speichern
levels(E.OZ.erg$Pkw.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
E.OZ.erg$Pkw.ctg[E.OZ.erg$Pkw.len<5] <- 'missing'
#Rad
E.OZ.erg$Rad.ctg <- cut(E.OZ.erg$Rad.cv, breaks = breaks.ctg,labels = labels.ctg) ##Um nicht als 'factor' zu speichern
levels(E.OZ.erg$Rad.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
E.OZ.erg$Rad.ctg[E.OZ.erg$Rad.len<5] <- 'missing'




#--------------500------------------#
#------Arzt
E.Arzt.erg500 <- E.Arzt
E.Arzt.erg500 <- rename(E.Arzt.erg500, c("Minuten_OEV"="OEV","Minuten_Pkw"="Pkw","Minuten_Fuss"="Fuss","Minuten_Rad"="Rad"))
E.Arzt.erg500$Pkw <- E.Arzt.erg500$Pkw-5
E.Arzt.erg500[E.Arzt.erg500$OEV>120,]$OEV <- 120 ##Um eine bessere Vergleichbarkeit zu erzielen.
E.Arzt.erg500[E.Arzt.erg500$OEV<1,]$OEV <- 1 ##Um eine bessere Vergleichbarkeit zu erzielen.
E.Arzt.erg500[E.Arzt.erg500$Rad<2,]$Rad <- 2 ##Um eine bessere Vergleichbarkeit zu erzielen.
E.Arzt.erg500[E.Arzt.erg500$Fuss<2,]$Fuss <- 2 ##Um eine bessere Vergleichbarkeit zu erzielen.
E.Arzt.erg500[E.Arzt.erg500$Pkw<2,]$Pkw <- 2 ##Um eine bessere Vergleichbarkeit zu erzielen.
E.Arzt.erg500[E.Arzt.erg500$Meter_Pkw<50,]$Meter_Pkw <- 50 ##Um eine bessere Vergleichbarkeit zu erzielen.
E.Arzt.erg500[E.Arzt.erg500$Meter_NMIV<50,]$Meter_NMIV <- 50 ##Um eine bessere Vergleichbarkeit zu erzielen.
OEVArzt.erg <- do.call(data.frame,aggregate(OEV ~ ID500, E.Arzt.erg500,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
PkwArzt.erg <- do.call(data.frame,aggregate(Pkw ~ ID500, E.Arzt.erg500,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
FussArzt.erg <- do.call(data.frame,aggregate(Fuss ~ ID500, E.Arzt.erg500,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
RadArzt.erg <- do.call(data.frame,aggregate(Rad ~ ID500, E.Arzt.erg500,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
Pkw_MeterArzt.erg <- do.call(data.frame,aggregate(Meter_Pkw ~ ID500, E.Arzt.erg500,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
NMIV_MeterArzt.erg <- do.call(data.frame,aggregate(Meter_NMIV ~ ID500, E.Arzt.erg500,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))

E.Arzt.erg500 <- merge(OEVArzt.erg,PkwArzt.erg)
E.Arzt.erg500 <- merge(E.Arzt.erg500,FussArzt.erg)
E.Arzt.erg500 <- merge(E.Arzt.erg500,RadArzt.erg)
E.Arzt.erg500 <- merge(E.Arzt.erg500,Pkw_MeterArzt.erg)
E.Arzt.erg500 <- merge(E.Arzt.erg500,NMIV_MeterArzt.erg)
E.Arzt.erg500 <- merge(E.Arzt.erg500,EW500)
remove(OEVArzt.erg,PkwArzt.erg,FussArzt.erg,RadArzt.erg,Pkw_MeterArzt.erg,NMIV_MeterArzt.erg)

#Berechnung Varianzkoeffizient
E.Arzt.erg500$OEV.cv <- with(E.Arzt.erg500,OEV.sd/OEV.mean)
E.Arzt.erg500$Pkw.cv <- with(E.Arzt.erg500,Pkw.sd/Pkw.mean)
E.Arzt.erg500$Fuss.cv <- with(E.Arzt.erg500,Fuss.sd/Fuss.mean)
E.Arzt.erg500$Rad.cv <- with(E.Arzt.erg500,Rad.sd/Rad.mean)
E.Arzt.erg500$Meter_Pkw.cv <- with(E.Arzt.erg500,Meter_Pkw.sd/Meter_Pkw.mean)
E.Arzt.erg500$Meter_NMIV.cv <- with(E.Arzt.erg500,Meter_NMIV.sd/Meter_NMIV.mean)

#Kategorisierung
#OEV
E.Arzt.erg500$OEV.ctg <- cut(E.Arzt.erg500$OEV.cv, breaks = breaks.ctg,labels = labels.ctg) ##Um nicht als 'factor' zu speichern
levels(E.Arzt.erg500$OEV.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
E.Arzt.erg500$OEV.ctg[E.Arzt.erg500$OEV.len<2] <- 'missing'
#Pkw
E.Arzt.erg500$Pkw.ctg <- cut(E.Arzt.erg500$Pkw.cv, breaks = breaks.ctg,labels = labels.ctg) ##Um nicht als 'factor' zu speichern
levels(E.Arzt.erg500$Pkw.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
E.Arzt.erg500$Pkw.ctg[E.Arzt.erg500$Pkw.len<2] <- 'missing'
#Fuss
E.Arzt.erg500$Fuss.ctg <- cut(E.Arzt.erg500$Fuss.cv, breaks = breaks.ctg,labels = labels.ctg) ##Um nicht als 'factor' zu speichern
levels(E.Arzt.erg500$Fuss.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
E.Arzt.erg500$Fuss.ctg[E.Arzt.erg500$Fuss.len<2] <- 'missing'
#Rad
E.Arzt.erg500$Rad.ctg <- cut(E.Arzt.erg500$Rad.cv, breaks = breaks.ctg,labels = labels.ctg) ##Um nicht als 'factor' zu speichern
levels(E.Arzt.erg500$Rad.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
E.Arzt.erg500$Rad.ctg[E.Arzt.erg500$Rad.len<2] <- 'missing'
#Meter
E.Arzt.erg500$Meter_Pkw.ctg <- cut(E.Arzt.erg500$Meter_Pkw.cv, breaks = breaks.ctg,labels = labels.ctg) ##Um nicht als 'factor' zu speichern
levels(E.Arzt.erg500$Meter_Pkw.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
E.Arzt.erg500$Meter_Pkw.ctg[E.Arzt.erg500$Meter_Pkw.len<2] <- 'missing'
E.Arzt.erg500$Meter_NMIV.ctg <- cut(E.Arzt.erg500$Meter_NMIV.cv, breaks = breaks.ctg,labels = labels.ctg) ##Um nicht als 'factor' zu speichern
levels(E.Arzt.erg500$Meter_NMIV.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
E.Arzt.erg500$Meter_NMIV.ctg[E.Arzt.erg500$Meter_NMIV.len<2] <- 'missing'

#------OZ
E.OZ.erg500 <- E.OZ
E.OZ.erg500 <- rename(E.OZ.erg500, c("Minuten_OEV"="OEV","Minuten_Pkw"="Pkw","Minuten_Rad"="Rad"))
E.OZ.erg500[E.OZ.erg500$OEV>120,]$OEV <- 120 ##Um eine bessere Vergleichbarkeit zu erzielen.
E.OZ.erg500[E.OZ.erg500$OEV<1,]$OEV <- 1 ##Um eine bessere Vergleichbarkeit zu erzielen.
E.OZ.erg500[E.OZ.erg500$Rad<2,]$Rad <- 2 ##Um eine bessere Vergleichbarkeit zu erzielen.
E.OZ.erg500[E.OZ.erg500$Fuss<2,]$Fuss <- 2 ##Um eine bessere Vergleichbarkeit zu erzielen.
E.OZ.erg500[E.OZ.erg500$Pkw<2,]$Pkw <- 2 ##Um eine bessere Vergleichbarkeit zu erzielen.
OEVOZ.erg <- do.call(data.frame,aggregate(OEV ~ ID500, E.OZ.erg500,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
PkwOZ.erg <- do.call(data.frame,aggregate(Pkw ~ ID500, E.OZ.erg500,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
RadOZ.erg <- do.call(data.frame,aggregate(Rad ~ ID500, E.OZ.erg500,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))

E.OZ.erg500 <- merge(OEVOZ.erg,PkwOZ.erg)
E.OZ.erg500 <- merge(E.OZ.erg500,RadOZ.erg)
E.OZ.erg500 <- merge(E.OZ.erg500,EW500)
remove(OEVOZ.erg,PkwOZ.erg,RadOZ.erg)

#Berechnung Varianzkoeffizient
E.OZ.erg500$OEV.cv <- with(E.OZ.erg500,OEV.sd/OEV.mean)
E.OZ.erg500$Pkw.cv <- with(E.OZ.erg500,Pkw.sd/Pkw.mean)
E.OZ.erg500$Rad.cv <- with(E.OZ.erg500,Rad.sd/Rad.mean)

#Kategorisierung
#OEV
E.OZ.erg500$OEV.ctg <- cut(E.OZ.erg500$OEV.cv, breaks = breaks.ctg,labels = labels.ctg) ##Um nicht als 'factor' zu speichern
levels(E.OZ.erg500$OEV.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
E.OZ.erg500$OEV.ctg[E.OZ.erg500$OEV.len<2] <- 'missing'
#Pkw
E.OZ.erg500$Pkw.ctg <- cut(E.OZ.erg500$Pkw.cv, breaks = breaks.ctg,labels = labels.ctg) ##Um nicht als 'factor' zu speichern
levels(E.OZ.erg500$Pkw.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
E.OZ.erg500$Pkw.ctg[E.OZ.erg500$Pkw.len<2] <- 'missing'
#Rad
E.OZ.erg500$Rad.ctg <- cut(E.OZ.erg500$Rad.cv, breaks = breaks.ctg,labels = labels.ctg) ##Um nicht als 'factor' zu speichern
levels(E.OZ.erg500$Rad.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
E.OZ.erg500$Rad.ctg[E.OZ.erg500$Rad.len<2] <- 'missing'

