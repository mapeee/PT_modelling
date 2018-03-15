#####################################
#Inhalt:  Macro Micro Arzt 	        #
#Datum: 	Januar 2018			          #
#Autor: 	mape			 	              #
#####################################

##--Import Packages--##
library(rgdal)
library(plyr)
# E.Arzt.erg[E.Arzt.erg$Umstiege<10,]$Minuten_OEV <- (E.Arzt.zugang[E.Arzt.zugang$Umstiege<10,]$Minuten_OEV-E.Arzt.zugang[E.Arzt.zugang$Umstiege<10,]$Anbindungszeit)-E.Arzt.zugang[E.Arzt.zugang$Umstiege<10,]$Abgangszeit
# E.Arzt.erg[E.Arzt.erg$Minuten_OEV==0,]$Minuten_OEV <- 1 ##Um eine bessere Vergleichbarkeit zu erzielen.

#--Distanz Indikatoren--#
#------Arzt
E.Arzt.erg <- E.Arzt
E.Arzt.erg <- rename(E.Arzt.erg, c("Minuten_OEV"="OEV","Minuten_Pkw"="Pkw","Minuten_Fuss"="Fuss","Minuten_Rad"="Rad"))
E.Arzt.erg[E.Arzt.erg$OEV>120,]$OEV <- 120 ##Um eine bessere Vergleichbarkeit zu erzielen.
OEVArzt.erg <- do.call(data.frame,aggregate(OEV ~ IDVZelle+TYPENO, E.Arzt.erg,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
PkwArzt.erg <- do.call(data.frame,aggregate(Pkw ~ IDVZelle+TYPENO, E.Arzt.erg,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
FussArzt.erg <- do.call(data.frame,aggregate(Fuss ~ IDVZelle+TYPENO, E.Arzt.erg,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
RadArzt.erg <- do.call(data.frame,aggregate(Rad ~ IDVZelle+TYPENO, E.Arzt.erg,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))

E.Arzt.erg <- merge(OEVArzt.erg,PkwArzt.erg)
E.Arzt.erg <- merge(E.Arzt.erg,FussArzt.erg)
E.Arzt.erg <- merge(E.Arzt.erg,RadArzt.erg)
E.Arzt.erg <- merge(E.Arzt.erg,EWVZ)
remove(OEVArzt.erg,PkwArzt.erg,FussArzt.erg,RadArzt.erg)

#Berechnung Varianzkoeffizient
E.Arzt.erg$OEV.cv <- with(E.Arzt.erg,OEV.sd/OEV.mean)
E.Arzt.erg$Pkw.cv <- with(E.Arzt.erg,OEV.sd/Pkw.mean)
E.Arzt.erg$Fuss.cv <- with(E.Arzt.erg,OEV.sd/Fuss.mean)
E.Arzt.erg$Rad.cv <- with(E.Arzt.erg,OEV.sd/Rad.mean)

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


#------OZ
E.OZ.erg <- E.OZ
E.OZ.erg <- rename(E.OZ.erg, c("Minuten_OEV"="OEV","Minuten_Pkw"="Pkw","Minuten_Fuss"="Fuss","Minuten_Rad"="Rad"))
E.OZ.erg[E.OZ.erg$OEV>120,]$OEV <- 120 ##Um eine bessere Vergleichbarkeit zu erzielen.
OEVOZ.erg <- do.call(data.frame,aggregate(OEV ~ IDVZelle+TYPENO, E.OZ.erg,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
PkwOZ.erg <- do.call(data.frame,aggregate(Pkw ~ IDVZelle+TYPENO, E.OZ.erg,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
RadOZ.erg <- do.call(data.frame,aggregate(Rad ~ IDVZelle+TYPENO, E.OZ.erg,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))

E.OZ.erg <- merge(OEVOZ.erg,PkwOZ.erg)
E.OZ.erg <- merge(E.OZ.erg,RadOZ.erg)
E.OZ.erg <- merge(E.OZ.erg,EWVZ)
remove(OEVOZ.erg,PkwOZ.erg,RadOZ.erg)

#Berechnung Varianzkoeffizient
E.OZ.erg$OEV.cv <- with(E.OZ.erg,OEV.sd/OEV.mean)
E.OZ.erg$Pkw.cv <- with(E.OZ.erg,OEV.sd/Pkw.mean)
E.OZ.erg$Rad.cv <- with(E.OZ.erg,OEV.sd/Rad.mean)

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
E.Arzt.erg500[E.Arzt.erg500$OEV>120,]$OEV <- 120 ##Um eine bessere Vergleichbarkeit zu erzielen.
OEVArzt.erg <- do.call(data.frame,aggregate(OEV ~ ID500, E.Arzt.erg500,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
PkwArzt.erg <- do.call(data.frame,aggregate(Pkw ~ ID500, E.Arzt.erg500,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
FussArzt.erg <- do.call(data.frame,aggregate(Fuss ~ ID500, E.Arzt.erg500,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
RadArzt.erg <- do.call(data.frame,aggregate(Rad ~ ID500, E.Arzt.erg500,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))

E.Arzt.erg500 <- merge(OEVArzt.erg,PkwArzt.erg)
E.Arzt.erg500 <- merge(E.Arzt.erg500,FussArzt.erg)
E.Arzt.erg500 <- merge(E.Arzt.erg500,RadArzt.erg)
E.Arzt.erg500 <- merge(E.Arzt.erg500,EW500)
remove(OEVArzt.erg,PkwArzt.erg,FussArzt.erg,RadArzt.erg)

#Berechnung Varianzkoeffizient
E.Arzt.erg500$OEV.cv <- with(E.Arzt.erg500,OEV.sd/OEV.mean)
E.Arzt.erg500$Pkw.cv <- with(E.Arzt.erg500,OEV.sd/Pkw.mean)
E.Arzt.erg500$Fuss.cv <- with(E.Arzt.erg500,OEV.sd/Fuss.mean)
E.Arzt.erg500$Rad.cv <- with(E.Arzt.erg500,OEV.sd/Rad.mean)

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


#------OZ
E.OZ.erg500 <- E.OZ
E.OZ.erg500 <- rename(E.OZ.erg500, c("Minuten_OEV"="OEV","Minuten_Pkw"="Pkw","Minuten_Rad"="Rad"))
E.OZ.erg500[E.OZ.erg500$OEV>120,]$OEV <- 120 ##Um eine bessere Vergleichbarkeit zu erzielen.
OEVOZ.erg <- do.call(data.frame,aggregate(OEV ~ ID500, E.OZ.erg500,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
PkwOZ.erg <- do.call(data.frame,aggregate(Pkw ~ ID500, E.OZ.erg500,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
RadOZ.erg <- do.call(data.frame,aggregate(Rad ~ ID500, E.OZ.erg500,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))

E.OZ.erg500 <- merge(OEVOZ.erg,PkwOZ.erg)
E.OZ.erg500 <- merge(E.OZ.erg500,RadOZ.erg)
E.OZ.erg500 <- merge(E.OZ.erg500,EW500)
remove(OEVOZ.erg,PkwOZ.erg,FussOZ.erg,RadOZ.erg)

#Berechnung Varianzkoeffizient
E.OZ.erg500$OEV.cv <- with(E.OZ.erg500,OEV.sd/OEV.mean)
E.OZ.erg500$Pkw.cv <- with(E.OZ.erg500,OEV.sd/Pkw.mean)
E.OZ.erg500$Rad.cv <- with(E.OZ.erg500,OEV.sd/Rad.mean)

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












