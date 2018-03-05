#####################################
#Inhalt:  Macro Micro Arzt 	        #
#Datum: 	Januar 2018			          #
#Autor: 	mape			 	              #
#####################################

##--Import Packages--##
library(rgdal)

#--Arzt--#
#Weitere Eingrenzungen
OEVArzt <- E.Arzt
OEVArzt[OEVArzt$Minuten_OEV>120,]$Minuten_OEV <- 120 ##Um eine bessere Vergleichbarkeit zu erzielen.

#Standardabweichung und Mittelwert je Zelle
OEVArzt.erg <- do.call(data.frame,aggregate(Minuten_OEV ~ IDVZelle+TYPENO, OEVArzt, 
                                            function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
colnames(OEVArzt.erg) <- c("IDVZelle","VZTyp","OEV_Arzt.len","OEV_Arzt.mean","OEV_Arzt.sd")

OEVArzt.erg500 <- do.call(data.frame,aggregate(Minuten_OEV ~ ID500, OEVArzt, 
                                            function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
colnames(OEVArzt.erg500) <- c("ID500","OEV_Arzt.len","OEV_Arzt.mean","OEV_Arzt.sd")

#Berechnung Varianzkoeffizient
OEVArzt.erg$OEV_Arzt.cv <- with(OEVArzt.erg,OEV_Arzt.sd/OEV_Arzt.mean)
OEVArzt.erg500$OEV_Arzt.cv <- with(OEVArzt.erg500,OEV_Arzt.sd/OEV_Arzt.mean)

#Weitere Datenaufbereitung
OEVArzt.erg <- (merge(OEVArzt.erg,EWVZ,all.x = T))[-c(7)] ##Merge EW und loesche die Spalte Vzellen Typ
OEVArzt.erg500 <- merge(OEVArzt.erg500,EW500) ##Merge EW

#Kategorisierung
OEVArzt.erg$ctg <- cut(OEVArzt.erg$OEV_Arzt.cv, 
                              breaks = breaks.ctg, 
                              labels = labels.ctg) ##Um nicht als 'factor' zu speichern
levels(OEVArzt.erg$ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
OEVArzt.erg$ctg[OEVArzt.erg$OEV_Arzt.len<5] <- 'missing'



#--500--#
OEVArzt.erg500$ctg <- cut(OEVArzt.erg500$OEV_Arzt.cv, 
                                       breaks = breaks.ctg, 
                                       labels = labels.ctg)
levels(OEVArzt.erg500$ctg) = append(labels.ctg,"missing")
OEVArzt.erg500$ctg[OEVArzt.erg500$OEV_Arzt.len<2] <- 'missing'



#--Oberzentrum--#
#Weitere Eingrenzungen
OEVOZ <- E.OZ
OEVOZ[OEVOZ$Minuten_OEV>120,]$Minuten_OEV <- 120 ##Um eine bessere Vergleichbarkeit zu erzielen.

#Standardabweichung und Mittelwert je Zelle
OEVOZ.erg <- do.call(data.frame,aggregate(Minuten_OEV ~ IDVZelle+TYPENO, OEVOZ, 
                                            function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
colnames(OEVOZ.erg) <- c("IDVZelle","VZTyp","OEV_OZ.len","OEV_OZ.mean","OEV_OZ.sd")

OEVOZ.erg500 <- do.call(data.frame,aggregate(Minuten_OEV ~ ID500, OEVOZ, 
                                               function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
colnames(OEVOZ.erg500) <- c("ID500","OEV_OZ.len","OEV_OZ.mean","OEV_OZ.sd")

#Berechnung Varianzkoeffizient
OEVOZ.erg$OEV_OZ.cv <- with(OEVOZ.erg,OEV_OZ.sd/OEV_OZ.mean)
OEVOZ.erg500$OEV_OZ.cv <- with(OEVOZ.erg500,OEV_OZ.sd/OEV_OZ.mean)

#Weitere Datenaufbereitung
OEVOZ.erg <- (merge(OEVOZ.erg,EWVZ,all.x = T))[-c(7)] ##Merge EW und loesche die Spalte Vzellen Typ
OEVOZ.erg500 <- merge(OEVOZ.erg500,EW500) ##Merge EW

#Kategorisierung
OEVOZ.erg$ctg <- cut(OEVOZ.erg$OEV_OZ.cv, 
                       breaks = breaks.ctg, 
                       labels = labels.ctg) ##Um nicht als 'factor' zu speichern
levels(OEVOZ.erg$ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
OEVOZ.erg$ctg[OEVOZ.erg$OEV_OZ.len<5] <- 'missing'


#--500--#
OEVOZ.erg500$ctg <- cut(OEVOZ.erg500$OEV_OZ.cv, 
                          breaks = breaks.ctg, 
                          labels = labels.ctg)
levels(OEVOZ.erg500$ctg) = append(labels.ctg,"missing")
OEVOZ.erg500$ctg[OEVOZ.erg500$OEV_OZ.len<2] <- 'missing'




#--MIV--#
#--Arzt--#
#Weitere Eingrenzungen
MIVArzt <- E.Arzt
MIVArzt[MIVArzt$Minuten_Pkw>120,]$Minuten_Pkw <- 120 ##Um eine bessere Vergleichbarkeit zu erzielen.

#Standardabweichung und Mittelwert je Zelle
MIVArzt.erg <- do.call(data.frame,aggregate(Minuten_Pkw ~ IDVZelle+TYPENO, MIVArzt, 
                                            function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
colnames(MIVArzt.erg) <- c("IDVZelle","VZTyp","MIV_Arzt.len","MIV_Arzt.mean","MIV_Arzt.sd")

MIVArzt.erg500 <- do.call(data.frame,aggregate(Minuten_Pkw ~ ID500, MIVArzt, 
                                               function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
colnames(MIVArzt.erg500) <- c("ID500","MIV_Arzt.len","MIV_Arzt.mean","MIV_Arzt.sd")

#Berechnung Varianzkoeffizient
MIVArzt.erg$MIV_Arzt.cv <- with(MIVArzt.erg,MIV_Arzt.sd/MIV_Arzt.mean)
MIVArzt.erg500$MIV_Arzt.cv <- with(MIVArzt.erg500,MIV_Arzt.sd/MIV_Arzt.mean)

#Weitere Datenaufbereitung
MIVArzt.erg <- (merge(MIVArzt.erg,EWVZ,all.x = T))[-c(7)] ##Merge EW und loesche die Spalte Vzellen Typ
MIVArzt.erg500 <- merge(MIVArzt.erg500,EW500) ##Merge EW

#Kategorisierung
MIVArzt.erg$ctg <- cut(MIVArzt.erg$MIV_Arzt.cv, 
                       breaks = breaks.ctg, 
                       labels = labels.ctg) ##Um nicht als 'factor' zu speichern
levels(MIVArzt.erg$ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
MIVArzt.erg$ctg[MIVArzt.erg$MIV_Arzt.len<5] <- 'missing'



#--500--#
MIVArzt.erg500$ctg <- cut(MIVArzt.erg500$MIV_Arzt.cv, 
                          breaks = breaks.ctg, 
                          labels = labels.ctg)
levels(MIVArzt.erg500$ctg) = append(labels.ctg,"missing")
MIVArzt.erg500$ctg[MIVArzt.erg500$MIV_Arzt.len<2] <- 'missing'


#--OZ--#
#Weitere Eingrenzungen
MIVOZ <- E.OZ
MIVOZ[MIVOZ$Minuten_Pkw>120,]$Minuten_Pkw <- 120 ##Um eine bessere Vergleichbarkeit zu erzielen.

#Standardabweichung und Mittelwert je Zelle
MIVOZ.erg <- do.call(data.frame,aggregate(Minuten_Pkw ~ IDVZelle+TYPENO, MIVOZ, 
                                          function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
colnames(MIVOZ.erg) <- c("IDVZelle","VZTyp","MIV_OZ.len","MIV_OZ.mean","MIV_OZ.sd")

MIVOZ.erg500 <- do.call(data.frame,aggregate(Minuten_Pkw ~ ID500, MIVOZ, 
                                             function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
colnames(MIVOZ.erg500) <- c("ID500","MIV_OZ.len","MIV_OZ.mean","MIV_OZ.sd")

#Berechnung Varianzkoeffizient
MIVOZ.erg$MIV_OZ.cv <- with(MIVOZ.erg,MIV_OZ.sd/MIV_OZ.mean)
MIVOZ.erg500$MIV_OZ.cv <- with(MIVOZ.erg500,MIV_OZ.sd/MIV_OZ.mean)

#Weitere Datenaufbereitung
MIVOZ.erg <- (merge(MIVOZ.erg,EWVZ,all.x = T))[-c(7)] ##Merge EW und loesche die Spalte Vzellen Typ
MIVOZ.erg500 <- merge(MIVOZ.erg500,EW500) ##Merge EW

#Kategorisierung
MIVOZ.erg$ctg <- cut(MIVOZ.erg$MIV_OZ.cv, 
                     breaks = breaks.ctg, 
                     labels = labels.ctg) ##Um nicht als 'factor' zu speichern
levels(MIVOZ.erg$ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
MIVOZ.erg$ctg[MIVOZ.erg$MIV_OZ.len<5] <- 'missing'


#--500--#
MIVOZ.erg500$ctg <- cut(MIVOZ.erg500$MIV_OZ.cv, 
                        breaks = breaks.ctg, 
                        labels = labels.ctg)
levels(MIVOZ.erg500$ctg) = append(labels.ctg,"missing")
MIVOZ.erg500$ctg[MIVOZ.erg500$MIV_OZ.len<2] <- 'missing'
