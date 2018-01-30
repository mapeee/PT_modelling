#####################################
#Inhalt:  Macro Micro Arzt 	        #
#Datum: 	Januar 2018			          #
#Autor: 	mape			 	              #
#####################################

##--Import Packages--##
library(rgdal)


#Weitere Eingrenzungen
OEVArzt <- E.Arzt
OEVArzt[OEVArzt$Minuten_OEV==999,]$Minuten_OEV <- 150

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
OEVArzt.erg$ctg <- as.character(cut(OEVArzt.erg$OEV_Arzt.cv, 
                                    breaks = breaks.ctg, 
                                    labels = labels.ctg)) ##Um nicht als 'factor' zu speichern
OEVArzt.erg[OEVArzt.erg=="NA"] <- NA ##'NA' durch NA, dann ersetzen
OEVArzt.erg$ctg <- ifelse(is.na(OEVArzt.erg$ctg),
                          0, OEVArzt.erg$ctg) ##An dieser Stelle damit die NA nach Division durch 0 verschwinden.
OEVArzt.erg$ctg <- ifelse(OEVArzt.erg$OEV_Arzt.len<5,'missing', 
                          OEVArzt.erg$ctg) ##Missung wenn zu wenige Faelle
OEVArzt.erg$ctg <- ifelse(is.na(OEVArzt.erg$ctg),'missing', OEVArzt.erg$ctg) ##Wenn Arzt.len NA, dann wird ctg auch wieder NA


#--500--#
OEVArzt.erg500$ctg <- as.character(cut(OEVArzt.erg500$OEV_Arzt.cv, 
                                       breaks = breaks.ctg, 
                                       labels = labels.ctg)) ##Um nicht als 'factor' zu speichern
OEVArzt.erg500[OEVArzt.erg500=="NA"] <- NA ##'NA' durch NA, dann ersetzen
OEVArzt.erg500$ctg <- ifelse(is.na(OEVArzt.erg500$ctg),
                             0, OEVArzt.erg500$ctg) ##An dieser Stelle damit die NA nach Division durch 0 verschwinden.
OEVArzt.erg500$ctg <- ifelse(OEVArzt.erg500$OEV_Arzt.len<2,'missing', 
                             OEVArzt.erg500$ctg) ##Missung wenn zu wenige Faelle, hier 3
OEVArzt.erg500$ctg <- ifelse(is.na(OEVArzt.erg500$ctg),'missing', OEVArzt.erg500$ctg)
