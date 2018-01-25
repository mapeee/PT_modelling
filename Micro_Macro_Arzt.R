#####################################
#Inhalt:  Macro Micro Arzt 	        #
#Datum: 	Januar 2018			          #
#Autor: 	mape			 	              #
#####################################

##--Import Packages--##
library(rgdal)

##--Formeln--##
var.ggs <- function(x) {n=length(x) ; sum((x-mean(x))^2)/n} ##varianz aus Grundgesamtheit
stdabw.ggs <- function(x) {n=length(x) ; sqrt(var.ggs(x))} ##stdabw aus Grundgesamtheit

##--Verbindung zu Geodaten--##
E.Arzt <- as.data.frame(readOGR(dsn="C:/Geodaten/LGV_Dienst/Gesundheit.gdb",
                                 layer="E_Hausarzt"))
E.Arzt <- E.Arzt[c("ID","Einwohner","Minuten_OEV","Minuten_Pkw")] ##Nimm nur diese beiden Spalten

##--Berechnung--##
#1. Merge
E.Arzt <- merge(E.Arzt[c("ID","Einwohner","Minuten_OEV","Minuten_Pkw")],Raster100[c("ID","IDVZelle","ID500")],by="ID")
E.Arzt <- merge(E.Arzt[c("ID","Einwohner","Minuten_OEV","Minuten_Pkw","IDVZelle","ID500")],
              data.frame(VZellen)[c("TYPENO","NO")],
              by.x="IDVZelle",by.y="NO")

#2. Weitere Eingrenzungen
# OEVArzt <- E.Arzt[E.Arzt$Minuten_OEV<=90,] #um die 999 Values zu entfernen
OEVArzt <- E.Arzt
OEVArzt[OEVArzt$Minuten_OEV==999,]$Minuten_OEV <- 150

#3. Standardabweichung und Mittelwert je Zelle
OEVArzt.erg <- merge(aggregate(Minuten_OEV ~ IDVZelle+TYPENO, OEVArzt, stdabw.ggs),##Berechne sd und mean über Werte je Zelle
                     aggregate(Minuten_OEV ~ IDVZelle, OEVArzt, mean),by="IDVZelle")
colnames(OEVArzt.erg) <- c("IDVZelle","VZTyp","Arzt.sd","Arzt.mean")

OEVArzt.erg500 <- merge(aggregate(Minuten_OEV ~ ID500+TYPENO, OEVArzt, stdabw.ggs),##Berechne sd und mean über Werte je Zelle
                        aggregate(Minuten_OEV ~ ID500, OEVArzt, mean),by="ID500")
colnames(OEVArzt.erg500) <- c("ID500","VZTyp","Arzt.sd","Arzt.mean")

#4. Berechnung Varianzkoeffizient
OEVArzt.erg$Arzt.cv <- with(OEVArzt.erg,Arzt.sd/Arzt.mean)
OEVArzt.erg500$Arzt.cv <- with(OEVArzt.erg500,Arzt.sd/Arzt.mean)

#5. Weitere Datenaufbereitung
OEVArzt.erg <- merge(OEVArzt.erg,aggregate(Minuten_OEV ~ IDVZelle, 
                                           OEVArzt, length)) ##Fuege noch die laenge hinzu
colnames(OEVArzt.erg) <- c("IDVZelle","VZTyp","Arzt.sd","Arzt.mean","Arzt.cv","Arzt.len")
OEVArzt.erg <- merge(data.frame(VZellen$NO),OEVArzt.erg, 
                     by.x="VZellen.NO", by.y="IDVZelle",all=T) ##ohne klappt merge an Shape nicht
colnames(OEVArzt.erg) <- c("IDVZelle","VZTyp","Arzt.sd","Arzt.mean","Arzt.cv","Arzt.len")
OEVArzt.erg[OEVArzt.erg=="NA"] <- NA
OEVArzt.erg[OEVArzt.erg=="NaN"] <- 0 ##Entferne die NaN bei Division durch 0 im .cv

#--500--#
OEVArzt.erg500 <- merge(OEVArzt.erg500,aggregate(Minuten_OEV ~ ID500, 
                                                 OEVArzt, length)) ##Fuege noch die laenge hinzu
colnames(OEVArzt.erg500) <- c("ID500","VZTyp","Arzt.sd","Arzt.mean","Arzt.cv","Arzt.len")
OEVArzt.erg500[OEVArzt.erg500=="NA"] <- NA
OEVArzt.erg500[OEVArzt.erg500=="NaN"] <- 0 ##Entferne die NaN bei Division durch 0 im .cv

#6. Kategorisierung
breaks.ctg = c(seq(0.0,1.3,0.1),Inf)
labels.ctg = c(as.character(seq(0.1,1.3,0.1)),">1.3") ##Ein label weniger als break!!

#--OEV--#
OEVArzt.erg$ctg <- as.character(cut(OEVArzt.erg$Arzt.cv, 
                                    breaks = breaks.ctg, 
                                    labels = labels.ctg)) ##Um nicht als 'factor' zu speichern
OEVArzt.erg[OEVArzt.erg=="NA"] <- NA ##'NA' durch NA, dann ersetzen
OEVArzt.erg$ctg <- ifelse(is.na(OEVArzt.erg$ctg),
                          0, OEVArzt.erg$ctg) ##An dieser Stelle damit die NA nach Division durch 0 verschwinden.
OEVArzt.erg$ctg <- ifelse(OEVArzt.erg$Arzt.len<5,'missing', 
                          OEVArzt.erg$ctg) ##Missung wenn zu wenige Faelle
OEVArzt.erg$ctg <- ifelse(is.na(OEVArzt.erg$ctg),'missing', OEVArzt.erg$ctg) ##Wenn Arzt.len NA, dann wird ctg auch wieder NA

#--500--#
OEVArzt.erg500$ctg <- as.character(cut(OEVArzt.erg500$Arzt.cv, 
                                       breaks = breaks.ctg, 
                                       labels = labels.ctg)) ##Um nicht als 'factor' zu speichern
OEVArzt.erg500[OEVArzt.erg500=="NA"] <- NA ##'NA' durch NA, dann ersetzen
OEVArzt.erg500$ctg <- ifelse(is.na(OEVArzt.erg500$ctg),
                             0, OEVArzt.erg500$ctg) ##An dieser Stelle damit die NA nach Division durch 0 verschwinden.
OEVArzt.erg500$ctg <- ifelse(OEVArzt.erg500$Arzt.len<2,'missing', 
                             OEVArzt.erg500$ctg) ##Missung wenn zu wenige Faelle, hier 3
OEVArzt.erg500$ctg <- ifelse(is.na(OEVArzt.erg500$ctg),'missing', OEVArzt.erg500$ctg)
