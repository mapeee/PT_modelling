#####################################
#Inhalt:  Mean R100 -- R500   	    #
#Datum: 	Januar 2018			          #
#Autor: 	mape			 	              #
#####################################
#Ziel: Wie sehr unterscheiden sich der Mittelwert aller R100 in einer Zelle R500 vom Wert dieser Zelle.
#Vorgehen: Die Standardabweichung von R100 in einer Zelle R500 wird nicht ueber den Mittelwert aller R100
#bestimmt, sondern ueber den Wert von R500.

##--Import Packages--##
library(rgdal)

##--Formeln--##
stdabw.ggs.sd500 <- function(x) {n=length(x) ; sqrt(sum(x)/n)} ##stdabw aus Grundgesamtheit

##--Berechnung--##
#1. Merge der Werte von R500 an die Werte von R100
AP30.sd500 <- merge(AP30,AP30.500,by.x="ID500",by.y="ID")
AP30.sd500$IDVZelle.y <- NULL
AP30.sd500$TYPENO.y <- NULL
colnames(AP30.sd500) <- c("ID500","IDVZelle","ID100","OEV_AP30.100",
                                "Pkw_AP30.100","EW.100","VZTyp","OEV_AP30.500","Pkw_AP30.500","EW.500")

#2. relative Differenz bilden
#Bilden der quadrierten Differenz beider Werte. Qaudrat um neg. Werte zu vermeiden.
#Das ist quasi der erste Schritt zur Varianz (die quadratische Abweichung vom Mittelwert aus R500)
AP30.sd500$OEVAP30.diff <- with(AP30.sd500,(OEV_AP30.100-OEV_AP30.500)^2)
AP30.sd500$MIVAP30.diff <- with(AP30.sd500,(Pkw_AP30.100-Pkw_AP30.500)^2)


#--OEPNV--#
#3. Standardabweichung und Mittelwert R100 je Zelle R500
#3.1. Std und mean fuer R100
OEVAP30.sd500.erg <- merge(aggregate(OEV_AP30.100 ~ ID500+OEV_AP30.500, AP30.sd500, stdabw.ggs),##Berechne sd und mean über Werte je Zelle
                          aggregate(OEV_AP30.100 ~ ID500, AP30.sd500, mean),by="ID500") ##Mittelwert OEVAP30 in R100
colnames(OEVAP30.sd500.erg) <- c("ID500","AP30.500","AP30.sd100","AP30.mean100")

#3.2 Berechnung der stdabw ueber den Wert von R500 als Mittelwert
#Stdabw ist nicht Abweichung vom Mittelwert in R100 sondern Abweichung von R500 als angenommenen Mittelwert!
OEVAP30.sd500.erg <- merge(OEVAP30.sd500.erg,##Berechne sd und mean über Werte je Zelle
                     aggregate(OEVAP30.diff ~ ID500, AP30.sd500, stdabw.ggs.sd500),by="ID500") ##Mittelwert OEVAP30 in R100
colnames(OEVAP30.sd500.erg) <- c("ID500","AP30.500","AP30.sd100","AP30.mean100","AP30.sd500")


#4. Berechnung Varianzkoeffizient
OEVAP30.sd500.erg$AP30.sd100.cv <- with(OEVAP30.sd500.erg,AP30.sd100/AP30.mean100) ##Normal, wie gehabt
OEVAP30.sd500.erg$AP30.sd500.cv <- with(OEVAP30.sd500.erg,AP30.sd500/AP30.mean100) ##cv ueber diff.sd
OEVAP30.sd500.erg$AP30.sd500.cv.diff <- with(OEVAP30.sd500.erg,sqrt((AP30.sd100.cv-AP30.sd500.cv)^2)) ##diff der Abweichungen, Vermeidung negativer Werte

#5. Weitere Datenaufbereitung
OEVAP30.sd500.erg <- merge(OEVAP30.sd500.erg,aggregate(OEV_AP30 ~ ID500, 
                                           AP30, length)) ##Fuege noch die laenge hinzu, wie viele R100 liegen in einer R500
colnames(OEVAP30.sd500.erg) <- c("ID500","AP30.500","AP30.sd100","AP30.mean100","AP30.sd500",
                                "AP30.sd100.cv","AP30.sd500.cv","AP30.sd500.cv.diff","R100.500.len")
OEVAP30.sd500.erg <- merge(OEVAP30.sd500.erg,EW500) ##Fuege noch die EW hinzu
OEVAP30.sd500.erg[OEVAP30.sd500.erg=="NA"] <- NA
OEVAP30.sd500.erg[OEVAP30.sd500.erg=="NaN"] <- 0 ##Entferne die NaN bei Division durch 0 im .cv

#inf bei Division durch 0. Wenn sd nach R500 >0 und mean aus R100 =0.
OEVAP30.sd500.erg$AP30.sd500.cv <- ifelse(OEVAP30.sd500.erg$AP30.sd500.cv==Inf,2, 
                                          OEVAP30.sd500.erg$AP30.sd500.cv) ##An dieser Stelle nur Krücke zur Visualisierung
#6. Kategorisierung
#Kategorisierung von sd500 oder der Differenz
OEVAP30.sd500.erg$ctg <- cut(OEVAP30.sd500.erg$AP30.sd500.cv, ## sd500.cv / sd500.cv.diff
                                       breaks = breaks.ctg, 
                                       labels = labels.ctg) ##Um nicht als 'factor' zu speichern
levels(OEVAP30.sd500.erg$ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
OEVAP30.sd500.erg$ctg[OEVAP30.sd500.erg$R100.500.len<2] <- 'missing'
# OEVAP30.sd500.erg$ctg[OEVAP30.sd500.erg$AP30.500+OEVAP30.sd500.erg$AP30.mean100==0] <- 'missing'

#--MIV--#
#3. Standardabweichung und Mittelwert R100 je Zelle R500
#3.1. Std und mean fuer R100
MIVAP30.sd500.erg <- merge(aggregate(Pkw_AP30.100 ~ ID500+Pkw_AP30.500, AP30.sd500, stdabw.ggs),##Berechne sd und mean über Werte je Zelle
                           aggregate(Pkw_AP30.100 ~ ID500, AP30.sd500, mean),by="ID500") ##Mittelwert MIVAP30 in R100
colnames(MIVAP30.sd500.erg) <- c("ID500","AP30.500","AP30.sd100","AP30.mean100")

#3.2 Berechnung der stdabw ueber den Wert von R500 als Mittelwert
#Stdabw ist nicht Abweichung vom Mittelwert in R100 sondern Abweichung von R500 als angenommenen Mittelwert!
MIVAP30.sd500.erg <- merge(MIVAP30.sd500.erg,##Berechne sd und mean über Werte je Zelle
                           aggregate(MIVAP30.diff ~ ID500, AP30.sd500, stdabw.ggs.sd500),by="ID500") ##Mittelwert MIVAP30 in R100
colnames(MIVAP30.sd500.erg) <- c("ID500","AP30.500","AP30.sd100","AP30.mean100","AP30.sd500")


#4. Berechnung Varianzkoeffizient
MIVAP30.sd500.erg$AP30.sd100.cv <- with(MIVAP30.sd500.erg,AP30.sd100/AP30.mean100) ##Normal, wie gehabt
MIVAP30.sd500.erg$AP30.sd500.cv <- with(MIVAP30.sd500.erg,AP30.sd500/AP30.mean100) ##cv ueber diff.sd
MIVAP30.sd500.erg$AP30.sd500.cv.diff <- with(MIVAP30.sd500.erg,sqrt((AP30.sd100.cv-AP30.sd500.cv)^2)) ##diff der Abweichungen, Vermeidung negativer Werte

#5. Weitere Datenaufbereitung
MIVAP30.sd500.erg <- merge(MIVAP30.sd500.erg,aggregate(Pkw_AP30 ~ ID500, 
                                                       AP30, length)) ##Fuege noch die laenge hinzu, wie viele R100 liegen in einer R500
colnames(MIVAP30.sd500.erg) <- c("ID500","AP30.500","AP30.sd100","AP30.mean100","AP30.sd500",
                                 "AP30.sd100.cv","AP30.sd500.cv","AP30.sd500.cv.diff","R100.500.len")
MIVAP30.sd500.erg <- merge(MIVAP30.sd500.erg,EW500) ##Fuege noch die EW hinzu
MIVAP30.sd500.erg[MIVAP30.sd500.erg=="NA"] <- NA
MIVAP30.sd500.erg[MIVAP30.sd500.erg=="NaN"] <- 0 ##Entferne die NaN bei Division durch 0 im .cv

#inf bei Division durch 0. Wenn sd nach R500 >0 und mean aus R100 =0.
MIVAP30.sd500.erg$AP30.sd500.cv <- ifelse(MIVAP30.sd500.erg$AP30.sd500.cv==Inf,2, 
                                          MIVAP30.sd500.erg$AP30.sd500.cv) ##An dieser Stelle nur Krücke zur Visualisierung
#6. Kategorisierung
#Kategorisierung von sd500 oder der Differenz
MIVAP30.sd500.erg$ctg <- cut(MIVAP30.sd500.erg$AP30.sd500.cv, ## sd500.cv / sd500.cv.diff
                             breaks = breaks.ctg, 
                             labels = labels.ctg) ##Um nicht als 'factor' zu speichern
levels(MIVAP30.sd500.erg$ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
MIVAP30.sd500.erg$ctg[MIVAP30.sd500.erg$R100.500.len<2] <- 'missing'
