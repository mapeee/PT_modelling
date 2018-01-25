#####################################
#Inhalt:  Mean R100 -- R500   	    #
#Datum: 	Januar 2018			          #
#Autor: 	mape			 	              #
#####################################


##--Import Packages--##
library(rgdal)

##--Verbindung zu Geodaten--##
AP30.500 <- as.data.frame(readOGR(dsn="C:/Geodaten/LGV_Dienst/Standortqualitaet.gdb",
                              layer="E_Arbeitsplaetze_Potenziale_500"))
AP30.500 <- AP30.500[c("ID","OEV_AP30","Pkw_AP30")] ##Nimm nur diese beiden Spalten

##--Formeln--##
stdabw.ggs.diff <- function(x) {n=length(x) ; sqrt(sum(x)/n)} ##stdabw aus Grundgesamtheit


##--Berechnung--##
#1. Merge
OEVAP30.diff <- merge(AP30,AP30.500,by.x="ID500",by.y="ID")
colnames(OEVAP30.diff) <- c("ID500","IDVZelle","ID","OEV_AP30",
                                "Pkw_AP30","EW","VZTyp","OEV_AP30.500","Pkw_AP30.500")

#2. relative Differenz bilden
OEVAP30.diff$AP30.diff <- with(OEVAP30.diff,(OEV_AP30-OEV_AP30.500)^2)

#3. Standardabweichung und Mittelwert je Zelle
OEVAP30.diff.erg <- merge(aggregate(OEV_AP30 ~ ID500+OEV_AP30.500, OEVAP30.diff, stdabw.ggs),##Berechne sd und mean über Werte je Zelle
                          aggregate(OEV_AP30 ~ ID500, OEVAP30.diff, mean),by="ID500") ##Mittelwert OEVAP30 in R100
colnames(OEVAP30.diff.erg) <- c("ID500","AP30.500","AP30.sd","AP30.mean")

OEVAP30.diff.erg <- merge(OEVAP30.diff.erg,##Berechne sd und mean über Werte je Zelle
                     aggregate(AP30.diff ~ ID500, OEVAP30.diff, stdabw.ggs.diff),by="ID500") ##Mittelwert OEVAP30 in R100
colnames(OEVAP30.diff.erg) <- c("ID500","AP30.500","AP30.sd","AP30.mean","AP30.diff.sd")


#4. Berechnung Varianzkoeffizient
OEVAP30.diff.erg$AP30.cv <- with(OEVAP30.diff.erg,AP30.sd/AP30.mean)
OEVAP30.diff.erg$AP30.diff.cv <- with(OEVAP30.diff.erg,AP30.diff.sd/AP30.mean)
OEVAP30.diff.erg$AP30.diff.cv.diff <- with(OEVAP30.diff.erg,sqrt((AP30.cv-AP30.diff.cv)^2))



#5. Weitere Datenaufbereitung
OEVAP30.diff.erg <- merge(OEVAP30.diff.erg,aggregate(OEV_AP30 ~ ID500, 
                                           AP30, length)) ##Fuege noch die laenge hinzu
colnames(OEVAP30.diff.erg) <- c("ID500","AP30.500","AP30.sd","AP30.mean","AP30.diff.sd",
                                "AP30.cv","AP30.diff.cv","AP30.diff.cv.diff","AP30.len")
OEVAP30.diff.erg[OEVAP30.diff.erg=="NA"] <- NA
OEVAP30.diff.erg[OEVAP30.diff.erg=="NaN"] <- 0 ##Entferne die NaN bei Division durch 0 im .cv

#6. Kategorisierung
OEVAP30.diff.erg$ctg <- as.character(cut(OEVAP30.diff.erg$AP30.diff.cv, ##oder diff.cv.diff
                                       breaks = breaks.ctg, 
                                       labels = labels.ctg)) ##Um nicht als 'factor' zu speichern
OEVAP30.diff.erg$ctg <- ifelse(is.na(OEVAP30.diff.erg$ctg),
                             0, OEVAP30.diff.erg$ctg) ##An dieser Stelle damit die NA nach Division durch 0 verschwinden.
OEVAP30.diff.erg$ctg <- ifelse(OEVAP30.diff.erg$AP30.len<2,'missing', 
                               OEVAP30.diff.erg$ctg) ##Missung wenn zu wenige Faelle, hier 3

OEVAP30.diff.erg$AP30.diff.cv <- ifelse(OEVAP30.diff.erg$AP30.diff.cv==Inf,2, 
                                        OEVAP30.diff.erg$AP30.diff.cv) ##An dieser Stelle nur Krücke zur Visualisierung
