#####################################
#Inhalt:  Micro vs. Macro OEV 	    #
#Datum: 	Januar 2018			          #
#Autor: 	mape			 	              #
#####################################


##--Import Packages--##
library(rgdal)

##--Formeln--##
var.ggs <- function(x) {n=length(x) ; sum((x-mean(x))^2)/n} ##varianz aus Grundgesamtheit
stdabw.ggs <- function(x) {n=length(x) ; sqrt(var.ggs(x))} ##stdabw aus Grundgesamtheit

##--Verbindung zu Geodaten--##
gdb <- "C:/Geodaten/Material.gdb" #Verbindung zur gdb
AP30 <- as.data.frame(readOGR(dsn="C:/Geodaten/LGV_Dienst/Arbeitsplaetze.gdb",
                                 layer="E_Arbeitsplaetze_Potenziale"))
AP30 <- AP30[c("ID","OEV_AP30","Pkw_AP30")] ##Nimm nur diese beiden Spalten
Raster100 <- readOGR(dsn=gdb,layer="MRH_EW_ha")
Raster100 <- data.frame(Raster100[c("ID","EW","IDVZelle","ID500")]) ##Nimm nur diese Spalten
Raster100 <- Raster100[Raster100$EW>0,] ##Nimm keine unbewohnten Zellen

VZellen <- readOGR(dsn=gdb,layer="MRH_Verkehrszellen_10") ##Verbindung zum Verkehrszellenshape
VZellen <- VZellen[VZellen$MRH == 1,] ##Nimm nur die Zellen, die in der MRH sind (Untersuchungsgebiet)

##--Ausgabe einzelner Werte--##
print(head(VZellen@data))

##--Berechnung--##
#1. Merge
AP30 <- merge(AP30[c("ID","OEV_AP30","Pkw_AP30")],Raster100[c("ID","EW","IDVZelle","ID500")],by="ID")
AP30 <- merge(AP30[c("ID","OEV_AP30","Pkw_AP30","EW","IDVZelle","ID500")],
                 data.frame(VZellen)[c("TYPENO","NO")],
                 by.x="IDVZelle",by.y="NO")

#2. Produkt aus EW und Indikatorwert
######

#3. Standardabweichung und Mittelwert je Zelle
OEVAP30.erg <- merge(aggregate(OEV_AP30 ~ IDVZelle+TYPENO, AP30, stdabw.ggs),##Berechne sd und mean über Werte je Zelle
                    aggregate(OEV_AP30 ~ IDVZelle, AP30, mean),by="IDVZelle")
colnames(OEVAP30.erg) <- c("IDVZelle","VZTyp","AP30.sd","AP30.mean")

OEVAP30.erg500 <- merge(aggregate(OEV_AP30 ~ ID500+TYPENO, AP30, stdabw.ggs),##Berechne sd und mean über Werte je Zelle
                     aggregate(OEV_AP30 ~ ID500, AP30, mean),by="ID500")
colnames(OEVAP30.erg500) <- c("ID500","VZTyp","AP30.sd","AP30.mean")

#4. Berechnung Varianzkoeffizient
OEVAP30.erg$AP30.cv <- with(OEVAP30.erg,AP30.sd/AP30.mean)
OEVAP30.erg500$AP30.cv <- with(OEVAP30.erg500,AP30.sd/AP30.mean)

#5. Weitere Datenaufbereitung
EW500 <- aggregate(EW ~ ID500+TYPENO, AP30, sum) ##Berechne Einwohner je 500-Meter-Zelle

OEVAP30.erg <- merge(OEVAP30.erg,aggregate(OEV_AP30 ~ IDVZelle, 
                                           AP30, length)) ##Fuege noch die laenge hinzu
colnames(OEVAP30.erg) <- c("IDVZelle","VZTyp","AP30.sd","AP30.mean","AP30.cv","AP30.len")
OEVAP30.erg <- merge(data.frame(VZellen$NO),OEVAP30.erg, 
                     by.x="VZellen.NO", by.y="IDVZelle",all=T) ##ohne klappt merge an Shape nicht
colnames(OEVAP30.erg) <- c("IDVZelle","VZTyp","AP30.sd","AP30.mean","AP30.cv","AP30.len")
OEVAP30.erg[OEVAP30.erg=="NA"] <- NA
OEVAP30.erg[OEVAP30.erg=="NaN"] <- 0 ##Entferne die NaN bei Division durch 0 im .cv

#--500--#
OEVAP30.erg500 <- merge(OEVAP30.erg500,aggregate(OEV_AP30 ~ ID500, 
                                                 AP30, length)) ##Fuege noch die laenge hinzu
colnames(OEVAP30.erg500) <- c("ID500","VZTyp","AP30.sd","AP30.mean","AP30.cv","AP30.len")
OEVAP30.erg500[OEVAP30.erg500=="NA"] <- NA
OEVAP30.erg500[OEVAP30.erg500=="NaN"] <- 0 ##Entferne die NaN bei Division durch 0 im .cv

#6. Kategorisierung
breaks.ctg = c(seq(0.0,1.3,0.1),Inf)
labels.ctg = c(as.character(seq(0.1,1.3,0.1)),">1.3") ##Ein label weniger als break!!

#--OEV--#
OEVAP30.erg$ctg <- as.character(cut(OEVAP30.erg$AP30.cv, 
                                    breaks = breaks.ctg, 
                                    labels = labels.ctg)) ##Um nicht als 'factor' zu speichern
OEVAP30.erg[OEVAP30.erg=="NA"] <- NA ##'NA' durch NA, dann ersetzen
OEVAP30.erg$ctg <- ifelse(is.na(OEVAP30.erg$ctg),
                             0, OEVAP30.erg$ctg) ##An dieser Stelle damit die NA nach Division durch 0 verschwinden.
OEVAP30.erg$ctg <- ifelse(OEVAP30.erg$AP30.len<5,'missing', 
                          OEVAP30.erg$ctg) ##Missung wenn zu wenige Faelle
OEVAP30.erg$ctg <- ifelse(is.na(OEVAP30.erg$ctg),'missing', OEVAP30.erg$ctg) ##Wenn AP30.len NA, dann wird ctg auch wieder NA

#--500--#
OEVAP30.erg500$ctg <- as.character(cut(OEVAP30.erg500$AP30.cv, 
                                    breaks = breaks.ctg, 
                                    labels = labels.ctg)) ##Um nicht als 'factor' zu speichern
OEVAP30.erg500[OEVAP30.erg500=="NA"] <- NA ##'NA' durch NA, dann ersetzen
OEVAP30.erg500$ctg <- ifelse(is.na(OEVAP30.erg500$ctg),
                             0, OEVAP30.erg500$ctg) ##An dieser Stelle damit die NA nach Division durch 0 verschwinden.
OEVAP30.erg500$ctg <- ifelse(OEVAP30.erg500$AP30.len<2,'missing', 
                             OEVAP30.erg500$ctg) ##Missung wenn zu wenige Faelle, hier 3
OEVAP30.erg500$ctg <- ifelse(is.na(OEVAP30.erg500$ctg),'missing', OEVAP30.erg500$ctg)