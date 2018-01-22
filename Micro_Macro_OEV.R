#####################################
#Inhalt:  Test Scripte  	          #
#Datum: 	Januar 2018			          #
#Autor: 	mape			 	              #
#####################################
##graphics.off()
##dev.off()


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
AP30$AP30EW.oev <- with(AP30,EW*OEV_AP30) ##lege neue Spalte an und packe dort das Produkt rein
AP30$AP30EW.miv <- with(AP30,EW*Pkw_AP30) ##lege neue Spalte an und packe dort das Produkt rein

#3. Standardabweichung und Mittelwert je Zelle
OEVAP30.erg <- merge(aggregate(AP30EW.oev ~ IDVZelle+TYPENO, AP30, stdabw.ggs),##Berechne sd und mean über Werte je Zelle
                    aggregate(AP30EW.oev ~ IDVZelle, AP30, mean),by="IDVZelle")
colnames(OEVAP30.erg) <- c("IDVZelle","VZTyp","AP30EW.sd","AP30EW.mean")

OEVAP30.erg500 <- merge(aggregate(AP30EW.oev ~ ID500+TYPENO, AP30, stdabw.ggs),##Berechne sd und mean über Werte je Zelle
                     aggregate(AP30EW.oev ~ ID500, AP30, mean),by="ID500")
colnames(OEVAP30.erg500) <- c("ID500","VZTyp","AP30EW.sd","AP30EW.mean")

#4. Berechnung Varianzkoeffizient
OEVAP30.erg$AP30EW.cv <- with(OEVAP30.erg,AP30EW.sd/AP30EW.mean)
OEVAP30.erg500$AP30EW.cv <- with(OEVAP30.erg500,AP30EW.sd/AP30EW.mean)

#5. Weitere Datenaufbereitung
OEVAP30.erg <- merge(OEVAP30.erg,aggregate(AP30EW.oev ~ IDVZelle, 
                                           AP30, length)) ##Fuege noch die laenge hinzu
colnames(OEVAP30.erg) <- c("IDVZelle","VZTyp","AP30EW.sd","AP30EW.mean","AP30EW.cv","AP30EW.len")
OEVAP30.erg <- merge(data.frame(VZellen$NO),OEVAP30.erg, 
                     by.x="VZellen.NO", by.y="IDVZelle",all=T) ##ohne klappt merge an Shape nicht
colnames(OEVAP30.erg) <- c("IDVZelle","VZTyp","AP30EW.sd","AP30EW.mean","AP30EW.cv","AP30EW.len")
OEVAP30.erg[OEVAP30.erg=="NA"] <- NA
OEVAP30.erg[OEVAP30.erg=="NaN"] <- 0 ##Entferne die NaN bei Division durch 0 im .cv

#--500--#
OEVAP30.erg500 <- merge(OEVAP30.erg500,aggregate(AP30EW.oev ~ ID500, 
                                                 AP30, length)) ##Fuege noch die laenge hinzu
colnames(OEVAP30.erg500) <- c("ID500","VZTyp","AP30EW.sd","AP30EW.mean","AP30EW.cv","AP30EW.len")
OEVAP30.erg500[OEVAP30.erg500=="NA"] <- NA
OEVAP30.erg500[OEVAP30.erg500=="NaN"] <- 0 ##Entferne die NaN bei Division durch 0 im .cv

#6. Kategorisierung
breaks.ctg = c(seq(0.0,2.6,0.2),Inf)
labels.ctg = c(as.character(seq(0.2,2.6,0.2)),">2.6") ##Ein label weniger als break!!

#--OEV--#
OEVAP30.erg$ctg <- as.character(cut(OEVAP30.erg$AP30EW.cv, 
                                    breaks = breaks.ctg, 
                                    labels = labels.ctg)) ##Um nicht als 'factor' zu speichern
OEVAP30.erg[OEVAP30.erg=="NA"] <- NA ##'NA' durch NA, dann ersetzen
OEVAP30.erg$ctg <- ifelse(OEVAP30.erg$AP30EW.len<10,'missing', 
                          OEVAP30.erg$ctg) ##Missung wenn zu wenige Faelle
OEVAP30.erg$ctg <- ifelse(is.na(OEVAP30.erg$ctg),'missing', OEVAP30.erg$ctg) ##replace NA Values

#--500--#
OEVAP30.erg500$ctg <- as.character(cut(OEVAP30.erg500$AP30EW.cv, 
                                    breaks = breaks.ctg, 
                                    labels = labels.ctg)) ##Um nicht als 'factor' zu speichern
OEVAP30.erg500[OEVAP30.erg500=="NA"] <- NA ##'NA' durch NA, dann ersetzen
OEVAP30.erg500$ctg <- ifelse(OEVAP30.erg500$AP30EW.len<3,'missing', 
                             OEVAP30.erg500$ctg) ##Missung wenn zu wenige Faelle, hier 3
OEVAP30.erg500$ctg <- ifelse(is.na(OEVAP30.erg500$ctg),
                             'missing', OEVAP30.erg500$ctg) ##replace NA Values