#####################################
#Inhalt:  Test Scripte  	          #
#Datum: 	Januar 2018			          #
#Autor: 	mape			 	              #
#####################################
##graphics.off()
##dev.off()


##--Import Packages--##
library(rgdal)
library(xlsx)

##--Formeln--##
var.ggs <- function(x) {n=length(x) ; sum((x-mean(x))^2)/n} ##varianz aus Grundgesamtheit
stdabw.ggs <- function(x) {n=length(x) ; sqrt(var.ggs(x))} ##stdabw aus Grundgesamtheit

##--Verbindung zu Geodaten--##
gdb <- "C:/Geodaten/Material.gdb" #Verbindung zur gdb
OEVAP30 <- as.data.frame(readOGR(dsn="C:/Geodaten/LGV_Dienst/Arbeitsplaetze.gdb",
                                 layer="E_Arbeitsplaetze_Potenziale"))
OEVAP30 <- OEVAP30[c("ID","OEV_AP30")] ##Nimm nur diese beiden Spalten
Raster100 <- readOGR(dsn=gdb,layer="MRH_EW_ha")
Raster100 <- data.frame(Raster100[c("ID","EW","IDVZelle","ID500")]) ##Nimm nur diese Spalten
VZellen <- readOGR(dsn=gdb,layer="MRH_Verkehrszellen_10") ##Verbindung zum Verkehrszellenshape
VZellen <- VZellen[VZellen$MRH == 1,] ##Nimm nur die Zellen, die in der MRH sind (Untersichungsgebiet)

##--Ausgabe einzelner Werte--##
print(names(Gemeinden)) ##Namen der Attributspalten
print(head(VZellen@data))

##--Berechnung--##
#1. Merge
OEVAP30 <- merge(OEVAP30[c("ID","OEV_AP30")],Raster100[c("ID","EW","IDVZelle")],by="ID")

#2. Produkt aus EW und Indikatorwert
OEVAP30$AP30EW <- with(OEVAP30,EW*OEV_AP30) ##lege neue Spalte an und packe dort das Produkt rein

#3. Standardabweichung und Mittelwert je Zelle
OEVAP30.erg <- merge(aggregate(AP30EW ~ IDVZelle, OEVAP30, stdabw.ggs),##Berechne sd und mean über Werte je Zelle
                    aggregate(AP30EW ~ IDVZelle, OEVAP30, mean),by="IDVZelle")
colnames(OEVAP30.erg) <- c("IDVZelle","AP30EW.sd","AP30EW.mean")

#4. Berechnung Varianzkoeffizient
OEVAP30.erg$AP30EW.cv <- with(OEVAP30.erg,AP30EW.sd/AP30EW.mean)

#5. Weitere Datenaufbereitung
OEVAP30.erg <- merge(OEVAP30.erg,aggregate(AP30EW ~ IDVZelle, 
                                           OEVAP30, length)) ##Fuege noch die laenge hinzu
colnames(OEVAP30.erg) <- c("IDVZelle","AP30EW.sd","AP30EW.mean","AP30EW.cv","AP30EW.len")
OEVAP30.erg <- merge(data.frame(VZellen$NO),OEVAP30.erg, by.x="VZellen.NO", by.y="IDVZelle",all=T)
colnames(OEVAP30.erg) <- c("IDVZelle","AP30EW.sd","AP30EW.mean","AP30EW.cv","AP30EW.len")
OEVAP30.erg[OEVAP30.erg=="NA"] <- NA

#6. Kategorisierung
breaks.ctg = c(Inf,seq(0.2,2.6,0.2),Inf)
labels.ctg = c("<0.2",as.character(seq(0.2,2.6,0.2)),">2.6")
OEVAP30.erg$ctg <- as.character(cut(OEVAP30.erg$AP30EW.cv, 
                                    breaks = breaks.ctg, 
                                    labels = labels.ctg)) ##Um nicht als 'factor' zu speichern
OEVAP30.erg$ctg <- ifelse(is.na(OEVAP30.erg$ctg),'missing', OEVAP30.erg$ctg) ##replace NA Values


##--Visualisierungen--##
#0. Parameter
Farbe <- colorRampPalette(c("green", "white"))
Farbe <- Farbe(16)
ctgs <- factor(as.factor(OEVAP30.erg$ctg), 
               levels = c(labels.ctg,"missing")) ##Sortierung der Kategorien

#1. Daten verschneiden
VZellen.erg <- merge(VZellen, OEVAP30.erg, by.x="NO", by.y="IDVZelle")

#2.  Karte
plot(VZellen.erg,col=Farbe[ctgs],border=NA)
legend("bottomright", legend=levels(ctgs),fill=Farbe)

##--Ausgabe--##
# write.xlsx(OEVAP30[OEVAP30$IDVZelle==203,], "C:\\Users\\mape\\Desktop\\test.xlsx")
