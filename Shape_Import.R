#####################################
#Inhalt:  Import Shapefiles   	    #
#Datum: 	Januar 2018			          #
#Autor: 	mape			 	              #
#####################################


##--Import Packages--##
library(rgdal)

##--Verbindung zu Geodaten--##
gdb <- "C:/Geodaten/Material.gdb" #Verbindung zur gdb
AP30.500 <- as.data.frame(readOGR(dsn="C:/Geodaten/LGV_Dienst/Standortqualitaet.gdb",
                                  layer="E_Arbeitsplaetze_Potenziale_500"))
AP30.500 <- AP30.500[c("ID","OEV_AP30","Pkw_AP30")] ##Nimm nur diese drei Spalten
AP30 <- as.data.frame(readOGR(dsn="C:/Geodaten/LGV_Dienst/Arbeitsplaetze.gdb",
                              layer="E_Arbeitsplaetze_Potenziale"))
AP30 <- AP30[c("ID","OEV_AP30","Pkw_AP30")] ##Nimm nur diese beiden Spalten

E.Arzt <- as.data.frame(readOGR(dsn="C:/Geodaten/LGV_Dienst/Gesundheit.gdb",
                                layer="E_Hausarzt"))
E.Arzt <- E.Arzt[c("ID","Einwohner","Minuten_OEV","Minuten_Pkw")] ##Nimm nur diese beiden Spalten

Raster100 <- readOGR(dsn=gdb,layer="MRH_EW_ha")
Raster100 <- data.frame(Raster100[c("ID","EW","IDVZelle","ID500")]) ##Nimm nur diese Spalten
Raster100 <- Raster100[Raster100$EW>0,] ##Nimm keine unbewohnten Zellen

VZellen <- readOGR(dsn=gdb,layer="MRH_Verkehrszellen_10") ##Verbindung zum Verkehrszellenshape
VZellen <- VZellen[VZellen$MRH == 1,] ##Nimm nur die Zellen, die in der MRH sind (Untersuchungsgebiet)

##--Aufbereitungen--##
#MErge von Einwohnern, VZID, VZTyp und ID500 an die Ergebnisse
AP30 <- merge(AP30[c("ID","OEV_AP30","Pkw_AP30")],Raster100[c("ID","EW","IDVZelle","ID500")],by="ID")
AP30 <- merge(AP30[c("ID","OEV_AP30","Pkw_AP30","EW","IDVZelle","ID500")],
              data.frame(VZellen)[c("TYPENO","NO")],
              by.x="IDVZelle",by.y="NO")

E.Arzt <- merge(E.Arzt[c("ID","Einwohner","Minuten_OEV","Minuten_Pkw")],Raster100[c("ID","IDVZelle","ID500")],by="ID")
E.Arzt <- merge(E.Arzt[c("ID","Einwohner","Minuten_OEV","Minuten_Pkw","IDVZelle","ID500")],
                data.frame(VZellen)[c("TYPENO","NO")],
                by.x="IDVZelle",by.y="NO")
#EW R500
EW500 <- aggregate(EW ~ ID500, AP30, sum) ##Berechne Einwohner je 500-Meter-Zelle
#EW Verkehrszellen
EWVZ <- aggregate(EW ~ IDVZelle+TYPENO, AP30, sum) ##Berechne Einwohner je Verkehrszelle
