#####################################
#Inhalt:  Import Shapefiles   	    #
#Datum: 	Januar 2018			          #
#Autor: 	mape			 	              #
#####################################
##--Import Packages--##
library(rgdal)
library(xlsx)
# write.xlsx(E.Arzt.erg, "c:/Users/mape/Desktop/EArztVZ.xlsx") 

##--Verbindung zu Geodaten--##
gdb <- "C:/Geodaten/Material.gdb" #Verbindung zur gdb

#--Gebiete--#
VZellen <- readOGR(dsn=gdb,layer="MRH_Verkehrszellen_10") ##Verbindung zum Verkehrszellenshape
VZellen <- VZellen[VZellen$MRH == 1,] ##Nimm nur die Zellen, die in der MRH sind (Untersuchungsgebiet)
Raster100 <- readOGR(dsn=gdb,layer="MRH_EW_ha")
Raster100 <- data.frame(Raster100)[c("ID","EW","IDVZelle","ID500")] ##Nimm nur diese Spalten
Raster100 <- merge(Raster100,data.frame(VZellen)[c("TYPENO","NO")],by.x="IDVZelle",by.y="NO")[,c(2,3,1,4,5)] ##Merge und Sortierung der Spalten

EW500 <- aggregate(EW ~ ID500, Raster100, sum) ##Berechne Einwohner je 500-Meter-Zelle
EWVZ <- aggregate(EW ~ IDVZelle+TYPENO, Raster100, sum) ##Berechne Einwohner je Verkehrszelle

Raster500 <- readOGR(dsn=gdb,layer="MRH_500")
Raster500 <- data.frame(Raster500)[c("ID","IDVZelle")] ##Nimm nur diese Spalten
Raster500 <- merge(Raster500,EW500,by.x="ID",by.y="ID500",all.x=T)
Raster500$EW <- ifelse(is.na(Raster500$EW),0, Raster500$EW) ##Setze 0 ein, wenn vorher Fehlwert.
Raster500 <- merge(Raster500,data.frame(VZellen)[c("TYPENO","NO")],by.x="IDVZelle",by.y="NO")[,c(2,1,3,4)]

#--Kumulationsindikatoren--#
AP.500 <- as.data.frame(readOGR(dsn="C:/Geodaten/LGV_Dienst/Standortqualitaet.gdb",layer="E_Arbeitsplaetze_Potenziale_500"))
AP.500 <- AP.500[c("ID","OEV_AP30","Pkw_AP30","OEV_AP60","Pkw_AP60","Fuss_AP15","Rad_AP15",
               "Fuss_AP30","Rad_AP30","OEV_AP05","Pkw_AP05","Fuss_AP05","Rad_AP05")]
AP.500 <- merge(AP.500,Raster500,by="ID")

AP <- as.data.frame(readOGR(dsn="C:/Geodaten/LGV_Dienst/Arbeitsplaetze.gdb",layer="E_Arbeitsplaetze_Potenziale"))
# AP <- as.data.frame(readOGR(dsn="C:/Geodaten/LGV_Dienst/Freizeit.gdb",layer="E_Einwohner_Potenziale"))
# colnames(AP) = gsub("EW", "AP", colnames(AP)) ##Umbenennen der Spaltennamen. Replace mit AP
AP <- AP[c("ID","OEV_AP30","Pkw_AP30","OEV_AP60","Pkw_AP60","Fuss_AP15","Rad_AP15",
               "Fuss_AP30","Rad_AP30","OEV_AP05","Pkw_AP05","Fuss_AP05","Rad_AP05")]
AP <- merge(AP,Raster100,by="ID")

#--Distanzindikatoren--#
E.Arzt <- as.data.frame(readOGR(dsn="C:/Geodaten/LGV_Dienst/Gesundheit.gdb",layer="E_Hausarzt"))
E.Arzt <- E.Arzt[c("ID","Einwohner","Minuten_OEV","Minuten_Pkw","Minuten_Fuss","Minuten_Rad","Meter_Pkw","Meter_NMIV",
                   "Anbindungszeit","Abgangszeit","Umstiege","Verbindungen","StartHst","Luftlinie")]
E.Arzt <- merge(E.Arzt,Raster100,by="ID")

E.OZ <- as.data.frame(readOGR(dsn="C:/Geodaten/LGV_Dienst/Raumplanung.gdb",layer="E_OZ"))
E.OZ <- E.OZ[c("ID","Einwohner","Minuten_OEV","Minuten_Pkw","Minuten_Rad",
              "Anbindungszeit","Abgangszeit","Umstiege","Verbindungen")]
E.OZ <- merge(E.OZ,Raster100,by="ID")

#--Removes--#
remove(gdb)
remove(Raster100,Raster500)
remove(EWVZ,EW500)
