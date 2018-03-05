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

# AP.Fuss <- as.data.frame(readOGR(dsn="C:/Geodaten/LGV_Dienst/Arbeitsplaetze.gdb",
#                               layer="E_Fuss_Neu"))
# AP.Fuss <- AP.Fuss[c("ID","AP15","AP30")] ##Nimm nur diese beiden Spalten

E.Arzt <- as.data.frame(readOGR(dsn="C:/Geodaten/LGV_Dienst/Gesundheit.gdb",
                                layer="E_Hausarzt"))
E.Arzt <- E.Arzt[c("ID","Einwohner","Minuten_OEV","Minuten_Pkw","Anbindungszeit","Abgangszeit","Umstiege")] ##Nimm nur diese beiden Spalten
# E.Arzt <- E.Arzt[c("ID","Einwohner","Minuten_OEV","Minuten_Pkw","Anbindungszeit","Abgangszeit","Umstiege","StartHst","ZielHst","BH")] ##Nimm nur diese beiden Spalten
# E.Arzt[E.Arzt$Umstiege<10,]$Minuten_OEV <- (E.Arzt[E.Arzt$Umstiege<10,]$Minuten_OEV-E.Arzt[E.Arzt$Umstiege<10,]$Anbindungszeit)-E.Arzt[E.Arzt$Umstiege<10,]$Abgangszeit
# E.Arzt[E.Arzt$Minuten_OEV==0,]$Minuten_OEV <- 1 ##Um eine bessere Vergleichbarkeit zu erzielen.
E.Arzt <- E.Arzt[-c(5,6,7)]


E.OZ <- as.data.frame(readOGR(dsn="C:/Geodaten/LGV_Dienst/Raumplanung.gdb",
                                layer="E_OZ"))
E.OZ <- E.OZ[c("ID","Einwohner","Minuten_OEV","Minuten_Pkw","Anbindungszeit","Abgangszeit","Umstiege")] ##Nimm nur diese beiden Spalten
# E.OZ[E.OZ$Umstiege<10,]$Minuten_OEV <- (E.OZ[E.OZ$Umstiege<10,]$Minuten_OEV-E.OZ[E.OZ$Umstiege<10,]$Anbindungszeit)-E.OZ[E.OZ$Umstiege<10,]$Abgangszeit
# E.OZ[E.OZ$Minuten_OEV==0,]$Minuten_OEV <- 1 ##Um eine bessere Vergleichbarkeit zu erzielen.
E.OZ <- E.OZ[-c(5,6,7)]

Raster100 <- readOGR(dsn=gdb,layer="MRH_EW_ha")
Raster100 <- data.frame(Raster100)[c("ID","EW","IDVZelle","ID500")] ##Nimm nur diese Spalten
Raster100 <- Raster100[Raster100$EW>0,] ##Nimm keine unbewohnten Zellen

Raster500 <- readOGR(dsn=gdb,layer="MRH_500")
Raster500 <- data.frame(Raster500)[c("ID","IDVZelle")] ##Nimm nur diese Spalten

VZellen <- readOGR(dsn=gdb,layer="MRH_Verkehrszellen_10") ##Verbindung zum Verkehrszellenshape
VZellen <- VZellen[VZellen$MRH == 1,] ##Nimm nur die Zellen, die in der MRH sind (Untersuchungsgebiet)

##--Aufbereitungen--##
#EW R500
EW500 <- aggregate(EW ~ ID500, AP30, sum) ##Berechne Einwohner je 500-Meter-Zelle
#EW Verkehrszellen
EWVZ <- aggregate(EW ~ IDVZelle+TYPENO, AP30, sum) ##Berechne Einwohner je Verkehrszelle

#MErge von Einwohnern, VZID, VZTyp und ID500 an die Ergebnisse
AP30 <- merge(AP30[c("ID","OEV_AP30","Pkw_AP30")],Raster100[c("ID","EW","IDVZelle","ID500")],by="ID")
AP30 <- merge(AP30[c("ID","OEV_AP30","Pkw_AP30","EW","IDVZelle","ID500")],
              data.frame(VZellen)[c("TYPENO","NO")],
              by.x="IDVZelle",by.y="NO")

#EW R500
EW500 <- aggregate(EW ~ ID500, AP30, sum) ##Berechne Einwohner je 500-Meter-Zelle
#EW Verkehrszellen
EWVZ <- aggregate(EW ~ IDVZelle+TYPENO, AP30, sum) ##Berechne Einwohner je Verkehrszelle

# AP.Fuss <- merge(AP.Fuss[c("ID","AP15","AP30")],Raster100[c("ID","EW","IDVZelle","ID500")],by="ID")
# AP.Fuss <- merge(AP.Fuss[c("ID","AP15","AP30","EW","IDVZelle","ID500")],
#               data.frame(VZellen)[c("TYPENO","NO")],
#               by.x="IDVZelle",by.y="NO")
# colnames(AP.Fuss) <- c("IDVZelle","ID","Fuss_AP15","Fuss_AP30","EW","ID500","TYPENO")


E.Arzt <- merge(E.Arzt[c("ID","Einwohner","Minuten_OEV","Minuten_Pkw")],Raster100[c("ID","IDVZelle","ID500")],by="ID")
E.Arzt <- merge(E.Arzt[c("ID","Einwohner","Minuten_OEV","Minuten_Pkw","IDVZelle","ID500")],
                data.frame(VZellen)[c("TYPENO","NO")],
                by.x="IDVZelle",by.y="NO")
E.OZ <- merge(E.OZ[c("ID","Einwohner","Minuten_OEV","Minuten_Pkw")],Raster100[c("ID","IDVZelle","ID500")],by="ID")
E.OZ <- merge(E.OZ[c("ID","Einwohner","Minuten_OEV","Minuten_Pkw","IDVZelle","ID500")],
                data.frame(VZellen)[c("TYPENO","NO")],
                by.x="IDVZelle",by.y="NO")
AP30.500 <- merge(AP30.500,EW500, by.x = "ID", by.y = "ID500",all=T)
AP30.500 <- merge(AP30.500,Raster500, by = "ID")
AP30.500 <- merge(AP30.500,data.frame(VZellen)[c("TYPENO","NO")],by.x="IDVZelle",by.y="NO",all.x=T)
AP30.500$EW <- ifelse(is.na(AP30.500$EW),
                              0, AP30.500$EW)
