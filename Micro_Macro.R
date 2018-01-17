#####################################
#Inhalt:  Test Scripte  	          #
#Datum: 	Januar 2018			          #
#Autor: 	mape			 	              #
#####################################
##graphics.off()
##dev.off()


##--Import Packages--##
library(rgdal)


##--Verbindung zu Geodaten--##
gdb <- "C:/Geodaten/Material.gdb" #Verbindung zur gdb
Raster100 <- as.data.frame(readOGR(dsn=gdb,layer="MRH_EW_ha")) ##Schreibe Attriubuttabelle in Data-Frame
print(colnames(Raster100)) ##Ausgabe der Spaltennamen

# Gemeinden <- readOGR(dsn=gdb,layer="MRH_Gemeinden_2017")
print(names(Gemeinden)) ##Namen der Attributspalten