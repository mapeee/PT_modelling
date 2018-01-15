#####################################
#Inhalt:  Auswertungen Kleinraum  	#
#Datum: 	Januar 2018			            #
#Autor: 	mape			 	            #
#####################################
##graphics.off()
##dev.off()

##Import Packages##
library(rgdal)

fgdb <- "C:/Geodaten/LGV_Dienst/Arbeitsplaetze.gdb"
fc <- readOGR(dsn=fgdb,layer="E_Arbeitsplaetze_Potenziale")

t <- as.data.frame(fc)


##Ausgaben##
#plot(fc[1:100,]) #nur die ersten 100 Zeilen
plot.ts(t[2]) #nur die zweite Spalte
