#######################################################
#Inhalt:  Visualisierung Aufwand vs. Kumulation	      #
#Datum: 	Januar 2018			                            #
#Autor: 	mape			 	                                #
#######################################################


##--Import Packages--##
library(rgdal)
library(ggplot2)

#0.1 Vorbereitung Daten
OEVArzt.erg$VZTyp <- ifelse(is.na(OEVArzt.erg$VZTyp),0, OEVArzt.erg$VZTyp) ##replace NA Values
# MIVArzt.erg$VZTyp <- ifelse(is.na(MIVArzt.erg$VZTyp),0, MIVArzt.erg$VZTyp) ##replace NA Values

#1. Daten verschneiden
TAZ.oev.Arzt.erg <- merge(VZellen, OEVArzt.erg, by.x="NO", by.y="IDVZelle") ##VZ ohne Werte bekommen Wert 0
# TAZ.miv.Arzt.erg <- merge(VZellen, MIVArzt.erg, by.x="NO", by.y="IDVZelle") ##VZ ohne Werte bekommen Wert 0

#1.  Parameter
dev.off()
par(mfrow=c(2,2)) ##zwei Spalten, zwei Zeilen
par(mar=c(2,2,2,2)) ##Keine Raender

#2. Diegramme
Visual.mm(OEVArzt.erg,ctgs.oev,800,"OEV Arzt VZ","Arzt.cv")
Visual.mm(OEVAP30.erg,ctgs.oev,800,"OEV AP30 VZ","AP30.cv")
# Visual.mm.g(TAZ.oev.Arzt.erg,ctgs.oev,450,"OEV Arzt Gemeinden","Arzt.cv",4)
Visual.mm(OEVArzt.erg500,ctgs.oev,15000,"OEV Arzt R500","Arzt.cv")
Visual.mm(OEVAP30.erg500,ctgs.oev,15000,"OEV AP30 R500","AP30.cv")
# Visual.mm.g(TAZ.oev.Arzt.erg,ctgs.oev,450,"OEV Arzt Stat. Gebiete","Arzt.cv",5)