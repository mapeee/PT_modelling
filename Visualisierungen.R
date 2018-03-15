#####################################
#Inhalt:  Visualisierung	          #
#Datum: 	Januar 2018			          #
#Autor: 	mape			 	              #
#####################################


##--Import Packages--##
library(rgdal)
library(ggplot2)


##--Visualisierungen--##
#0.0 Parameter
Farbe <- colorRampPalette(c("forestgreen","yellow2","red2"))
Farbe <- c(Farbe(length(labels.ctg)),"gray50")

#Kategorisierung
breaks.ctg = c(seq(-0.0001,2.0,0.2),Inf)
labels.ctg = c("0 - \n 0.19","0.2 - \n 0.39","0.4 - \n 0.59","0.6 - \n 0.79",
               "0.8 - \n 0.99","1.0 - \n 1.19","1.2 - \n 1.39","1.4 - \n 1.59",
               "1.6 - \n 1.79","1.8 - \n 1.99",">=2.0") ##Ein label weniger als break!!


#Druckbereich
dev.off()
par(mfrow=c(2,2)) ##zwei Spalten, zwei Zeilen
par(mar=c(3.5,2.2,3.5,4.0)) ##Keine Raender
par(mfrow=c(4,1)) ##zwei Spalten, zwei Zeilen

#Karten
#--OEV--#
Visual.karte(AP.erg,"OEV_AP30.cv","OEV_AP30.len",5,0)
Visual.karte(AP.erg,"OEV_AP30.cv","OEV_AP30.len",5,5)


#Boxplots
#--OEV--#
boxplot(na.omit(OEVAP30.erg$AP30.cv)) ##beruecksichtige keine leeren Felder
boxplot(na.omit(OEVAP30.erg$AP30.cv[OEVAP30.erg$VZTyp==4])) ##Plotte nur fur Typ 4
#--Pkw--#
boxplot(na.omit(PkwAP30.erg[PkwAP30.erg$AP30.len>10,]$AP30.cv)) ##beruecksichtige keine leeren Felder
boxplot(na.omit(PkwAP30.erg$AP30.cv[PkwAP30.erg$VZTyp==4])) ##Plotte nur fur Typ 4

#Diagramme, Kumulation
#--OEV--#
Visual.mm(AP.erg,600,"PT Jobs 30min Study area","OEV_AP30.cv",T,0,"OEV_AP30.ctg")
Visual.mm(AP.erg,400,"PT Jobs 30min Municipality","OEV_AP30.cv",T,4,"OEV_AP30.ctg")
Visual.mm(AP.erg,400,"PT Jobs 30min Hamburg (statistical areas)","OEV_AP30.cv",T,5,"OEV_AP30.ctg")
Visual.mm(AP.erg500,15000,"PT Jobs 30min R500","OEV_AP30.cv",T,0,"OEV_AP30.ctg")
Visual.mm(AP.erg,800,"PT Jobs 60min Study area","OEV_AP60.cv",T,0,"OEV_AP60.ctg")
Visual.mm(AP.erg,400,"PT Jobs 60min Municipality","OEV_AP60.cv",T,4,"OEV_AP60.ctg")
Visual.mm(AP.erg,400,"PT Jobs 60min Hamburg (statistical areas)","OEV_AP60.cv",T,5,"OEV_AP60.ctg")
Visual.mm(AP.erg500,15000,"PT Jobs 60min R500","OEV_AP60.cv",T,0,"OEV_AP60.ctg")
#--Pkw--#
Visual.mm(AP.erg,2000,"Car Jobs 30min Study area","Pkw_AP30.cv",T,0,"Pkw_AP30.ctg")
Visual.mm(AP.erg500,25000,"Car Jobs 30min R500","Pkw_AP30.cv",T,0,"Pkw_AP30.ctg")
Visual.mm(AP.erg500,25000,"Car AP60 R500","Pkw_AP60.cv",T,0,"Pkw_AP60.ctg")
Visual.mm(AP.erg,450,"Car AP30 Hamburg (statistical areas)","Pkw_AP30.cv",T,5,"Pkw_AP30.ctg")
Visual.mm(AP.erg,700,"Car AP30 Municipality","Pkw_AP30.cv",T,4,"Pkw_AP30.ctg")
#--Fuss--#
Visual.mm(AP.erg,400,"Walk Jobs 15min Study area","Fuss_AP15.cv",T,0,"Fuss_AP15.ctg")
Visual.mm(AP.erg,500,"Walk Jobs 30min Study area","Fuss_AP30.cv",T,0,"Fuss_AP30.ctg")
Visual.mm(AP.erg500,10000,"Walk Jobs 15min R500","Fuss_AP15.cv",T,0,"Fuss_AP15.ctg")
Visual.mm(AP.erg500,12000,"Walk Jobs 30min R500","Fuss_AP30.cv",T,0,"Fuss_AP30.ctg")
#--Rad--#
Visual.mm(AP.erg,400,"Bike Jobs 15min Study area","Rad_AP15.cv",T,0,"Rad_AP15.ctg")
Visual.mm(AP.erg,500,"Bike Jobs 30min Study area","Rad_AP30.cv",T,0,"Rad_AP30.ctg")
Visual.mm(AP.erg500,10000,"Bike Jobs 15min R500","Rad_AP15.cv",T,0,"Rad_AP15.ctg")
Visual.mm(AP.erg500,12000,"Bike Jobs 30min R500","Rad_AP30.cv",T,0,"Rad_AP30.ctg")

#sd500 sd100
Visual.mm(AP.sd500erg,13000,"PT Jobs 30min sd500","OEVAP30.sd500.cv",T,0,"OEVAP30.ctg")
Visual.mm(AP.sd500erg,13000,"Car Jobs 30min sd500","PkwAP30.sd500.cv",T,0,"PkwAP30.ctg")

#Aufwandsindikator
#--OEV--#
#--Arzt--#
Visual.mm(E.Arzt.erg,400,"PT next doctor Study area","OEV.cv",T,0,"OEV.ctg")
Visual.mm(E.Arzt.erg,400,"PT next doctor Municipality","OEV.cv",T,4,"OEV.ctg")
Visual.mm(E.Arzt.erg,400,"PT next doctor Hamburg (statistical areas)","OEV.cv",T,5,"OEV.ctg")
Visual.mm(E.Arzt.erg500,11000,"PT next doctor R500","OEV.cv",T,0,"OEV.ctg")
#--OZ--#
Visual.mm(OEVOZ.erg,400,"PT next regional metropolis Study area","OEV_OZ.cv",T,0)
Visual.mm(OEVOZ.erg,400,"PT next regional metropolis Municipality","OEV_OZ.cv",T,4)
Visual.mm(OEVOZ.erg,400,"PT next regional metropolis Hamburg (statistical areas)","OEV_OZ.cv",T,5)
Visual.mm(OEVOZ.erg500,11000,"PT next regional metropolis R500","OEV_OZ.cv",T,0)
#--Pkw--#
#--Arzt--#
Visual.mm(PkwArzt.erg,400,"Car next doctor Study area","Pkw_Arzt.cv",T,0)
Visual.mm(PkwArzt.erg,400,"Car next doctor Municipality","Pkw_Arzt.cv",T,4)
Visual.mm(PkwArzt.erg,400,"Car next doctor Hamburg (statistical areas)","Pkw_Arzt.cv",T,5)
Visual.mm(PkwArzt.erg500,11000,"Car next doctor R500","Pkw_Arzt.cv",T,0)
#--OZ--#
Visual.mm(PkwOZ.erg,400,"Car next regional metropolis Study area","Pkw_OZ.cv",T,0)
Visual.mm(PkwOZ.erg,400,"Car next regional metropolis Municipality","Pkw_OZ.cv",T,4)
Visual.mm(PkwOZ.erg,400,"Car next regional metropolis Hamburg (statistical areas)","Pkw_OZ.cv",T,5)
Visual.mm(PkwOZ.erg500,11000,"Car next regional metropolis R500","Pkw_OZ.cv",T,0)

#Gebiet.R500
Visual.mm(OEVAP30.500.erg,800,"PT Jobs 30min R500 vs. Study area","OEV_AP30.cv",T,0)
Visual.mm(OEVAP30.500.erg,800,"PT Jobs 30min R500 vs. Municipality","OEV_AP30.cv",T,4)
Visual.mm(OEVAP30.500.erg,500,"PT Jobs 30min R500 vs. Hamburg (statistical areas)","OEV_AP30.cv",T,5)

Visual.mm(PkwAP30.500.erg,1800,"Car Jobs 30min R500 vs. Study area","Pkw_AP30.cv",T,0)

#--Ende--#
dev.copy(png,'Abbildungen/OEV_micro_macro.png',width=1500, height=1000, pointsize=12)
dev.off()
