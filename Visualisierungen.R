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
Visual.karte(OEVAP30.erg,"OEV_AP30.cv","OEV_AP30.len",5,0)
Visual.karte(OEVAP30.erg,"OEV_AP30.cv","OEV_AP30.len",5,5)

#--MIV--#


#Boxplots
#--OEV--#
boxplot(na.omit(OEVAP30.erg$AP30.cv)) ##beruecksichtige keine leeren Felder
boxplot(na.omit(OEVAP30.erg$AP30.cv[OEVAP30.erg$VZTyp==4])) ##Plotte nur fur Typ 4
#--MIV--#
boxplot(na.omit(MIVAP30.erg[MIVAP30.erg$AP30.len>10,]$AP30.cv)) ##beruecksichtige keine leeren Felder
boxplot(na.omit(MIVAP30.erg$AP30.cv[MIVAP30.erg$VZTyp==4])) ##Plotte nur fur Typ 4

#Diagramme Zuschnitt, Kumulation
#--OEV--#
Visual.mm(OEVAP30.erg,600,"(K) PT Jobs 30min Study area","OEV_AP30.cv",T,0)
Visual.mm(OEVAP30.erg,400,"(M) PT Jobs 30min Municipality","OEV_AP30.cv",T,4)
Visual.mm(OEVAP30.erg,400,"(C) PT Jobs 30min Hamburg (statistical areas)","OEV_AP30.cv",T,5)
Visual.mm(OEVAP30.erg500,15000,"(O) PT Jobs 30min R500","OEV_AP30.cv",T,0)
Visual.mm(OEVAP60.erg,800,"(L) PT Jobs 60min Study area","OEV_AP60.cv",T,0)
Visual.mm(OEVAP60.erg,400,"(N) PT Jobs 60min Municipality","OEV_AP60.cv",T,4)
Visual.mm(OEVAP60.erg,400,"PT Jobs 60min Hamburg (statistical areas)","OEV_AP60.cv",T,5)
Visual.mm(OEVAP60.erg500,15000,"(P) PT Jobs 60min R500","OEV_AP60.cv",T,0)
#--MIV--#
Visual.mm(MIVAP30.erg,2000,"(F) Car Jobs 30min Study area","Pkw_AP30.cv",T,0)
Visual.mm(MIVAP30.erg500,25000,"(G) Car Jobs 30min R500","Pkw_AP30.cv",T,0)
Visual.mm(MIVAP60.erg500,25000,"Car AP60 R500","Pkw_AP60.cv",T,0)
Visual.mm(MIVAP30.erg,450,"Car AP30 Hamburg (statistical areas)","Pkw_AP30.cv",T,5)
Visual.mm(MIVAP30.erg,700,"Car AP30 Municipality","Pkw_AP30.cv",T,4)
#--FUSS--#
Visual.mm(FUSSAP15.erg,400,"Walk Jobs 15min Study area","Fuss_AP15.cv",T,0)
Visual.mm(FUSSAP30.erg,500,"(I) Walk Jobs 30min Study area","Fuss_AP30.cv",T,0)
Visual.mm(FUSSAP15.erg500,10000,"Walk Jobs 15min R500","Fuss_AP15.cv",T,0)
Visual.mm(FUSSAP30.erg500,12000,"(J) Walk Jobs 30min R500","Fuss_AP30.cv",T,0)

#Diff sd500 sd100
Visual.mm(OEVAP30.sd500.erg,13000,"(E) PT Jobs 30min sd500","AP30.sd500.cv",T,0)
Visual.mm(OEVAP30.sd500.erg,18000,"PT Jobs 30min (sd500-sd100) ","AP30.sd500.cv.diff",T,0)

Visual.mm(MIVAP30.sd500.erg,13000,"(H) Car Jobs 30min sd500","AP30.sd500.cv",T,0)
Visual.mm(MIVAP30.sd500.erg,18000,"Car Jobs 30min (sd500-sd100) ","AP30.sd500.cv.diff",T,0)

#Aufwandsindikator
#--OEV--#
#--Arzt--#
Visual.mm(OEVArzt.erg,400,"(Q) PT next doctor Study area","OEV_Arzt.cv",T,0)
Visual.mm(OEVArzt.erg,400,"() PT next doctor Municipality","OEV_Arzt.cv",T,4)
Visual.mm(OEVArzt.erg,400,"() PT next doctor Hamburg (statistical areas)","OEV_Arzt.cv",T,5)
Visual.mm(OEVArzt.erg500,11000,"(S) PT next doctor R500","OEV_Arzt.cv",T,0)
#--OZ--#
Visual.mm(OEVOZ.erg,400,"(R) PT next regional metropolis Study area","OEV_OZ.cv",T,0)
Visual.mm(OEVOZ.erg,400,"() PT next regional metropolis Municipality","OEV_OZ.cv",T,4)
Visual.mm(OEVOZ.erg,400,"() PT next regional metropolis Hamburg (statistical areas)","OEV_OZ.cv",T,5)
Visual.mm(OEVOZ.erg500,11000,"(T) PT next regional metropolis R500","OEV_OZ.cv",T,0)
#--MIV--#
#--Arzt--#
Visual.mm(MIVArzt.erg,400,"(Q) Car next doctor Study area","MIV_Arzt.cv",T,0)
Visual.mm(MIVArzt.erg,400,"() Car next doctor Municipality","MIV_Arzt.cv",T,4)
Visual.mm(MIVArzt.erg,400,"() Car next doctor Hamburg (statistical areas)","MIV_Arzt.cv",T,5)
Visual.mm(MIVArzt.erg500,11000,"(S) Car next doctor R500","MIV_Arzt.cv",T,0)
#--OZ--#
Visual.mm(MIVOZ.erg,400,"(R) Car next regional metropolis Study area","MIV_OZ.cv",T,0)
Visual.mm(MIVOZ.erg,400,"() Car next regional metropolis Municipality","MIV_OZ.cv",T,4)
Visual.mm(MIVOZ.erg,400,"() Car next regional metropolis Hamburg (statistical areas)","MIV_OZ.cv",T,5)
Visual.mm(MIVOZ.erg500,11000,"(T) Car next regional metropolis R500","MIV_OZ.cv",T,0)

#Gebiet.R500
Visual.mm(OEVAP30.500.erg,800,"PT Jobs 30min R500 vs. Study area","OEV_AP30.cv",T,0)
Visual.mm(OEVAP30.500.erg,800,"PT Jobs 30min R500 vs. Municipality","OEV_AP30.cv",T,4)
Visual.mm(OEVAP30.500.erg,500,"PT Jobs 30min R500 vs. Hamburg (statistical areas)","OEV_AP30.cv",T,5)

Visual.mm(MIVAP30.500.erg,1800,"Car Jobs 30min R500 vs. Study area","Pkw_AP30.cv",T,0)

#--Ende--#
dev.copy(png,'Abbildungen/OEV_micro_macro.png',width=1500, height=1000, pointsize=12)
dev.off()
