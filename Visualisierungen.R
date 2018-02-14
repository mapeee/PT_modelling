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
Farbe <- colorRampPalette(c("chartreuse","dodgerblue", "white"))
Farbe <- c(Farbe(length(labels.ctg)),"grey")
ctgs.oev <- factor(as.factor(OEVAP30.erg$ctg), 
                   levels = c("0",labels.ctg)) ##Sortierung der Kategorien
ctgs.miv <- factor(as.factor(MIVAP30.erg$ctg), 
                   levels = c("0",labels.ctg)) ##Sortierung der Kategorien

#Kategorisierung
breaks.ctg = c(seq(0.0,1.3,0.1),Inf)
labels.ctg = c(as.character(seq(0.1,1.3,0.1)),">1.3") ##Ein label weniger als break!!


#Vorbereitung Daten
OEVAP30.erg$VZTyp <- ifelse(is.na(OEVAP30.erg$VZTyp),0, OEVAP30.erg$VZTyp) ##replace NA Values
MIVAP30.erg$VZTyp <- ifelse(is.na(MIVAP30.erg$VZTyp),0, MIVAP30.erg$VZTyp) ##replace NA Values
OEVAP60.erg$VZTyp <- ifelse(is.na(OEVAP60.erg$VZTyp),0, OEVAP60.erg$VZTyp) ##replace NA Values
MIVAP60.erg$VZTyp <- ifelse(is.na(MIVAP60.erg$VZTyp),0, MIVAP60.erg$VZTyp) ##replace NA Values
OEVArzt.erg$VZTyp <- ifelse(is.na(OEVArzt.erg$VZTyp),0, OEVArzt.erg$VZTyp) ##replace NA Values

#Daten verschneiden
TAZ.oev.AP30.erg <- merge(VZellen, OEVAP30.erg, by.x="NO", by.y="IDVZelle") ##VZ ohne Werte bekommen Wert 0
TAZ.miv.AP30.erg <- merge(VZellen, MIVAP30.erg, by.x="NO", by.y="IDVZelle") ##VZ ohne Werte bekommen Wert 0
TAZ.oev.AP60.erg <- merge(VZellen, OEVAP60.erg, by.x="NO", by.y="IDVZelle") ##VZ ohne Werte bekommen Wert 0
TAZ.miv.AP60.erg <- merge(VZellen, MIVAP60.erg, by.x="NO", by.y="IDVZelle") ##VZ ohne Werte bekommen Wert 0
TAZ.oev.Arzt.erg <- merge(VZellen, OEVArzt.erg, by.x="NO", by.y="IDVZelle") ##VZ ohne Werte bekommen Wert 0

#Druckbereich
dev.off()
par(mfrow=c(2,2)) ##zwei Spalten, zwei Zeilen
par(mar=c(3.5,2.2,3.5,3.5)) ##Keine Raender

#Karten
#--OEV--#
plot(TAZ.oev.AP30.erg,col=Farbe[ctgs.oev],border=NA)
legend("bottomright", legend=levels(ctgs.oev),fill=Farbe)
plot(TAZ.oev.AP30.erg[TAZ.oev.AP30.erg$TYPENO==5,],col=Farbe[ctgs.oev],border=NA) ##nur Hamburg

#--MIV--#
plot(TAZ.miv.AP30.erg,col=Farbe[ctgs.miv],border=NA)
legend("bottomright", legend=levels(ctgs.miv),fill=Farbe)
plot(TAZ.miv.AP30.erg[TAZ.miv.AP30.erg$TYPENO==5,],col=Farbe[ctgs.miv],border=NA) ##nur Hamburg

#Boxplots
#--OEV--#
boxplot(na.omit(OEVAP30.erg$AP30.cv)) ##beruecksichtige keine leeren Felder
boxplot(na.omit(OEVAP30.erg$AP30.cv[OEVAP30.erg$VZTyp==4])) ##Plotte nur fur Typ 4
#--MIV--#
boxplot(na.omit(MIVAP30.erg[MIVAP30.erg$AP30.len>10,]$AP30.cv)) ##beruecksichtige keine leeren Felder
boxplot(na.omit(MIVAP30.erg$AP30.cv[MIVAP30.erg$VZTyp==4])) ##Plotte nur fur Typ 4

#Diagramme Zuschnitt, Kumulation
#--OEV--#
Visual.mm(OEVAP30.erg,ctgs.oev,400,"PT AP30 Study area","OEV_AP30.cv",T)
Visual.mm.g(OEVAP30.erg,ctgs.oev,400,"PT AP30 Municipality","OEV_AP30.cv",4,T)
Visual.mm.g(OEVAP30.erg,ctgs.oev,400,"PT AP30 Hamburg (statistical areas)","OEV_AP30.cv",5,T)
Visual.mm(OEVAP60.erg,ctgs.oev,800,"PT AP60 Study area","AP60.cv",T)
Visual.mm(OEVAP30.erg500,ctgs.oev,5000,"PT AP30 R500","OEV_AP30.cv",T)
Visual.mm(OEVAP60.erg500,ctgs.oev,15000,"PT AP60 R500","AP60.cv",T)
#--MIV--#
Visual.mm(MIVAP30.erg,ctgs.miv,2000,"Car AP30 Study area","Pkw_AP30.cv",T)
Visual.mm(MIVAP30.erg500,ctgs.miv,25000,"Car AP30 R500","Pkw_AP30.cv",T)
Visual.mm(MIVAP60.erg500,ctgs.miv,25000,"Car AP60 R500","Pkw_AP60.cv",T)
Visual.mm.g(MIVAP30.erg,ctgs.miv,450,"Car AP30 Hamburg (statistical areas)","Pkw_AP30.cv",5)
Visual.mm.g(MIVAP30.erg,ctgs.miv,700,"Car AP30 Municipality","Pkw_AP30.cv",4)
#--FUSS--#
Visual.mm(FUSSAP15.erg,ctgs.miv,400,"Walk AP15 Study area","Fuss_AP15.cv",T)
Visual.mm(FUSSAP30.erg,ctgs.miv,500,"Walk AP30 Study area","Fuss_AP30.cv",T)
Visual.mm(FUSSAP15.erg500,ctgs.oev,10000,"Walk AP15 R500","Fuss_AP15.cv",T)
Visual.mm(FUSSAP30.erg500,ctgs.oev,12000,"Walk AP30 R500","Fuss_AP30.cv",T)

#Diff sd500 sd100
Visual.mm(OEVAP30.erg500,ctgs.oev,6000,"PT AP30 R500","OEV_AP30.cv",T)
Visual.mm(OEVAP30.sd500.erg,ctgs.oev,6000,"PT AP30 sd500","AP30.sd500.cv",T)
Visual.mm(OEVAP30.sd500.erg,ctgs.oev,15000,"PT AP30 (sd500-sd100) ","AP30.sd500.cv.diff",T)

#Arzt, Aufwandsindikator
Visual.mm(OEVArzt.erg,ctgs.oev,400,"PT doctor Study area","OEV_Arzt.cv",T)
Visual.mm(OEVAP30.erg,ctgs.oev,400,"PT AP30 Study area","OEV_AP30.cv",T)
# Visual.mm.g(TAZ.oev.Arzt.erg,ctgs.oev,450,"PT doctor Municipality","Arzt.cv",4)
Visual.mm(OEVArzt.erg500,ctgs.oev,11000,"PT doctor R500","OEV_Arzt.cv",T)
Visual.mm(OEVAP30.erg500,ctgs.oev,11000,"PT AP30 R500","OEV_AP30.cv",T)
# Visual.mm.g(TAZ.oev.Arzt.erg,ctgs.oev,450,"PT doctor Hamburg (statistical areas)","Arzt.cv",5)

#Gebiet.R500
Visual.mm(OEVAP30.500.erg,ctgs.oev,1500,"PT AP30 Study area","OEV_AP30.cv",T)
Visual.mm(MIVAP30.500.erg,ctgs.oev,1000,"Car AP30 Study area","Pkw_AP30.cv",T)
Visual.mm.g(OEVAP30.500.erg,ctgs.oev,1500,"PT AP30 Municipality","OEV_AP30.cv",4,T)
Visual.mm.g(OEVAP30.500.erg,ctgs.oev,1500,"PT AP30 Hamburg (statistical areas)","OEV_AP30.cv",5,T)

#--Ende--#
dev.copy(png,'Abbildungen/OEV_micro_macro.png',width=1500, height=1000, pointsize=12)
dev.off()
