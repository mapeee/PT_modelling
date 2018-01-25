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
                   levels = c(labels.ctg,"missing")) ##Sortierung der Kategorien
ctgs.miv <- factor(as.factor(MIVAP30.erg$ctg), 
                   levels = c(labels.ctg,"missing")) ##Sortierung der Kategorien
#0.1 Vorbereitung Daten
OEVAP30.erg$VZTyp <- ifelse(is.na(OEVAP30.erg$VZTyp),0, OEVAP30.erg$VZTyp) ##replace NA Values
MIVAP30.erg$VZTyp <- ifelse(is.na(MIVAP30.erg$VZTyp),0, MIVAP30.erg$VZTyp) ##replace NA Values
OEVAP60.erg$VZTyp <- ifelse(is.na(OEVAP60.erg$VZTyp),0, OEVAP60.erg$VZTyp) ##replace NA Values
MIVAP60.erg$VZTyp <- ifelse(is.na(MIVAP60.erg$VZTyp),0, MIVAP60.erg$VZTyp) ##replace NA Values

#1. Daten verschneiden
TAZ.oev.AP30.erg <- merge(VZellen, OEVAP30.erg, by.x="NO", by.y="IDVZelle") ##VZ ohne Werte bekommen Wert 0
TAZ.miv.AP30.erg <- merge(VZellen, MIVAP30.erg, by.x="NO", by.y="IDVZelle") ##VZ ohne Werte bekommen Wert 0
TAZ.oev.AP60.erg <- merge(VZellen, OEVAP60.erg, by.x="NO", by.y="IDVZelle") ##VZ ohne Werte bekommen Wert 0
TAZ.miv.AP60.erg <- merge(VZellen, MIVAP60.erg, by.x="NO", by.y="IDVZelle") ##VZ ohne Werte bekommen Wert 0

#2.  Karte
dev.off()
par(mfrow=c(2,2)) ##zwei Spalten, zwei Zeilen
par(mar=c(2,2,2,2)) ##Keine Raender

#--OEV--#
plot(TAZ.oev.AP30.erg,col=Farbe[ctgs.oev],border=NA)
legend("bottomright", legend=levels(ctgs.oev),fill=Farbe)
plot(TAZ.oev.AP30.erg[TAZ.oev.AP30.erg$TYPENO==5,],col=Farbe[ctgs.oev],border=NA) ##nur Hamburg

#--MIV--#
plot(TAZ.miv.AP30.erg,col=Farbe[ctgs.miv],border=NA)
legend("bottomright", legend=levels(ctgs.miv),fill=Farbe)
plot(TAZ.miv.AP30.erg[TAZ.miv.AP30.erg$TYPENO==5,],col=Farbe[ctgs.miv],border=NA) ##nur Hamburg

#3. Diagramm
#--OEV--#
boxplot(na.omit(OEVAP30.erg$AP30.cv)) ##beruecksichtige keine leeren Felder
boxplot(na.omit(OEVAP30.erg$AP30.cv[OEVAP30.erg$VZTyp==4])) ##Plotte nur fur Typ 4

Visual.mm(TAZ.oev.AP30.erg,ctgs.oev,800,"OEV AP30 VZ","AP30.cv")
# Visual.mm.g(TAZ.oev.AP30.erg,ctgs.oev,450,"OEV AP30 Gemeinden","AP30.cv",4)
Visual.mm(TAZ.oev.AP60.erg,ctgs.oev,800,"OEV AP60 VZ","AP60.cv")
Visual.mm(OEVAP30.erg500,ctgs.oev,15000,"OEV AP30 R500","AP30.cv")
Visual.mm(OEVAP60.erg500,ctgs.oev,15000,"OEV AP60 R500","AP60.cv")
# Visual.mm.g(TAZ.oev.AP30.erg,ctgs.oev,450,"OEV AP30 Stat. Gebiete","AP30.cv",5)


#--MIV--#
boxplot(na.omit(MIVAP30.erg[MIVAP30.erg$AP30.len>10,]$AP30.cv)) ##beruecksichtige keine leeren Felder
boxplot(na.omit(MIVAP30.erg$AP30.cv[MIVAP30.erg$VZTyp==4])) ##Plotte nur fur Typ 4

Visual.mm(TAZ.miv.AP30.erg,ctgs.miv,2000,"MIV AP30 VZ","AP30.cv")
Visual.mm(MIVAP30.erg500,ctgs.miv,25000,"MIV AP30 R500","AP30.cv")
Visual.mm.g(TAZ.miv.AP30.erg,ctgs.miv,450,"MIV AP30 Stat. Gebiete","AP30.cv",5)
Visual.mm.g(TAZ.miv.AP30.erg,ctgs.miv,700,"MIV AP30 Gemeinden","AP30.cv",4)

#--Ende--#
dev.copy(png,'Abbildungen/OEV_micro_macro.png',width=1500, height=1000, pointsize=12)
dev.off()
