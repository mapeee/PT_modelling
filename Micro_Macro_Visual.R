#####################################
#Inhalt:  Test Scripte  	          #
#Datum: 	Januar 2018			          #
#Autor: 	mape			 	              #
#####################################
##graphics.off()
##dev.off()


##--Import Packages--##
library(rgdal)


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

#1. Daten verschneiden
VZellen.oev.erg <- merge(VZellen, OEVAP30.erg, by.x="NO", by.y="IDVZelle") ##VZ ohne Werte bekommen Wert 0
VZellen.miv.erg <- merge(VZellen, MIVAP30.erg, by.x="NO", by.y="IDVZelle") ##VZ ohne Werte bekommen Wert 0

#2.  Karte
par(mfrow=c(2,2)) ##zwei Spalten, zwei Zeilen
par(mar=c(2,2,0.6,0)) ##Keine Raender

#--OEV--#
plot(VZellen.oev.erg,col=Farbe[ctgs.oev],border=NA)
legend("bottomright", legend=levels(ctgs.oev),fill=Farbe)
plot(VZellen.oev.erg[VZellen.oev.erg$TYPENO==5,],col=Farbe[ctgs.oev],border=NA) ##nur Hamburg

#--MIV--#
plot(VZellen.miv.erg,col=Farbe[ctgs.miv],border=NA)
legend("bottomright", legend=levels(ctgs.miv),fill=Farbe)
plot(VZellen.miv.erg[VZellen.miv.erg$TYPENO==5,],col=Farbe[ctgs.miv],border=NA) ##nur Hamburg

#3. Diagramm
#--OEV--#
boxplot(na.omit(OEVAP30.erg$AP30EW.cv)) ##beruecksichtige keine leeren Felder
boxplot(na.omit(OEVAP30.erg$AP30EW.cv[OEVAP30.erg$VZTyp==4])) ##Plotte nur fur Typ 4
barplot(table(VZellen.oev.erg$ctg)[levels(ctgs.oev)],
        names.arg=levels(ctgs.oev),
        main = "OEV AP30 VZ",
        ylim = c(0,1000),
        # yaxt='n',
        xlab = "Kategorie",
        xpd = FALSE,
        col=Farbe,
        cex.names = 0.8)
barplot(table(VZellen.oev.erg[VZellen.oev.erg$TYPENO==5,]$ctg)[levels(ctgs.oev)],
        names.arg=levels(ctgs.oev),
        main = "OEV AP30 Stat. Gebiete",
        ylim = c(0,1000),
        yaxt='n',
        xlab = "Kategorie",
        xpd = FALSE,
        col=Farbe,
        cex.names = 0.8)
barplot(table(OEVAP30.erg500[OEVAP30.erg500$ctg!='missing',]$ctg)[levels(ctgs.oev)],
        names.arg=levels(ctgs.oev),
        main = "OEV AP30 R500",
        ylim = c(0,10000),
        # yaxt='n',
        xlab = "Kategorie",
        xpd = FALSE,        
        col = Farbe,
        cex.names = 0.8)

#--MIV--#
boxplot(na.omit(MIVAP30.erg[MIVAP30.erg$AP30EW.len>10,]$AP30EW.cv)) ##beruecksichtige keine leeren Felder
boxplot(na.omit(MIVAP30.erg$AP30EW.cv[MIVAP30.erg$VZTyp==4])) ##Plotte nur fur Typ 4
barplot(table(MIVAP30.erg$ctg)[levels(ctgs.miv)],
        names.arg=levels(ctgs.miv),
        main = "MIV AP30 VZ",
        ylim = c(0,1000),
        # yaxt='n',
        xlab = "Kategorie",
        xpd = FALSE,
        col = Farbe,
        cex.names = 0.8)
barplot(table(VZellen.miv.erg[VZellen.miv.erg$TYPENO==5,]$ctg)[levels(ctgs.miv)],
        names.arg=levels(ctgs.miv),
        main = "MIV AP30 Stat. Gebiete",
        ylim = c(0,1000),
        yaxt='n',
        xlab = "Kategorie",
        xpd = FALSE,
        col = Farbe,
        cex.names = 0.8)
barplot(table(MIVAP30.erg500[MIVAP30.erg500$ctg!='missing',]$ctg)[levels(ctgs.miv)],
        names.arg=levels(ctgs.miv),
        main = "MIV AP30 R500",
        ylim = c(0,10000),
        # yaxt='n',
        xlab = "Kategorie",
        xpd = FALSE,
        col = Farbe,
        cex.names = 0.8)

#--Ende--#
dev.copy(png,'Abbildungen/OEV_micro_macro.png',width=1500, height=1000, pointsize=12)
dev.off()
