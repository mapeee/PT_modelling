#######################################################
#Inhalt:  Visualisierung R500 Differenz               #
#Datum: 	Januar 2018			                            #
#Autor: 	mape			 	                                #
#######################################################


##--Import Packages--##
library(rgdal)
library(ggplot2)

#1.  Parameter
dev.off()
par(mfrow=c(2,2)) ##zwei Spalten, zwei Zeilen
par(mar=c(2,2,2,2)) ##Keine Raender

#2. Diegramme
Visual.mm(OEVAP30.diff.erg,ctgs.oev,6000,"OEV AP30 mean.R500","AP30.diff.cv")
Visual.mm(OEVAP30.erg500,ctgs.oev,6000,"OEV AP30 R500","AP30.cv")
Visual.mm(OEVAP30.diff.erg,ctgs.oev,15000,"OEV AP30 (mean.R500-R100) ","AP30.diff.cv.diff")
