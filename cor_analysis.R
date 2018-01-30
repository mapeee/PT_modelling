#####################################
#Inhalt:  Cor-Analysis    	        #
#Datum: 	Januar 2018			          #
#Autor: 	mape			 	              #
#####################################
#Korrelation zwischen dem cv, der Flaeche und der Einwohnerzahl


#--Aufbereitung--#
#Hier werden die Zellen genommen, um die Flaeche zu erhalten.
#und missing flaechen werden nicht verwendet.
cor.ana.f <- as.data.frame(TAZ.miv.AP60.erg[!is.na(TAZ.miv.AP60.erg$AP60.cv),]) #nur die Zellen ohne NA
cor.ana.f <- cor.ana.f[cor.ana.f$ctg!='missing',]
# cor.ana.f <- cor.ana.f[cor.ana.f$TYPENO==5,]

#--Ergebnisse--#
#--flaeche
cor(cor.ana.f$Shape_Area,cor.ana.f$AP60.cv) #Cor zwischen Flaeche und cv
#--einwohner
#keine missings, in allen flaechen sind einwohner enthalten.
cor(OEVAP60.erg500[OEVAP60.erg500$ctg!='missing',]$EW,OEVAP60.erg500[OEVAP60.erg500$ctg!='missing',]$AP60.cv)
