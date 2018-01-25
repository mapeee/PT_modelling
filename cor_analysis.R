#####################################
#Inhalt:  Cor-Analysis    	        #
#Datum: 	Januar 2018			          #
#Autor: 	mape			 	              #
#####################################


#--Aufbereitung--#
#Hier werden die Zellen genommen, um die Flaeche zu erhalten.
cor.ana.f <- as.data.frame(TAZ.miv.AP30.erg[!is.na(TAZ.miv.AP30.erg$AP30.cv),]) #nur die Zellen ohne NA
cor.ana.f <- cor.ana.f[cor.ana.f$ctg!='missing',]
# cor.ana.f <- cor.ana.f[cor.ana.f$TYPENO==4,]

#Merge der Einwohner, um cor ueber EW zu ermoeglichen
cor.ana.ew <- merge(OEVAP30.erg500[OEVAP30.erg500$AP30.cv>0,],EW500,by="ID500")
#--Ergebnisse--#
cor(cor.ana.f$Shape_Area,cor.ana.f$AP30.cv) #Cor zwischen Flaeche und cv
cor(cor.ana.ew$EW,cor.ana.ew$AP30.cv)

