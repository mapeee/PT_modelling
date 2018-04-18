#####################################
#Inhalt:  Cor-Analysis    	        #
#Datum: 	Januar 2018			          #
#Autor: 	mape			 	              #
#####################################
#Korrelation zwischen dem cv, und verschiedenen Parametern (bspw. Flaeche und Einwohnerzahl)
##--Import Packages--##
library(rgdal)
library(plyr)

cor.analyse <- function(par1,par2,par3,par4){
  #--Daten verschneiden und vorbereiten--#
  if(par3=="Shape_Area"){
    Area <- as.data.frame(merge(VZellen, par1, by.x="NO", by.y="IDVZelle")) ##VZ ohne Werte bekommen Wert 0
    Area <- Area[Area[,par4]!='missing',] ##keine Beruesichtigung von 'missings'
    Area <- na.omit(Area) ##keine Beruecksichtigung von NAs
    print(paste("cor: ",par2," / ",par3))
    cor(Area[,par2],Area[,par3])
    }
  else{ ##wenn es nicht um Flaechen geht, wird keine Flaeche gejoined!!
    Area <- par1[par1[,par4]!='missing',] ##keine Beruesichtigung von 'missings'
    Area <- Area[complete.cases(Area[c(par2,par3)]),c(par2,par3)] ##keine Beruecksichtigung von NAs
    print(paste("cor: ",par2," / ",par3))
    cor(Area[,par2],Area[,par3])
    }
  }

#--Analyse--#
cor.analyse(AP.erg,"OEV_AP30.cv","Shape_Area","OEV_AP30.ctg")
cor.analyse(AP.erg,"Pkw_AP30.cv","Shape_Area","Pkw_AP30.ctg")
cor.analyse(AP.erg,"Fuss_AP30.cv","Shape_Area","Fuss_AP30.ctg")
cor.analyse(AP.erg,"Rad_AP30.cv","Shape_Area","Rad_AP30.ctg")
cor.analyse(AP.erg,"OEV_AP30.cv","EW","OEV_AP30.ctg")
cor.analyse(AP.erg,"OEV_AP30.cv","OEV_AP30.len","OEV_AP30.ctg")
cor.analyse(AP.erg500,"OEV_AP30.cv","EW","OEV_AP30.ctg")
cor.analyse(AP.erg,"OEV_AP60.cv","Shape_Area","OEV_AP60.ctg")
cor.analyse(AP.erg,"OEV_AP60.cv","EW","OEV_AP60.ctg")
cor.analyse(E.Arzt.erg,"Pkw.cv","Shape_Area","Pkw.ctg")

#OEV-Modellierung
cor(E.Arzt.cor[E.Arzt.cor$OEV.Hst>0,]$OEV.cv,E.Arzt.cor[E.Arzt.cor$OEV.Hst>0,]$OEV.Hst)
cor(E.Arzt.cor[E.Arzt.cor$OEV.BH>0,]$OEV.cv,E.Arzt.cor[E.Arzt.cor$OEV.BH>0,]$OEV.BH)
cor(E.Arzt.cor[E.Arzt.cor$OEV.Fuss>0,]$OEV.cv,E.Arzt.cor[E.Arzt.cor$OEV.Fuss>0,]$OEV.Fuss)

cor(E.Arzt.cor500[E.Arzt.cor500$OEV.Hst>0,]$OEV.cv,E.Arzt.cor500[E.Arzt.cor500$OEV.Hst>0,]$OEV.Hst)
cor(E.Arzt.cor500[E.Arzt.cor500$OEV.BH>0,]$OEV.cv,E.Arzt.cor500[E.Arzt.cor500$OEV.BH>0,]$OEV.BH)
cor(E.Arzt.cor500[E.Arzt.cor500$OEV.Fuss>0,]$OEV.cv,E.Arzt.cor500[E.Arzt.cor500$OEV.Fuss>0,]$OEV.Fuss)




#--Analyse OEV-Modellierung--#
E.Arzt.cor <- E.Arzt
E.Arzt.cor <- rename(E.Arzt.cor, c("Minuten_OEV"="OEV","Minuten_Pkw"="Pkw","Minuten_Fuss"="Fuss","Minuten_Rad"="Rad"))
E.Arzt.cor[E.Arzt.cor$OEV>120,]$OEV <- 120 ##Um eine bessere Vergleichbarkeit zu erzielen.
E.Arzt.cor[E.Arzt.cor$OEV<1,]$OEV <- 1 ##Um eine bessere Vergleichbarkeit zu erzielen.

E.Arzt.Hst = aggregate(StartHst ~ IDVZelle, E.Arzt.cor[E.Arzt.cor$Umstiege<100,],function(x) length(unique(x))) ##Ohne Direktwege
E.Arzt.Hst <-rename(E.Arzt.Hst, c("StartHst"="OEV.Hst"))
E.Arzt.BH = aggregate(Verbindungen ~ IDVZelle, E.Arzt.cor[E.Arzt.cor$Umstiege<100,], FUN="mean") ##Ohne Direktwege
E.Arzt.BH <-rename(E.Arzt.BH, c("Verbindungen"="OEV.BH"))
E.Arzt.Fuss = aggregate(StartHst ~ IDVZelle, E.Arzt.cor[E.Arzt.cor$Umstiege>100,],FUN="length") ##Ohne Direktwege
E.Arzt.Fuss <-rename(E.Arzt.Fuss, c("StartHst"="OEV.Fuss"))

E.Arzt.cor <- merge(E.Arzt.erg[c("IDVZelle","OEV.cv","OEV.len")],E.Arzt.Hst,all.x=T)
E.Arzt.cor <- merge(E.Arzt.cor,E.Arzt.BH,all.x=T)
E.Arzt.cor <- merge(E.Arzt.cor,E.Arzt.Fuss,all.x=T)

remove(E.Arzt.Hst,E.Arzt.BH,E.Arzt.Fuss)

#Aufbereitung
E.Arzt.cor <- E.Arzt.cor[E.Arzt.cor$OEV.len>4,] ##Nimm nur Gebiete, die mind. 5 Werte haben.

E.Arzt.cor$OEV.Hst[is.na(E.Arzt.cor$OEV.Hst)] <-0
E.Arzt.cor$OEV.BH[is.na(E.Arzt.cor$OEV.BH)] <-0
E.Arzt.cor$OEV.Fuss[is.na(E.Arzt.cor$OEV.Fuss)] <-0


#--500--#
E.Arzt.cor500 <- E.Arzt
E.Arzt.cor500 <- rename(E.Arzt.cor500, c("Minuten_OEV"="OEV","Minuten_Pkw"="Pkw","Minuten_Fuss"="Fuss","Minuten_Rad"="Rad"))
E.Arzt.cor500[E.Arzt.cor500$OEV>120,]$OEV <- 120 ##Um eine bessere Vergleichbarkeit zu erzielen.
E.Arzt.cor500[E.Arzt.cor500$OEV<1,]$OEV <- 1 ##Um eine bessere Vergleichbarkeit zu erzielen.

E.Arzt.Hst = aggregate(StartHst ~ ID500, E.Arzt.cor500[E.Arzt.cor500$Umstiege<100,],function(x) length(unique(x))) ##Ohne Direktwege
E.Arzt.Hst <-rename(E.Arzt.Hst, c("StartHst"="OEV.Hst"))
E.Arzt.BH = aggregate(Verbindungen ~ ID500, E.Arzt.cor500[E.Arzt.cor500$Umstiege<100,], FUN="mean") ##Ohne Direktwege
E.Arzt.BH <-rename(E.Arzt.BH, c("Verbindungen"="OEV.BH"))
E.Arzt.Fuss = aggregate(StartHst ~ ID500, E.Arzt.cor500[E.Arzt.cor500$Umstiege>100,],FUN="length") ##Ohne Direktwege
E.Arzt.Fuss <-rename(E.Arzt.Fuss, c("StartHst"="OEV.Fuss"))

E.Arzt.cor500 <- merge(E.Arzt.erg500[c("ID500","OEV.cv","OEV.len")],E.Arzt.Hst,all.x=T)
E.Arzt.cor500 <- merge(E.Arzt.cor500,E.Arzt.BH,all.x=T)
E.Arzt.cor500 <- merge(E.Arzt.cor500,E.Arzt.Fuss,all.x=T)

remove(E.Arzt.Hst,E.Arzt.BH,E.Arzt.Fuss)

#Aufbereitung
E.Arzt.cor500 <- E.Arzt.cor500[E.Arzt.cor500$OEV.len>1,] ##Nimm nur Gebiete, die mind. 1 Werte haben.

E.Arzt.cor500$OEV.Hst[is.na(E.Arzt.cor500$OEV.Hst)] <-0
E.Arzt.cor500$OEV.BH[is.na(E.Arzt.cor500$OEV.BH)] <-0
E.Arzt.cor500$OEV.Fuss[is.na(E.Arzt.cor500$OEV.Fuss)] <-0
