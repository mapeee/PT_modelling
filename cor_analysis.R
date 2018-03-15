#####################################
#Inhalt:  Cor-Analysis    	        #
#Datum: 	Januar 2018			          #
#Autor: 	mape			 	              #
#####################################
#Korrelation zwischen dem cv, und verschiedenen Parametern (bspw. Flaeche und Einwohnerzahl)

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


cor.analyse(OEVArzt.erg,"OEV_Arzt.cv","Shape_Area")
cor.analyse(MIVArzt.erg,"MIV_Arzt.cv","Shape_Area")
cor.analyse(MIVOZ.erg,"MIV_OZ.cv","Shape_Area")

cor.analyse(OEVArzt.erg,"OEV_Arzt.cv","Hst.len")
cor.analyse(OEVArzt.erg500,"OEV_Arzt.cv","Hst.len")
cor.analyse(OEVArzt.erg,"OEV_Arzt.cv","BH.mean")
cor.analyse(OEVArzt.erg500,"OEV_Arzt.cv","BH.mean")
cor.analyse(OEVArzt.erg,"OEV_Arzt.cv","Fuss.share")
cor.analyse(OEVArzt.erg500,"OEV_Arzt.cv","Fuss.share")


#--Analyse Hst-Dichte--#
# E.Arzt.Hst <- merge(E.Arzt,Raster100[c("ID","IDVZelle","ID500")],by="ID")

OEVArzt.erg <- OEVArzt.erg[-c(9,10,11,12)]
OEVArzt.erg <- merge(OEVArzt.erg,aggregate(StartHst ~ IDVZelle, E.Arzt.Hst[E.Arzt.Hst$BH<100,], 
                                           function(x) length(unique(x))),all.x = T,by="IDVZelle") ##ohne Fusswege; unterschiedliche Haltestellen in Gebiet
colnames(OEVArzt.erg)[9] <- "Hst.len"
OEVArzt.erg$Hst.len[is.na(OEVArzt.erg$Hst.len)] <- 0 ##Wenn nur Fusswege in einem Gebiet. 0 wegen Vergleich zu wenigen unterschiedlichen Hst.
OEVArzt.erg <- merge(OEVArzt.erg,aggregate(BH ~ IDVZelle, E.Arzt.Hst[E.Arzt.Hst$BH<100,], FUN="mean"),all.x = T,by="IDVZelle") ##ohne Fusswege!
colnames(OEVArzt.erg)[10] <- "BH.mean"
# OEVArzt.erg$BH.mean[is.na(OEVArzt.erg$BH.mean)] <- 111 ##Wenn nur Fusswege in einem Gebiet. Hoehere Bedienung gleich besser (Annahme).
OEVArzt.erg <- merge(OEVArzt.erg,aggregate(BH ~ IDVZelle, E.Arzt.Hst[E.Arzt.Hst$BH>100,], FUN="length"),all.x = T,by="IDVZelle") ##nur Fusswege!
colnames(OEVArzt.erg)[11] <- "Fuss.len"
OEVArzt.erg$Fuss.share <- OEVArzt.erg$Fuss.len/OEVArzt.erg$OEV_Arzt.len
OEVArzt.erg$Fuss.share[is.na(OEVArzt.erg$Fuss.share)] <- 0 ##Wenn keine Fusswege, dann laenge Fusswege NA.

#--500--#
OEVArzt.erg500 <- OEVArzt.erg500[-c(8,9,10,11)]
OEVArzt.erg500 <- merge(OEVArzt.erg500,aggregate(StartHst ~ ID500, E.Arzt.Hst[E.Arzt.Hst$BH<100,], 
                                           function(x) length(unique(x))),all.x = T,by="ID500") ##ohne Fusswege; unterschiedliche Haltestellen in Gebiet
colnames(OEVArzt.erg500)[8] <- "Hst.len"
OEVArzt.erg500$Hst.len[is.na(OEVArzt.erg500$Hst.len)] <- 0 ##Wenn nur Fusswege in einem Gebiet. 0 wegen Vergleich zu wenigen unterschiedlichen Hst.
OEVArzt.erg500 <- merge(OEVArzt.erg500,aggregate(BH ~ ID500, E.Arzt.Hst[E.Arzt.Hst$BH<100,], FUN="mean"),all.x = T,by="ID500") ##ohne Fusswege!
colnames(OEVArzt.erg500)[9] <- "BH.mean"
# OEVArzt.erg500$BH.mean[is.na(OEVArzt.erg500$BH.mean)] <- 111 ##Wenn nur Fusswege in einem Gebiet. Hoehere Bedienung gleich besser (Annahme).
OEVArzt.erg500 <- merge(OEVArzt.erg500,aggregate(BH ~ ID500, E.Arzt.Hst[E.Arzt.Hst$BH>100,], FUN="length"),all.x = T,by="ID500") ##nur Fusswege!
colnames(OEVArzt.erg500)[10] <- "Fuss.len"
OEVArzt.erg500$Fuss.share <- OEVArzt.erg500$Fuss.len/OEVArzt.erg500$OEV_Arzt.len
OEVArzt.erg500$Fuss.share[is.na(OEVArzt.erg500$Fuss.share)] <- 0 ##Wenn keine Fusswege, dann laenge Fusswege NA.

