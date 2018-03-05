#####################################
#Inhalt:  Micro vs. Macro OEV 	    #
#Datum: 	Januar 2018			          #
#Autor: 	mape			 	              #
#####################################

#Ziel: Wie stark streuen eigentlich die Werte von R500 innerhalb der VZ?
#Vorgehen: Analog zu den R100 Zellen, jedoch mit den R500 Zellen

##--Import Packages--##
library(rgdal)


#Standardabweichung und Mittelwert je Zelle
OEVAP30.500.erg <- do.call(data.frame,aggregate(OEV_AP30 ~ IDVZelle+TYPENO, AP30.500[AP30.500$EW>0,], 
                                            function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))

#Berechnung Varianzkoeffizient
OEVAP30.500.erg$OEV_AP30.cv <- with(OEVAP30.500.erg,OEV_AP30.sd/OEV_AP30.mean)

#Weitere Datenaufbereitung
OEVAP30.500.erg <- merge(OEVAP30.500.erg,EWVZ,all.x = T,by = "IDVZelle")[(-7)] ##Merge EW und VZTyp und nimm zweite VZTyp weg
OEVAP30.500.erg$EW <- ifelse(is.na(OEVAP30.500.erg$EW),
                         0, OEVAP30.500.erg$EW) 

OEVAP30.500.erg[OEVAP30.500.erg=="NA"] <- NA
OEVAP30.500.erg[OEVAP30.500.erg=="NaN"] <- 0 ##Entferne die NaN bei Division durch 0 im .cv


#--Kategorisierung--#
colnames(OEVAP30.500.erg) <- c("IDVZelle","VZTyp","OEV_AP30.len","OEV_AP30.mean","OEV_AP30.sd","OEV_AP30.cv","EW")

#--OEV--#
OEVAP30.500.erg$ctg <- cut(OEVAP30.500.erg$OEV_AP30.cv, 
                       breaks = breaks.ctg, 
                       labels = labels.ctg)

levels(OEVAP30.500.erg$ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
OEVAP30.500.erg$ctg[OEVAP30.500.erg$OEV_AP30.len<2] <- 'missing'


#----MIV----#
#Standardabweichung und Mittelwert je Zelle
MIVAP30.500.erg <- do.call(data.frame,aggregate(Pkw_AP30 ~ IDVZelle+TYPENO, AP30.500[AP30.500$EW>0,], 
                                                function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))

#Berechnung Varianzkoeffizient
MIVAP30.500.erg$Pkw_AP30.cv <- with(MIVAP30.500.erg,Pkw_AP30.sd/Pkw_AP30.mean)

#Weitere Datenaufbereitung
MIVAP30.500.erg <- merge(MIVAP30.500.erg,EWVZ,all.x = T,by = "IDVZelle")[(-7)] ##Merge EW und VZTyp und nimm zweite VZTyp weg
MIVAP30.500.erg$EW <- ifelse(is.na(MIVAP30.500.erg$EW),
                             0, MIVAP30.500.erg$EW) 

MIVAP30.500.erg[MIVAP30.500.erg=="NA"] <- NA
MIVAP30.500.erg[MIVAP30.500.erg=="NaN"] <- 0 ##Entferne die NaN bei Division durch 0 im .cv


#--Kategorisierung--#
colnames(MIVAP30.500.erg) <- c("IDVZelle","VZTyp","Pkw_AP30.len","Pkw_AP30.mean","Pkw_AP30.sd","Pkw_AP30.cv","EW")

#--MIV--#
MIVAP30.500.erg$ctg <- cut(MIVAP30.500.erg$Pkw_AP30.cv, 
                           breaks = breaks.ctg, 
                           labels = labels.ctg)

levels(MIVAP30.500.erg$ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
MIVAP30.500.erg$ctg[MIVAP30.500.erg$Pkw_AP30.len<2] <- 'missing'

