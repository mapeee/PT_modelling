#####################################
#Inhalt:  Micro vs. Macro OEV 	    #
#Datum: 	Januar 2018			          #
#Autor: 	mape			 	              #
#####################################


##--Import Packages--##
library(rgdal)

##--Formeln--##
var.ggs <- function(x) {n=length(x) ; sum((x-mean(x))^2)/n} ##varianz aus Grundgesamtheit
stdabw.ggs <- function(x) {n=length(x) ; sqrt(var.ggs(x))} ##stdabw aus Grundgesamtheit


#Standardabweichung und Mittelwert je Zelle
OEVAP30.erg <- do.call(data.frame,aggregate(OEV_AP30 ~ IDVZelle+TYPENO, AP30, 
                          function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))

OEVAP30.erg500 <- do.call(data.frame,aggregate(OEV_AP30 ~ ID500, AP30,
                                            function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))

#Berechnung Varianzkoeffizient
OEVAP30.erg$OEV_AP30.cv <- with(OEVAP30.erg,OEV_AP30.sd/OEV_AP30.mean)
OEVAP30.erg500$OEV_AP30.cv <- with(OEVAP30.erg500,OEV_AP30.sd/OEV_AP30.mean)

#Weitere Datenaufbereitung
OEVAP30.erg <- merge(OEVAP30.erg,EWVZ,all.x = T,by = "IDVZelle")[(-7)] ##Merge EW und VZTyp und nimm zweite VZTyp weg
OEVAP30.erg$EW <- ifelse(is.na(OEVAP30.erg$EW),
                          0, OEVAP30.erg$EW) 

OEVAP30.erg[OEVAP30.erg=="NA"] <- NA
OEVAP30.erg[OEVAP30.erg=="NaN"] <- 0 ##Entferne die NaN bei Division durch 0 im .cv

#--500--#
OEVAP30.erg500 <- merge(OEVAP30.erg500,EW500) ##Merge EW

OEVAP30.erg500[OEVAP30.erg500=="NA"] <- NA
OEVAP30.erg500[OEVAP30.erg500=="NaN"] <- 0 ##Entferne die NaN bei Division durch 0 im .cv

#--Kategorisierung--#
colnames(OEVAP30.erg) <- c("IDVZelle","VZTyp","OEV_AP30.len","OEV_AP30.mean","OEV_AP30.sd","OEV_AP30.cv","EW")

#--OEV--#
OEVAP30.erg$ctg <- as.character(cut(OEVAP30.erg$OEV_AP30.cv, 
                                    breaks = breaks.ctg, 
                                    labels = labels.ctg)) ##Um nicht als 'factor' zu speichern
OEVAP30.erg[OEVAP30.erg=="NA"] <- NA ##'NA' durch NA, dann ersetzen
OEVAP30.erg$ctg <- ifelse(is.na(OEVAP30.erg$ctg),
                             0, OEVAP30.erg$ctg) ##An dieser Stelle damit die NA nach Division durch 0 verschwinden.
OEVAP30.erg$ctg <- ifelse(OEVAP30.erg$OEV_AP30.len<5,'missing', 
                          OEVAP30.erg$ctg) ##Missung wenn zu wenige Faelle
OEVAP30.erg$ctg <- ifelse(is.na(OEVAP30.erg$ctg),'missing', OEVAP30.erg$ctg) ##Wenn AP30.len NA, dann wird ctg auch wieder NA

#--500--#
OEVAP30.erg500$ctg <- as.character(cut(OEVAP30.erg500$OEV_AP30.cv, 
                                    breaks = breaks.ctg, 
                                    labels = labels.ctg)) ##Um nicht als 'factor' zu speichern
OEVAP30.erg500[OEVAP30.erg500=="NA"] <- NA ##'NA' durch NA, dann ersetzen
OEVAP30.erg500$ctg <- ifelse(is.na(OEVAP30.erg500$ctg),
                             0, OEVAP30.erg500$ctg) ##An dieser Stelle damit die NA nach Division durch 0 verschwinden.
OEVAP30.erg500$ctg <- ifelse(OEVAP30.erg500$OEV_AP30.len<2,'missing', 
                             OEVAP30.erg500$ctg) ##Missung wenn zu wenige Faelle, hier 3
OEVAP30.erg500$ctg <- ifelse(is.na(OEVAP30.erg500$ctg),'missing', OEVAP30.erg500$ctg)


#------------------MIV--------------------------#

#Standardabweichung und Mittelwert je Zelle
MIVAP30.erg <- do.call(data.frame,aggregate(Pkw_AP30 ~ IDVZelle+TYPENO, AP30, 
                                            function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))

MIVAP30.erg500 <- do.call(data.frame,aggregate(Pkw_AP30 ~ ID500, AP30,
                                               function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))

#Berechnung Varianzkoeffizient
MIVAP30.erg$Pkw_AP30.cv <- with(MIVAP30.erg,Pkw_AP30.sd/Pkw_AP30.mean)
MIVAP30.erg500$Pkw_AP30.cv <- with(MIVAP30.erg500,Pkw_AP30.sd/Pkw_AP30.mean)

#Weitere Datenaufbereitung
MIVAP30.erg <- merge(MIVAP30.erg,EWVZ,all.x = T,by = "IDVZelle")[(-7)] ##Merge EW und VZTyp und nimm zweite VZTyp weg
MIVAP30.erg$EW <- ifelse(is.na(MIVAP30.erg$EW),
                         0, MIVAP30.erg$EW) 

MIVAP30.erg[MIVAP30.erg=="NA"] <- NA
MIVAP30.erg[MIVAP30.erg=="NaN"] <- 0 ##Entferne die NaN bei Division durch 0 im .cv

#--500--#
MIVAP30.erg500 <- merge(MIVAP30.erg500,EW500) ##Merge EW

MIVAP30.erg500[MIVAP30.erg500=="NA"] <- NA
MIVAP30.erg500[MIVAP30.erg500=="NaN"] <- 0 ##Entferne die NaN bei Division durch 0 im .cv

#--Kategorisierung--#
colnames(MIVAP30.erg) <- c("IDVZelle","VZTyp","Pkw_AP30.len","Pkw_AP30.mean","Pkw_AP30.sd","Pkw_AP30.cv","EW")

#--MIV--#
MIVAP30.erg$ctg <- as.character(cut(MIVAP30.erg$Pkw_AP30.cv, 
                                    breaks = breaks.ctg, 
                                    labels = labels.ctg)) ##Um nicht als 'factor' zu speichern
MIVAP30.erg[MIVAP30.erg=="NA"] <- NA ##'NA' durch NA, dann ersetzen
MIVAP30.erg$ctg <- ifelse(is.na(MIVAP30.erg$ctg),
                          0, MIVAP30.erg$ctg) ##An dieser Stelle damit die NA nach Division durch 0 verschwinden.
MIVAP30.erg$ctg <- ifelse(MIVAP30.erg$Pkw_AP30.len<5,'missing', 
                          MIVAP30.erg$ctg) ##Missung wenn zu wenige Faelle
MIVAP30.erg$ctg <- ifelse(is.na(MIVAP30.erg$ctg),'missing', MIVAP30.erg$ctg) ##Wenn AP30.len NA, dann wird ctg auch wieder NA

#--500--#
MIVAP30.erg500$ctg <- as.character(cut(MIVAP30.erg500$Pkw_AP30.cv, 
                                       breaks = breaks.ctg, 
                                       labels = labels.ctg)) ##Um nicht als 'factor' zu speichern
MIVAP30.erg500[MIVAP30.erg500=="NA"] <- NA ##'NA' durch NA, dann ersetzen
MIVAP30.erg500$ctg <- ifelse(is.na(MIVAP30.erg500$ctg),
                             0, MIVAP30.erg500$ctg) ##An dieser Stelle damit die NA nach Division durch 0 verschwinden.
MIVAP30.erg500$ctg <- ifelse(MIVAP30.erg500$Pkw_AP30.len<2,'missing', 
                             MIVAP30.erg500$ctg) ##Missung wenn zu wenige Faelle, hier 3
MIVAP30.erg500$ctg <- ifelse(is.na(MIVAP30.erg500$ctg),'missing', MIVAP30.erg500$ctg)



#------------------Fuss--------------------------#
#Standardabweichung und Mittelwert je Zelle
FUSSAP30.erg <- do.call(data.frame,aggregate(Fuss_AP30 ~ IDVZelle+TYPENO, AP.Fuss, 
                                            function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
FUSSAP30.erg500 <- do.call(data.frame,aggregate(Fuss_AP30 ~ ID500, AP.Fuss,
                                               function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))

#Berechnung Varianzkoeffizient
FUSSAP30.erg$FUSS_AP30.cv <- with(FUSSAP30.erg,Fuss_AP30.sd/Fuss_AP30.mean)
FUSSAP30.erg500$Fuss_AP30.cv <- with(FUSSAP30.erg500,Fuss_AP30.sd/Fuss_AP30.mean)

#Weitere Datenaufbereitung
FUSSAP30.erg <- merge(FUSSAP30.erg,EWVZ,all.x = T,by = "IDVZelle")[(-7)] ##Merge EW und VZTyp und nimm zweite VZTyp weg
FUSSAP30.erg$EW <- ifelse(is.na(FUSSAP30.erg$EW),
                         0, FUSSAP30.erg$EW) 

FUSSAP30.erg[FUSSAP30.erg=="NA"] <- NA
FUSSAP30.erg[FUSSAP30.erg=="NaN"] <- 0 ##Entferne die NaN bei Division durch 0 im .cv

#--500--#
FUSSAP30.erg500 <- merge(FUSSAP30.erg500,EW500) ##Merge EW

FUSSAP30.erg500[FUSSAP30.erg500=="NA"] <- NA
FUSSAP30.erg500[FUSSAP30.erg500=="NaN"] <- 0 ##Entferne die NaN bei Division durch 0 im .cv

#--Kategorisierung--#
colnames(FUSSAP30.erg) <- c("IDVZelle","VZTyp","Fuss_AP30.len","FUSS_AP30.mean","Fuss_AP30.sd","Fuss_AP30.cv","EW")

#--FUSS--#
FUSSAP30.erg$ctg <- as.character(cut(FUSSAP30.erg$Fuss_AP30.cv, 
                                    breaks = breaks.ctg, 
                                    labels = labels.ctg)) ##Um nicht als 'factor' zu speichern
FUSSAP30.erg[FUSSAP30.erg=="NA"] <- NA ##'NA' durch NA, dann ersetzen
FUSSAP30.erg$ctg <- ifelse(is.na(FUSSAP30.erg$ctg),
                          0, FUSSAP30.erg$ctg) ##An dieser Stelle damit die NA nach Division durch 0 verschwinden.
FUSSAP30.erg$ctg <- ifelse(FUSSAP30.erg$Fuss_AP30.len<5,'missing', 
                          FUSSAP30.erg$ctg) ##Missung wenn zu wenige Faelle
FUSSAP30.erg$ctg <- ifelse(is.na(FUSSAP30.erg$ctg),'missing', FUSSAP30.erg$ctg) ##Wenn AP30.len NA, dann wird ctg auch wieder NA

#--500--#
FUSSAP30.erg500$ctg <- as.character(cut(FUSSAP30.erg500$Fuss_AP30.cv, 
                                       breaks = breaks.ctg, 
                                       labels = labels.ctg)) ##Um nicht als 'factor' zu speichern
FUSSAP30.erg500[FUSSAP30.erg500=="NA"] <- NA ##'NA' durch NA, dann ersetzen
FUSSAP30.erg500$ctg <- ifelse(is.na(FUSSAP30.erg500$ctg),
                             0, FUSSAP30.erg500$ctg) ##An dieser Stelle damit die NA nach Division durch 0 verschwinden.
FUSSAP30.erg500$ctg <- ifelse(FUSSAP30.erg500$Fuss_AP30.len<2,'missing', 
                              FUSSAP30.erg500$ctg) ##Missung wenn zu wenige Faelle, hier 3
FUSSAP30.erg500$ctg <- ifelse(is.na(FUSSAP30.erg500$ctg),'missing', FUSSAP30.erg500$ctg)