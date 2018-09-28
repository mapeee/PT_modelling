#####################################
#Inhalt:  Micro vs. Macro OEV 	    #
#Datum: 	Januar 2018			          #
#Autor: 	mape			 	              #
#####################################

##--Import Packages--##
library(rgdal)
library(plyr)

##--Formeln--##
var.ggs <- function(x) {n=length(x) ; sum((x-mean(x))^2)/n} ##varianz aus Grundgesamtheit
stdabw.ggs <- function(x) {n=length(x) ; sqrt(var.ggs(x))} ##stdabw aus Grundgesamtheit


#Standardabweichung und Mittelwert je Zelle
#-------100
OEVAP30.erg <- do.call(data.frame,aggregate(OEV_AP30 ~ IDVZelle+TYPENO, AP,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
PkwAP30.erg <- do.call(data.frame,aggregate(Pkw_AP30 ~ IDVZelle+TYPENO, AP,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
OEVAP60.erg <- do.call(data.frame,aggregate(OEV_AP60 ~ IDVZelle+TYPENO, AP,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
PkwAP60.erg <- do.call(data.frame,aggregate(Pkw_AP60 ~ IDVZelle+TYPENO, AP,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
OEVAP05.erg <- do.call(data.frame,aggregate(OEV_AP05 ~ IDVZelle+TYPENO, AP,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
PkwAP05.erg <- do.call(data.frame,aggregate(Pkw_AP05 ~ IDVZelle+TYPENO, AP,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
FussAP30.erg <- do.call(data.frame,aggregate(Fuss_AP30 ~ IDVZelle+TYPENO, AP,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
RadAP30.erg <- do.call(data.frame,aggregate(Rad_AP30 ~ IDVZelle+TYPENO, AP,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
FussAP15.erg <- do.call(data.frame,aggregate(Fuss_AP15 ~ IDVZelle+TYPENO, AP,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
RadAP15.erg <- do.call(data.frame,aggregate(Rad_AP15 ~ IDVZelle+TYPENO, AP,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
FussAP05.erg <- do.call(data.frame,aggregate(Fuss_AP05 ~ IDVZelle+TYPENO, AP,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
RadAP05.erg <- do.call(data.frame,aggregate(Rad_AP05 ~ IDVZelle+TYPENO, AP,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
AP.erg <- merge(OEVAP30.erg,PkwAP30.erg)
AP.erg <- merge(AP.erg,OEVAP60.erg)
AP.erg <- merge(AP.erg,PkwAP60.erg)
AP.erg <- merge(AP.erg,OEVAP05.erg)
AP.erg <- merge(AP.erg,PkwAP05.erg)
AP.erg <- merge(AP.erg,FussAP30.erg)
AP.erg <- merge(AP.erg,RadAP30.erg)
AP.erg <- merge(AP.erg,FussAP15.erg)
AP.erg <- merge(AP.erg,RadAP15.erg)
AP.erg <- merge(AP.erg,FussAP05.erg)
AP.erg <- merge(AP.erg,RadAP05.erg)

remove(OEVAP30.erg,PkwAP30.erg,FussAP30.erg,RadAP30.erg,OEVAP60.erg,PkwAP60.erg,FussAP15.erg,RadAP15.erg,FussAP05.erg,RadAP05.erg,OEVAP05.erg,PkwAP05.erg)

#Berechnung Varianzkoeffizient
AP.erg$OEV_AP30.cv <- with(AP.erg,OEV_AP30.sd/OEV_AP30.mean)
AP.erg$Pkw_AP30.cv <- with(AP.erg,Pkw_AP30.sd/Pkw_AP30.mean)
AP.erg$OEV_AP60.cv <- with(AP.erg,OEV_AP60.sd/OEV_AP60.mean)
AP.erg$Pkw_AP60.cv <- with(AP.erg,Pkw_AP60.sd/Pkw_AP60.mean)
AP.erg$OEV_AP05.cv <- with(AP.erg,OEV_AP05.sd/OEV_AP05.mean)
AP.erg$Pkw_AP05.cv <- with(AP.erg,Pkw_AP05.sd/Pkw_AP05.mean)
AP.erg$Fuss_AP30.cv <- with(AP.erg,Fuss_AP30.sd/Fuss_AP30.mean)
AP.erg$Rad_AP30.cv <- with(AP.erg,Rad_AP30.sd/Rad_AP30.mean)
AP.erg$Fuss_AP15.cv <- with(AP.erg,Fuss_AP15.sd/Fuss_AP15.mean)
AP.erg$Rad_AP15.cv <- with(AP.erg,Rad_AP15.sd/Rad_AP15.mean)
AP.erg$Fuss_AP05.cv <- with(AP.erg,Fuss_AP05.sd/Fuss_AP05.mean)
AP.erg$Rad_AP05.cv <- with(AP.erg,Rad_AP05.sd/Rad_AP05.mean)

#Weitere Datenaufbereitung
AP.erg <- merge(AP.erg,EWVZ,all.x = T,by = "IDVZelle")[(-51)] ##Merge EW und VZTyp und nimm zweite VZTyp weg
AP.erg$EW <- ifelse(is.na(AP.erg$EW),0, AP.erg$EW) 
AP.erg <- rename(AP.erg, c("TYPENO.x"="VZTyp"))
AP.erg[AP.erg=="NA"] <- NA
AP.erg[AP.erg=="NaN"] <- 0 ##Entferne die NaN bei Division durch 0 im .cv

#--Kategorisierung--#
#OEV
AP.erg$OEV_AP30.ctg <- cut(AP.erg$OEV_AP30.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.erg$OEV_AP30.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.erg$OEV_AP30.ctg[AP.erg$OEV_AP30.len<5] <- 'missing'
AP.erg$OEV_AP60.ctg <- cut(AP.erg$OEV_AP60.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.erg$OEV_AP60.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.erg$OEV_AP60.ctg[AP.erg$OEV_AP60.len<5] <- 'missing'
AP.erg$OEV_AP05.ctg <- cut(AP.erg$OEV_AP05.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.erg$OEV_AP05.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.erg$OEV_AP05.ctg[AP.erg$OEV_AP05.len<5] <- 'missing'
#Pkw
AP.erg$Pkw_AP30.ctg <- cut(AP.erg$Pkw_AP30.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.erg$Pkw_AP30.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.erg$Pkw_AP30.ctg[AP.erg$Pkw_AP30.len<5] <- 'missing'
AP.erg$Pkw_AP60.ctg <- cut(AP.erg$Pkw_AP60.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.erg$Pkw_AP60.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.erg$Pkw_AP60.ctg[AP.erg$Pkw_AP60.len<5] <- 'missing'
AP.erg$Pkw_AP05.ctg <- cut(AP.erg$Pkw_AP05.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.erg$Pkw_AP05.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.erg$Pkw_AP05.ctg[AP.erg$Pkw_AP05.len<5] <- 'missing'
#Fuss
AP.erg$Fuss_AP30.ctg <- cut(AP.erg$Fuss_AP30.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.erg$Fuss_AP30.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.erg$Fuss_AP30.ctg[AP.erg$Fuss_AP30.len<5] <- 'missing'
AP.erg$Fuss_AP15.ctg <- cut(AP.erg$Fuss_AP15.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.erg$Fuss_AP15.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.erg$Fuss_AP15.ctg[AP.erg$Fuss_AP15.len<5] <- 'missing'
AP.erg$Fuss_AP05.ctg <- cut(AP.erg$Fuss_AP05.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.erg$Fuss_AP05.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.erg$Fuss_AP05.ctg[AP.erg$Fuss_AP05.len<5] <- 'missing'
#Rad
AP.erg$Rad_AP30.ctg <- cut(AP.erg$Rad_AP30.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.erg$Rad_AP30.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.erg$Rad_AP30.ctg[AP.erg$Rad_AP30.len<5] <- 'missing'
AP.erg$Rad_AP15.ctg <- cut(AP.erg$Rad_AP15.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.erg$Rad_AP15.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.erg$Rad_AP15.ctg[AP.erg$Rad_AP15.len<5] <- 'missing'
AP.erg$Rad_AP05.ctg <- cut(AP.erg$Rad_AP05.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.erg$Rad_AP05.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.erg$Rad_AP05.ctg[AP.erg$Rad_AP05.len<5] <- 'missing'



#------500
OEVAP30.erg500 <- do.call(data.frame,aggregate(OEV_AP30 ~ ID500, AP,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
PkwAP30.erg500 <- do.call(data.frame,aggregate(Pkw_AP30 ~ ID500, AP,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
OEVAP60.erg500 <- do.call(data.frame,aggregate(OEV_AP60 ~ ID500, AP,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
PkwAP60.erg500 <- do.call(data.frame,aggregate(Pkw_AP60 ~ ID500, AP,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
OEVAP05.erg500 <- do.call(data.frame,aggregate(OEV_AP05 ~ ID500, AP,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
PkwAP05.erg500 <- do.call(data.frame,aggregate(Pkw_AP05 ~ ID500, AP,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
FussAP30.erg500 <- do.call(data.frame,aggregate(Fuss_AP30 ~ ID500, AP,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
RadAP30.erg500 <- do.call(data.frame,aggregate(Rad_AP30 ~ ID500, AP,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
FussAP15.erg500 <- do.call(data.frame,aggregate(Fuss_AP15 ~ ID500, AP,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
RadAP15.erg500 <- do.call(data.frame,aggregate(Rad_AP15 ~ ID500, AP,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
FussAP05.erg500 <- do.call(data.frame,aggregate(Fuss_AP05 ~ ID500, AP,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
RadAP05.erg500 <- do.call(data.frame,aggregate(Rad_AP05 ~ ID500, AP,function(x) c(len = length(x), mean = mean(x), sd = stdabw.ggs(x))))
AP.erg500 <- merge(OEVAP30.erg500,PkwAP30.erg500)
AP.erg500 <- merge(AP.erg500,OEVAP60.erg500)
AP.erg500 <- merge(AP.erg500,PkwAP60.erg500)
AP.erg500 <- merge(AP.erg500,OEVAP05.erg500)
AP.erg500 <- merge(AP.erg500,PkwAP05.erg500)
AP.erg500 <- merge(AP.erg500,FussAP30.erg500)
AP.erg500 <- merge(AP.erg500,RadAP30.erg500)
AP.erg500 <- merge(AP.erg500,FussAP15.erg500)
AP.erg500 <- merge(AP.erg500,RadAP15.erg500)
AP.erg500 <- merge(AP.erg500,FussAP05.erg500)
AP.erg500 <- merge(AP.erg500,RadAP05.erg500)
remove(OEVAP30.erg500,PkwAP30.erg500,FussAP30.erg500,RadAP30.erg500,OEVAP60.erg500,PkwAP60.erg500,FussAP15.erg500,RadAP15.erg500,FussAP05.erg500,RadAP05.erg500,OEVAP05.erg500,PkwAP05.erg500)

#Berechnung Varianzkoeffizient
AP.erg500$OEV_AP30.cv <- with(AP.erg500,OEV_AP30.sd/OEV_AP30.mean)
AP.erg500$Pkw_AP30.cv <- with(AP.erg500,Pkw_AP30.sd/Pkw_AP30.mean)
AP.erg500$OEV_AP60.cv <- with(AP.erg500,OEV_AP60.sd/OEV_AP60.mean)
AP.erg500$Pkw_AP60.cv <- with(AP.erg500,Pkw_AP60.sd/Pkw_AP60.mean)
AP.erg500$OEV_AP05.cv <- with(AP.erg500,OEV_AP05.sd/OEV_AP05.mean)
AP.erg500$Pkw_AP05.cv <- with(AP.erg500,Pkw_AP05.sd/Pkw_AP05.mean)
AP.erg500$Fuss_AP30.cv <- with(AP.erg500,Fuss_AP30.sd/Fuss_AP30.mean)
AP.erg500$Rad_AP30.cv <- with(AP.erg500,Rad_AP30.sd/Rad_AP30.mean)
AP.erg500$Fuss_AP15.cv <- with(AP.erg500,Fuss_AP15.sd/Fuss_AP15.mean)
AP.erg500$Rad_AP15.cv <- with(AP.erg500,Rad_AP15.sd/Rad_AP15.mean)
AP.erg500$Fuss_AP05.cv <- with(AP.erg500,Fuss_AP05.sd/Fuss_AP05.mean)
AP.erg500$Rad_AP05.cv <- with(AP.erg500,Rad_AP05.sd/Rad_AP05.mean)

#Weitere Datenaufbereitung
AP.erg500 <- merge(AP.erg500,EW500) ##Merge EW
AP.erg500[AP.erg500=="NA"] <- NA
AP.erg500[AP.erg500=="NaN"] <- 0 ##Entferne die NaN bei Division durch 0 im .cv

#--Kategorisierung--#
#OEV
AP.erg500$OEV_AP30.ctg <- cut(AP.erg500$OEV_AP30.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.erg500$OEV_AP30.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.erg500$OEV_AP30.ctg[AP.erg500$OEV_AP30.len<2] <- 'missing'
AP.erg500$OEV_AP60.ctg <- cut(AP.erg500$OEV_AP60.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.erg500$OEV_AP60.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.erg500$OEV_AP60.ctg[AP.erg500$OEV_AP60.len<2] <- 'missing'
AP.erg500$OEV_AP05.ctg <- cut(AP.erg500$OEV_AP05.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.erg500$OEV_AP05.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.erg500$OEV_AP05.ctg[AP.erg500$OEV_AP05.len<2] <- 'missing'
#Pkw
AP.erg500$Pkw_AP30.ctg <- cut(AP.erg500$Pkw_AP30.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.erg500$Pkw_AP30.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.erg500$Pkw_AP30.ctg[AP.erg500$Pkw_AP30.len<2] <- 'missing'
AP.erg500$Pkw_AP60.ctg <- cut(AP.erg500$Pkw_AP60.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.erg500$Pkw_AP60.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.erg500$Pkw_AP60.ctg[AP.erg500$Pkw_AP60.len<2] <- 'missing'
AP.erg500$Pkw_AP05.ctg <- cut(AP.erg500$Pkw_AP05.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.erg500$Pkw_AP05.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.erg500$Pkw_AP05.ctg[AP.erg500$Pkw_AP05.len<2] <- 'missing'
#Fuss
AP.erg500$Fuss_AP30.ctg <- cut(AP.erg500$Fuss_AP30.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.erg500$Fuss_AP30.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.erg500$Fuss_AP30.ctg[AP.erg500$Fuss_AP30.len<2] <- 'missing'
AP.erg500$Fuss_AP15.ctg <- cut(AP.erg500$Fuss_AP15.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.erg500$Fuss_AP15.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.erg500$Fuss_AP15.ctg[AP.erg500$Fuss_AP15.len<2] <- 'missing'
AP.erg500$Fuss_AP05.ctg <- cut(AP.erg500$Fuss_AP05.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.erg500$Fuss_AP05.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.erg500$Fuss_AP05.ctg[AP.erg500$Fuss_AP05.len<2] <- 'missing'
#Rad
AP.erg500$Rad_AP30.ctg <- cut(AP.erg500$Rad_AP30.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.erg500$Rad_AP30.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.erg500$Rad_AP30.ctg[AP.erg500$Rad_AP30.len<2] <- 'missing'
AP.erg500$Rad_AP15.ctg <- cut(AP.erg500$Rad_AP15.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.erg500$Rad_AP15.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.erg500$Rad_AP15.ctg[AP.erg500$Rad_AP15.len<2] <- 'missing'
AP.erg500$Rad_AP05.ctg <- cut(AP.erg500$Rad_AP05.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.erg500$Rad_AP05.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.erg500$Rad_AP05.ctg[AP.erg500$Rad_AP05.len<2] <- 'missing'
