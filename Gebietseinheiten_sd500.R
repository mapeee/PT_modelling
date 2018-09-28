#####################################
#Inhalt:  Mean R100 -- R500   	    #
#Datum: 	Januar 2018			          #
#Autor: 	mape			 	              #
#####################################
#Ziel: Wie sehr unterscheiden sich der Werte aller R100 in einer Zelle R500 vom Wert dieser Zelle.
#Vorgehen: Die Standardabweichung von R100 in einer Zelle R500 wird nicht ueber den Mittelwert aller R100
#bestimmt, sondern ueber den Wert von R500.

##--Import Packages--##
library(rgdal)
library(plyr)

##--Formeln--##
stdabw.ggs.geom500 <- function(x) {n=length(x) ; sqrt(sum(x)/n)} ##stdabw aus Grundgesamtheit

##--Berechnung--##
#1. Merge der Werte von R500 an die Werte von R100
AP.geom500erg <- merge(AP,AP.500,by.x="ID500",by.y="ID")
AP.geom500erg$IDVZelle.y <- NULL #loesche Spalte
AP.geom500erg$TYPENO.x <- NULL
AP.geom500erg$TYPENO.y <- NULL
names(AP.geom500erg) <- gsub("x", "100", names(AP.geom500erg)) #ersetze x duch 100
names(AP.geom500erg) <- gsub("y", "500", names(AP.geom500erg))
AP.geom500erg <- rename(AP.geom500erg, c("IDVZelle.500"="IDVZelle"))


#2. relative Differenz bilden
#Bilden der quadrierten Differenz beider Werte. Qaudrat um neg. Werte zu vermeiden.
#Das ist quasi der erste Schritt zur Varianz (die quadratische Abweichung vom Mittelwert aus R500)
AP.geom500erg$OEV_AP30.diff <- with(AP.geom500erg,(OEV_AP30.100-OEV_AP30.500)^2)
AP.geom500erg$Pkw_AP30.diff <- with(AP.geom500erg,(Pkw_AP30.100-Pkw_AP30.500)^2)
AP.geom500erg$OEV_AP60.diff <- with(AP.geom500erg,(OEV_AP60.100-OEV_AP60.500)^2)
AP.geom500erg$Pkw_AP60.diff <- with(AP.geom500erg,(Pkw_AP60.100-Pkw_AP60.500)^2)
AP.geom500erg$OEV_AP05.diff <- with(AP.geom500erg,(OEV_AP05.100-OEV_AP05.500)^2)
AP.geom500erg$Pkw_AP05.diff <- with(AP.geom500erg,(Pkw_AP05.100-Pkw_AP05.500)^2)
AP.geom500erg$Fuss_AP15.diff <- with(AP.geom500erg,(Fuss_AP15.100-Fuss_AP15.500)^2)
AP.geom500erg$Rad_AP15.diff <- with(AP.geom500erg,(Rad_AP15.100-Rad_AP15.500)^2)
AP.geom500erg$Fuss_AP30.diff <- with(AP.geom500erg,(Fuss_AP30.100-Fuss_AP30.500)^2)
AP.geom500erg$Rad_AP30.diff <- with(AP.geom500erg,(Rad_AP30.100-Rad_AP30.500)^2)
AP.geom500erg$Fuss_AP05.diff <- with(AP.geom500erg,(Fuss_AP05.100-Fuss_AP05.500)^2)
AP.geom500erg$Rad_AP05.diff <- with(AP.geom500erg,(Rad_AP05.100-Rad_AP05.500)^2)

#3. Standardabweichung und Mittelwert R100 je Zelle R500
#OEV
OEV_AP30.geom500erg <- merge(do.call(data.frame,aggregate(OEV_AP30.diff ~ ID500, AP.geom500erg,function(x) c(len = length(x), sd = stdabw.ggs.geom500(x)))),
                             do.call(data.frame,aggregate(OEV_AP30.100 ~ ID500, AP.geom500erg,function(x) c(mw = mean(x)))),by="ID500")
colnames(OEV_AP30.geom500erg) <- c("ID500","R100.len","OEV_AP30.geom500","OEV_AP30.mean100")
OEV_AP60.geom500erg <- merge(do.call(data.frame,aggregate(OEV_AP60.diff ~ ID500, AP.geom500erg,function(x) c(len = length(x), sd = stdabw.ggs.geom500(x)))),
                           do.call(data.frame,aggregate(OEV_AP60.100 ~ ID500, AP.geom500erg,function(x) c(mw = mean(x)))),by="ID500")
colnames(OEV_AP60.geom500erg) <- c("ID500","R100.len","OEV_AP60.geom500","OEV_AP60.mean100")
OEV_AP05.geom500erg <- merge(do.call(data.frame,aggregate(OEV_AP05.diff ~ ID500, AP.geom500erg,function(x) c(len = length(x), sd = stdabw.ggs.geom500(x)))),
                           do.call(data.frame,aggregate(OEV_AP05.100 ~ ID500, AP.geom500erg,function(x) c(mw = mean(x)))),by="ID500")
colnames(OEV_AP05.geom500erg) <- c("ID500","R100.len","OEV_AP05.geom500","OEV_AP05.mean100")

#Pkw
Pkw_AP30.geom500erg <- merge(do.call(data.frame,aggregate(Pkw_AP30.diff ~ ID500, AP.geom500erg,function(x) c(len = length(x), sd = stdabw.ggs.geom500(x)))),
                           do.call(data.frame,aggregate(Pkw_AP30.100 ~ ID500, AP.geom500erg,function(x) c(mw = mean(x)))),by="ID500")
colnames(Pkw_AP30.geom500erg) <- c("ID500","R100.len","Pkw_AP30.geom500","Pkw_AP30.mean100")
Pkw_AP60.geom500erg <- merge(do.call(data.frame,aggregate(Pkw_AP60.diff ~ ID500, AP.geom500erg,function(x) c(len = length(x), sd = stdabw.ggs.geom500(x)))),
                           do.call(data.frame,aggregate(Pkw_AP60.100 ~ ID500, AP.geom500erg,function(x) c(mw = mean(x)))),by="ID500")
colnames(Pkw_AP60.geom500erg) <- c("ID500","R100.len","Pkw_AP60.geom500","Pkw_AP60.mean100")
Pkw_AP05.geom500erg <- merge(do.call(data.frame,aggregate(Pkw_AP05.diff ~ ID500, AP.geom500erg,function(x) c(len = length(x), sd = stdabw.ggs.geom500(x)))),
                           do.call(data.frame,aggregate(Pkw_AP05.100 ~ ID500, AP.geom500erg,function(x) c(mw = mean(x)))),by="ID500")
colnames(Pkw_AP05.geom500erg) <- c("ID500","R100.len","Pkw_AP05.geom500","Pkw_AP05.mean100")
#Fuss
Fuss_AP30.geom500erg <- merge(do.call(data.frame,aggregate(Fuss_AP30.diff ~ ID500, AP.geom500erg,function(x) c(len = length(x), sd = stdabw.ggs.geom500(x)))),
                           do.call(data.frame,aggregate(Fuss_AP30.100 ~ ID500, AP.geom500erg,function(x) c(mw = mean(x)))),by="ID500")
colnames(Fuss_AP30.geom500erg) <- c("ID500","R100.len","Fuss_AP30.geom500","Fuss_AP30.mean100")
Fuss_AP15.geom500erg <- merge(do.call(data.frame,aggregate(Fuss_AP15.diff ~ ID500, AP.geom500erg,function(x) c(len = length(x), sd = stdabw.ggs.geom500(x)))),
                           do.call(data.frame,aggregate(Fuss_AP15.100 ~ ID500, AP.geom500erg,function(x) c(mw = mean(x)))),by="ID500")
colnames(Fuss_AP15.geom500erg) <- c("ID500","R100.len","Fuss_AP15.geom500","Fuss_AP15.mean100")
Fuss_AP05.geom500erg <- merge(do.call(data.frame,aggregate(Fuss_AP05.diff ~ ID500, AP.geom500erg,function(x) c(len = length(x), sd = stdabw.ggs.geom500(x)))),
                           do.call(data.frame,aggregate(Fuss_AP05.100 ~ ID500, AP.geom500erg,function(x) c(mw = mean(x)))),by="ID500")
colnames(Fuss_AP05.geom500erg) <- c("ID500","R100.len","Fuss_AP05.geom500","Fuss_AP05.mean100")
#Rad
Rad_AP30.geom500erg <- merge(do.call(data.frame,aggregate(Rad_AP30.diff ~ ID500, AP.geom500erg,function(x) c(len = length(x), sd = stdabw.ggs.geom500(x)))),
                            do.call(data.frame,aggregate(Rad_AP30.100 ~ ID500, AP.geom500erg,function(x) c(mw = mean(x)))),by="ID500")
colnames(Rad_AP30.geom500erg) <- c("ID500","R100.len","Rad_AP30.geom500","Rad_AP30.mean100")
Rad_AP15.geom500erg <- merge(do.call(data.frame,aggregate(Rad_AP15.diff ~ ID500, AP.geom500erg,function(x) c(len = length(x), sd = stdabw.ggs.geom500(x)))),
                            do.call(data.frame,aggregate(Rad_AP15.100 ~ ID500, AP.geom500erg,function(x) c(mw = mean(x)))),by="ID500")
colnames(Rad_AP15.geom500erg) <- c("ID500","R100.len","Rad_AP15.geom500","Rad_AP15.mean100")
Rad_AP05.geom500erg <- merge(do.call(data.frame,aggregate(Rad_AP05.diff ~ ID500, AP.geom500erg,function(x) c(len = length(x), sd = stdabw.ggs.geom500(x)))),
                            do.call(data.frame,aggregate(Rad_AP05.100 ~ ID500, AP.geom500erg,function(x) c(mw = mean(x)))),by="ID500")
colnames(Rad_AP05.geom500erg) <- c("ID500","R100.len","Rad_AP05.geom500","Rad_AP05.mean100")

###Mergen###
AP.geom500erg <- merge(OEV_AP30.geom500erg,Pkw_AP30.geom500erg)
AP.geom500erg <- merge(AP.geom500erg,OEV_AP60.geom500erg)
AP.geom500erg <- merge(AP.geom500erg,Pkw_AP60.geom500erg)
AP.geom500erg <- merge(AP.geom500erg,OEV_AP05.geom500erg)
AP.geom500erg <- merge(AP.geom500erg,Pkw_AP05.geom500erg)
AP.geom500erg <- merge(AP.geom500erg,Fuss_AP30.geom500erg)
AP.geom500erg <- merge(AP.geom500erg,Rad_AP30.geom500erg)
AP.geom500erg <- merge(AP.geom500erg,Fuss_AP15.geom500erg)
AP.geom500erg <- merge(AP.geom500erg,Rad_AP15.geom500erg)
AP.geom500erg <- merge(AP.geom500erg,Fuss_AP05.geom500erg)
AP.geom500erg <- merge(AP.geom500erg,Rad_AP05.geom500erg)
AP.geom500erg <- merge(AP.geom500erg,AP.500[,c("EW","ID")],by.x="ID500",by.y="ID") ##Einwohnerzahl anhaengen
remove(OEV_AP30.geom500erg,Pkw_AP30.geom500erg,Fuss_AP30.geom500erg,Rad_AP30.geom500erg,OEV_AP60.geom500erg,Pkw_AP60.geom500erg,
       OEV_AP05.geom500erg,Pkw_AP05.geom500erg,Fuss_AP15.geom500erg,Rad_AP15.geom500erg,Fuss_AP05.geom500erg,Rad_AP05.geom500erg)


#4. Varianzkoeffizient
AP.geom500erg$OEV_AP30.geom500.cv <- with(AP.geom500erg,OEV_AP30.geom500/OEV_AP30.mean100)
AP.geom500erg$Pkw_AP30.geom500.cv <- with(AP.geom500erg,Pkw_AP30.geom500/Pkw_AP30.mean100)
AP.geom500erg$OEV_AP60.geom500.cv <- with(AP.geom500erg,OEV_AP60.geom500/OEV_AP60.mean100)
AP.geom500erg$Pkw_AP60.geom500.cv <- with(AP.geom500erg,Pkw_AP60.geom500/Pkw_AP60.mean100)
AP.geom500erg$OEV_AP05.geom500.cv <- with(AP.geom500erg,OEV_AP05.geom500/OEV_AP05.mean100)
AP.geom500erg$Pkw_AP05.geom500.cv <- with(AP.geom500erg,Pkw_AP05.geom500/Pkw_AP05.mean100)
AP.geom500erg$Fuss_AP30.geom500.cv <- with(AP.geom500erg,Fuss_AP30.geom500/Fuss_AP30.mean100)
AP.geom500erg$Rad_AP30.geom500.cv <- with(AP.geom500erg,Rad_AP30.geom500/Rad_AP30.mean100)
AP.geom500erg$Fuss_AP15.geom500.cv <- with(AP.geom500erg,Fuss_AP15.geom500/Fuss_AP15.mean100)
AP.geom500erg$Rad_AP15.geom500.cv <- with(AP.geom500erg,Rad_AP15.geom500/Rad_AP15.mean100)
AP.geom500erg$Fuss_AP05.geom500.cv <- with(AP.geom500erg,Fuss_AP05.geom500/Fuss_AP05.mean100)
AP.geom500erg$Rad_AP05.geom500.cv <- with(AP.geom500erg,Rad_AP05.geom500/Rad_AP05.mean100)
AP.geom500erg[AP.geom500erg=="NA"] <- NA
AP.geom500erg[AP.geom500erg=="NaN"] <- 0 ##Entferne die NaN bei Division durch 0 im .cv
#inf bei Division durch 0. Wenn sd von R100 >0 und R500 = 0.
AP.geom500erg$OEV_AP30.geom500.cv <- ifelse(AP.geom500erg$OEV_AP30.geom500.cv==Inf,2,AP.geom500erg$OEV_AP30.geom500.cv) ##An dieser Stelle nur Krücke zur Visualisierung
AP.geom500erg$Pkw_AP30.geom500.cv <- ifelse(AP.geom500erg$Pkw_AP30.geom500.cv==Inf,2,AP.geom500erg$Pkw_AP30.geom500.cv)
AP.geom500erg$OEV_AP60.geom500.cv <- ifelse(AP.geom500erg$OEV_AP60.geom500.cv==Inf,2,AP.geom500erg$OEV_AP60.geom500.cv) ##An dieser Stelle nur Krücke zur Visualisierung
AP.geom500erg$Pkw_AP60.geom500.cv <- ifelse(AP.geom500erg$Pkw_AP60.geom500.cv==Inf,2,AP.geom500erg$Pkw_AP60.geom500.cv)
AP.geom500erg$OEV_AP05.geom500.cv <- ifelse(AP.geom500erg$OEV_AP05.geom500.cv==Inf,2,AP.geom500erg$OEV_AP05.geom500.cv) ##An dieser Stelle nur Krücke zur Visualisierung
AP.geom500erg$Pkw_AP05.geom500.cv <- ifelse(AP.geom500erg$Pkw_AP05.geom500.cv==Inf,2,AP.geom500erg$Pkw_AP05.geom500.cv)
AP.geom500erg$Fuss_AP30.geom500.cv <- ifelse(AP.geom500erg$Fuss_AP30.geom500.cv==Inf,2,AP.geom500erg$Fuss_AP30.geom500.cv) ##An dieser Stelle nur Krücke zur Visualisierung
AP.geom500erg$Rad_AP30.geom500.cv <- ifelse(AP.geom500erg$Rad_AP30.geom500.cv==Inf,2,AP.geom500erg$Rad_AP30.geom500.cv)
AP.geom500erg$Fuss_AP15.geom500.cv <- ifelse(AP.geom500erg$Fuss_AP15.geom500.cv==Inf,2,AP.geom500erg$Fuss_AP15.geom500.cv) ##An dieser Stelle nur Krücke zur Visualisierung
AP.geom500erg$Rad_AP15.geom500.cv <- ifelse(AP.geom500erg$Rad_AP15.geom500.cv==Inf,2,AP.geom500erg$Rad_AP15.geom500.cv)
AP.geom500erg$Fuss_AP05.geom500.cv <- ifelse(AP.geom500erg$Fuss_AP05.geom500.cv==Inf,2,AP.geom500erg$Fuss_AP05.geom500.cv) ##An dieser Stelle nur Krücke zur Visualisierung
AP.geom500erg$Rad_AP05.geom500.cv <- ifelse(AP.geom500erg$Rad_AP05.geom500.cv==Inf,2,AP.geom500erg$Rad_AP05.geom500.cv)
#Bereinigung Extremwerte und Besonderheiten
AP.geom500erg$OEV_AP30.geom500.cv <- ifelse(AP.geom500erg$OEV_AP30.geom500.cv>10,10,AP.geom500erg$OEV_AP30.geom500.cv)
AP.geom500erg$Pkw_AP30.geom500.cv <- ifelse(AP.geom500erg$Pkw_AP30.geom500.cv>10,10,AP.geom500erg$Pkw_AP30.geom500.cv)
AP.geom500erg$Fuss_AP30.geom500.cv <- ifelse(AP.geom500erg$Fuss_AP30.geom500.cv>10,10,AP.geom500erg$Fuss_AP30.geom500.cv)
AP.geom500erg$Rad_AP30.geom500.cv <- ifelse(AP.geom500erg$Rad_AP30.geom500.cv>10,10,AP.geom500erg$Rad_AP30.geom500.cv)
AP.geom500erg$OEV_AP60.geom500.cv <- ifelse(AP.geom500erg$OEV_AP60.geom500.cv>10,10,AP.geom500erg$OEV_AP60.geom500.cv)
AP.geom500erg$Pkw_AP60.geom500.cv <- ifelse(AP.geom500erg$Pkw_AP60.geom500.cv>10,10,AP.geom500erg$Pkw_AP60.geom500.cv)
AP.geom500erg$Fuss_AP15.geom500.cv <- ifelse(AP.geom500erg$Fuss_AP15.geom500.cv>10,10,AP.geom500erg$Fuss_AP15.geom500.cv)
AP.geom500erg$Rad_AP15.geom500.cv <- ifelse(AP.geom500erg$Rad_AP15.geom500.cv>10,10,AP.geom500erg$Rad_AP15.geom500.cv)
AP.geom500erg$OEV_AP05.geom500.cv <- ifelse(AP.geom500erg$OEV_AP05.geom500.cv>10,10,AP.geom500erg$OEV_AP05.geom500.cv)
AP.geom500erg$Pkw_AP05.geom500.cv <- ifelse(AP.geom500erg$Pkw_AP05.geom500.cv>10,10,AP.geom500erg$Pkw_AP05.geom500.cv)
AP.geom500erg$Fuss_AP05.geom500.cv <- ifelse(AP.geom500erg$Fuss_AP05.geom500.cv>10,10,AP.geom500erg$Fuss_AP05.geom500.cv)
AP.geom500erg$Rad_AP05.geom500.cv <- ifelse(AP.geom500erg$Rad_AP05.geom500.cv>10,10,AP.geom500erg$Rad_AP05.geom500.cv)
#5. Kategorisierung
#OEV
AP.geom500erg$OEV_AP30.ctg <- cut(AP.geom500erg$OEV_AP30.geom500.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.geom500erg$OEV_AP30.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.geom500erg$OEV_AP30.ctg[AP.geom500erg$R100.len<2] <- 'missing'
#Pkw
AP.geom500erg$Pkw_AP30.ctg <- cut(AP.geom500erg$Pkw_AP30.geom500.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.geom500erg$Pkw_AP30.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.geom500erg$Pkw_AP30.ctg[AP.geom500erg$R100.len<2] <- 'missing'
#Fuss
AP.geom500erg$Fuss_AP30.ctg <- cut(AP.geom500erg$Fuss_AP30.geom500.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.geom500erg$Fuss_AP30.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.geom500erg$Fuss_AP30.ctg[AP.geom500erg$R100.len<2] <- 'missing'
#Rad
AP.geom500erg$Rad_AP30.ctg <- cut(AP.geom500erg$Rad_AP30.geom500.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.geom500erg$Rad_AP30.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.geom500erg$Rad_AP30.ctg[AP.geom500erg$R100.len<2] <- 'missing'

#OEV
AP.geom500erg$OEV_AP60.ctg <- cut(AP.geom500erg$OEV_AP60.geom500.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.geom500erg$OEV_AP60.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.geom500erg$OEV_AP60.ctg[AP.geom500erg$R100.len<2] <- 'missing'
#Pkw
AP.geom500erg$Pkw_AP60.ctg <- cut(AP.geom500erg$Pkw_AP60.geom500.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.geom500erg$Pkw_AP60.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.geom500erg$Pkw_AP60.ctg[AP.geom500erg$R100.len<2] <- 'missing'
#Fuss
AP.geom500erg$Fuss_AP15.ctg <- cut(AP.geom500erg$Fuss_AP15.geom500.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.geom500erg$Fuss_AP15.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.geom500erg$Fuss_AP15.ctg[AP.geom500erg$R100.len<2] <- 'missing'
#Rad
AP.geom500erg$Rad_AP15.ctg <- cut(AP.geom500erg$Rad_AP15.geom500.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.geom500erg$Rad_AP15.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.geom500erg$Rad_AP15.ctg[AP.geom500erg$R100.len<2] <- 'missing'

#OEV
AP.geom500erg$OEV_AP05.ctg <- cut(AP.geom500erg$OEV_AP05.geom500.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.geom500erg$OEV_AP05.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.geom500erg$OEV_AP05.ctg[AP.geom500erg$R100.len<2] <- 'missing'
#Pkw
AP.geom500erg$Pkw_AP05.ctg <- cut(AP.geom500erg$Pkw_AP05.geom500.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.geom500erg$Pkw_AP05.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.geom500erg$Pkw_AP05.ctg[AP.geom500erg$R100.len<2] <- 'missing'
#Fuss
AP.geom500erg$Fuss_AP05.ctg <- cut(AP.geom500erg$Fuss_AP05.geom500.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.geom500erg$Fuss_AP05.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.geom500erg$Fuss_AP05.ctg[AP.geom500erg$R100.len<2] <- 'missing'
#Rad
AP.geom500erg$Rad_AP05.ctg <- cut(AP.geom500erg$Rad_AP05.geom500.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.geom500erg$Rad_AP05.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.geom500erg$Rad_AP05.ctg[AP.geom500erg$R100.len<2] <- 'missing'
