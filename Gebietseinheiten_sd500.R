#####################################
#Inhalt:  Mean R100 -- R500   	    #
#Datum: 	Januar 2018			          #
#Autor: 	mape			 	              #
#####################################
#Ziel: Wie sehr unterscheiden sich der Mittelwert aller R100 in einer Zelle R500 vom Wert dieser Zelle.
#Vorgehen: Die Standardabweichung von R100 in einer Zelle R500 wird nicht ueber den Mittelwert aller R100
#bestimmt, sondern ueber den Wert von R500.

##--Import Packages--##
library(rgdal)

##--Formeln--##
stdabw.ggs.sd500 <- function(x) {n=length(x) ; sqrt(sum(x)/n)} ##stdabw aus Grundgesamtheit

##--Berechnung--##
#1. Merge der Werte von R500 an die Werte von R100
AP.sd500erg <- merge(AP[,c("ID500","IDVZelle","ID","OEV_AP30","Pkw_AP30","OEV_AP60","Pkw_AP60","Fuss_AP30","Rad_AP30","EW")],AP.500,by.x="ID500",by.y="ID")
AP.sd500erg$IDVZelle.y <- NULL #loesche Spalte
AP.sd500erg$TYPENO <- NULL
colnames(AP.sd500erg) <- c("ID500","IDVZelle","ID100","OEV_AP30.100",
                          "Pkw_AP30.100","OEV_AP60.100","Pkw_AP60.100","Fuss_AP30.100","Rad_AP30.100","EW.100",
                          "OEV_AP30.500","Pkw_AP30.500","OEV_AP60.500","Pkw_AP60.500","Fuss_AP30.500","Rad_AP30.500","EW.500")

#2. relative Differenz bilden
#Bilden der quadrierten Differenz beider Werte. Qaudrat um neg. Werte zu vermeiden.
#Das ist quasi der erste Schritt zur Varianz (die quadratische Abweichung vom Mittelwert aus R500)
AP.sd500erg$OEVAP30.diff <- with(AP.sd500erg,(OEV_AP30.100-OEV_AP30.500)^2)
AP.sd500erg$PkwAP30.diff <- with(AP.sd500erg,(Pkw_AP30.100-Pkw_AP30.500)^2)
AP.sd500erg$OEVAP60.diff <- with(AP.sd500erg,(OEV_AP60.100-OEV_AP60.500)^2)
AP.sd500erg$PkwAP60.diff <- with(AP.sd500erg,(Pkw_AP60.100-Pkw_AP60.500)^2)
AP.sd500erg$FussAP30.diff <- with(AP.sd500erg,(Fuss_AP30.100-Fuss_AP30.500)^2)
AP.sd500erg$RadAP30.diff <- with(AP.sd500erg,(Rad_AP30.100-Rad_AP30.500)^2)

#3. Standardabweichung und Mittelwert R100 je Zelle R500
#OEV
OEVAP30.sd500erg <- merge(do.call(data.frame,aggregate(OEVAP30.diff ~ ID500, AP.sd500erg,function(x) c(len = length(x), sd = stdabw.ggs.sd500(x)))),
                            AP.500[,c("OEV_AP30","ID","EW")],by.x="ID500",by.y="ID") #Len behalten, da fuer alle Auswertungen wichtig.
colnames(OEVAP30.sd500erg) <- c("ID500","R100.len","OEVAP30.sd500","OEVAP30.mean500","EW")
#Pkw
PkwAP30.sd500erg <- merge(aggregate(PkwAP30.diff ~ ID500, AP.sd500erg,stdabw.ggs.sd500),
                          AP.500[,c("Pkw_AP30","ID","EW")],by.x="ID500",by.y="ID")
colnames(PkwAP30.sd500erg) <- c("ID500","PkwAP30.sd500","PkwAP30.mean500","EW")
#Fuss
FussAP30.sd500erg <- merge(aggregate(FussAP30.diff ~ ID500, AP.sd500erg,stdabw.ggs.sd500),
                          AP.500[,c("Fuss_AP30","ID","EW")],by.x="ID500",by.y="ID")
colnames(FussAP30.sd500erg) <- c("ID500","FussAP30.sd500","FussAP30.mean500","EW")
#Rad
RadAP30.sd500erg <- merge(aggregate(RadAP30.diff ~ ID500, AP.sd500erg,stdabw.ggs.sd500),
                          AP.500[,c("Rad_AP30","ID","EW")],by.x="ID500",by.y="ID")
colnames(RadAP30.sd500erg) <- c("ID500","RadAP30.sd500","RadAP30.mean500","EW")

AP.sd500erg <- merge(OEVAP30.sd500erg,PkwAP30.sd500erg)
AP.sd500erg <- merge(AP.sd500erg,FussAP30.sd500erg)
AP.sd500erg <- merge(AP.sd500erg,RadAP30.sd500erg)
remove(OEVAP30.sd500erg,PkwAP30.sd500erg,FussAP30.sd500erg,RadAP30.sd500erg)


#4. Varianzkoeffizient
AP.sd500erg$OEVAP30.sd500.cv <- with(AP.sd500erg,OEVAP30.sd500/OEVAP30.mean500)
AP.sd500erg$PkwAP30.sd500.cv <- with(AP.sd500erg,PkwAP30.sd500/PkwAP30.mean500)
AP.sd500erg$FussAP30.sd500.cv <- with(AP.sd500erg,FussAP30.sd500/FussAP30.mean500)
AP.sd500erg$RadAP30.sd500.cv <- with(AP.sd500erg,RadAP30.sd500/RadAP30.mean500)
AP.sd500erg[AP.sd500erg=="NA"] <- NA
AP.sd500erg[AP.sd500erg=="NaN"] <- 0 ##Entferne die NaN bei Division durch 0 im .cv
#inf bei Division durch 0. Wenn sd von R100 >0 und R500 = 0.
AP.sd500erg$OEVAP30.sd500.cv <- ifelse(AP.sd500erg$OEVAP30.sd500.cv==Inf,2,AP.sd500erg$OEVAP30.sd500.cv) ##An dieser Stelle nur Krücke zur Visualisierung
AP.sd500erg$PkwAP30.sd500.cv <- ifelse(AP.sd500erg$PkwAP30.sd500.cv==Inf,2,AP.sd500erg$PkwAP30.sd500.cv)
AP.sd500erg$FussAP30.sd500.cv <- ifelse(AP.sd500erg$FussAP30.sd500.cv==Inf,2,AP.sd500erg$FussAP30.sd500.cv)
AP.sd500erg$RadAP30.sd500.cv <- ifelse(AP.sd500erg$RadAP30.sd500.cv==Inf,2,AP.sd500erg$RadAP30.sd500.cv)
#Bereinigung Extremwerte und Besonderheiten
AP.sd500erg$OEVAP30.sd500.cv <- ifelse(AP.sd500erg$OEVAP30.sd500.cv>10,10,AP.sd500erg$OEVAP30.sd500.cv)
AP.sd500erg$PkwAP30.sd500.cv <- ifelse(AP.sd500erg$PkwAP30.sd500.cv>10,10,AP.sd500erg$PkwAP30.sd500.cv)
AP.sd500erg$FussAP30.sd500.cv <- ifelse(AP.sd500erg$FussAP30.sd500.cv>10,10,AP.sd500erg$FussAP30.sd500.cv)
AP.sd500erg$RadAP30.sd500.cv <- ifelse(AP.sd500erg$RadAP30.sd500.cv>10,10,AP.sd500erg$RadAP30.sd500.cv)
#5. Kategorisierung
#OEV
AP.sd500erg$OEVAP30.ctg <- cut(AP.sd500erg$OEVAP30.sd500.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.sd500erg$OEVAP30.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.sd500erg$OEVAP30.ctg[AP.sd500erg$R100.len<2] <- 'missing'
#Pkw
AP.sd500erg$PkwAP30.ctg <- cut(AP.sd500erg$PkwAP30.sd500.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.sd500erg$PkwAP30.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.sd500erg$PkwAP30.ctg[AP.sd500erg$R100.len<2] <- 'missing'
#Fuss
AP.sd500erg$FussAP30.ctg <- cut(AP.sd500erg$FussAP30.sd500.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.sd500erg$FussAP30.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.sd500erg$FussAP30.ctg[AP.sd500erg$R100.len<2] <- 'missing'
#Rad
AP.sd500erg$RadAP30.ctg <- cut(AP.sd500erg$RadAP30.sd500.cv,breaks = breaks.ctg,labels = labels.ctg)
levels(AP.sd500erg$RadAP30.ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
AP.sd500erg$RadAP30.ctg[AP.sd500erg$R100.len<2] <- 'missing'
