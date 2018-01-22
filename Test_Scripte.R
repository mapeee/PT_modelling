#####################################
#Inhalt:  Test Scripte  	          #
#Datum: 	Januar 2018			          #
#Autor: 	mape			 	              #
#####################################
##graphics.off()
##dev.off()


##--Import Packages--##
library(rgdal)


##--Verbindung zu Geodaten--##
gdb <- "C:/Geodaten/Material.gdb" #Verbindung zur gdb
Raster100 <- as.data.frame(readOGR(dsn=gdb,layer="MRH_EW_ha")) ##Schreibe Attriubuttabelle in Data-Frame
print(colnames(Raster100)) ##Ausgabe der Spaltennamen

# Gemeinden <- readOGR(dsn=gdb,layer="MRH_Gemeinden_2017")
print(names(Gemeinden)) ##Namen der Attributspalten


##--Ausgaben--##
# plot(fc[1:100,]) #nur die ersten 100 Zeilen
# plot.ts(fcdata[2]) #nur die zweite Spalte
# plot.ts(fcdata[1:300,2]) #ersten 300 Zeilen der zweiten Spalte


##--Group-by Statements--##
# a <- aggregate(EW ~ IDGem, Raster100, sum) ##Sum EW ueber IDGEM von Data Raster500
# plot.ts(a["EW"])

##--Slicing--##
# Ausw_AP100[Ausw_AP100$IDGem == 1054,] ##Nur Zeilen, wo Spalteninhalt == 1054

##--Joins--##
Ausw_AP100 <- merge(AP100[c("ID","Pkw_AP30")],Raster100[c("ID","EW","ID500","IDGem")],by="ID")

##--Auswertungen--##
AP30Gem_sd <- merge(aggregate(Pkw_AP30 ~ IDGem, Ausw_AP100, sd),
                    aggregate(EW ~ IDGem, Ausw_AP100, sum))
AP30Gem_sd[AP30Gem_sd=="NA"] <- NA ##'NA' durch NA, dann ersetzen
AP30Gem_sd[is.na(AP30Gem_sd)] <- 0
colnames(AP30Gem_sd)[2] <- "Pkw_AP30_sd" ##Spalte umbenennen

plot(AP30Gem_sd$EW,AP30Gem_sd$Pkw_AP30_sd,xlim=range(0:10000)) ##Zeichnen bis 10.000
abline(lm(formula = AP30Gem_sd$EW ~ AP30Gem_sd$Pkw_AP30_sd)) ##Regressionslinie
text(cor(AP30Gem_sd$EW, AP30Gem_sd$Pkw_AP30_sd),x=5000,y=250000)


# newobj <- merge(Gemeinden, AP30Gem_sd, by.x="ID", by.y="IDGem")
# plot(newobj, col = gray(newobj$Pkw_AP30_sd/max(newobj$Pkw_AP30_sd)),main ='Testkarte')
# text(newobj, newobj$GF, cex=0.75)
# plot(newobj, col = terrain.colors(20), axes=FALSE, main ='Testkarte')

##--Ausgabe--##
# write.xlsx(OEVAP30[OEVAP30$IDVZelle==203,], "C:\\Users\\mape\\Desktop\\test.xlsx")
