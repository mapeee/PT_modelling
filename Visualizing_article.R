##############################################
#Inhalt:  Visualisierung Applied Mobilities  #
#Datum: 	Maerz 2018			                   #
#Autor: 	mape			 	                       #
##############################################


##--Import Packages--##
library(rgdal)
library(ggplot2)


#Druckbereich
dev.off()
par(mfrow=c(4,1)) ##zwei Spalten, zwei Zeilen
par(mar=c(3.5,2.2,3.5,4.0)) ##Keine Raender
par(mfrow=c(2,1)) ##zwei Spalten, zwei Zeilen

#--Charts--#
#1
dev.off()
par(mfrow=c(4,1))
par(mar=c(3.5,2.2,3.5,4.0)) ##Keine Raender
Visual.mm(AP.erg,600,"(A) PT Jobs 30min TAZ","OEV_AP30.cv",T,0,"OEV_AP30.ctg")
Visual.mm(AP.erg,2000,"(B) Car Jobs 30min TAZ","Pkw_AP30.cv",T,0,"Pkw_AP30.ctg")
Visual.mm(AP.erg,500,"(C) Walk Jobs 30min TAZ","Fuss_AP30.cv",T,0,"Fuss_AP30.ctg")
Visual.mm(AP.erg,500,"(D) Bike Jobs 30min TAZ","Rad_AP30.cv",T,0,"Rad_AP30.ctg")
#2
dev.off()
par(mfrow=c(4,1))
par(mar=c(3.5,2.2,3.5,4.0)) ##Keine Raender
Visual.mm(AP.erg500,15000,"(E) PT Jobs 30min R500","OEV_AP30.cv",T,0,"OEV_AP30.ctg")
Visual.mm(AP.erg500,15000,"(F) PT Jobs 60min R500","OEV_AP60.cv",T,0,"OEV_AP60.ctg")
Visual.mm(AP.erg500,15000,"(H) Walk Jobs 15min R500","Fuss_AP15.cv",T,0,"Fuss_AP15.ctg")
Visual.mm(AP.erg500,15000,"(G) Walk Jobs 30min R500","Fuss_AP30.cv",T,0,"Fuss_AP30.ctg")



#--Abbildung ???--#
visual.cv <- function(par1,par2){
  plot(par1/1000, par2/1000, main="Coefficiants of Variation (Example)",
       xlab="",xaxt="n",
       ylab="",yaxt="n",
       ylim = c(0,500),
       xpd = F,pch=19,
       col = "firebrick2")
  title(ylab = "Standard Deviation",line = 2.4, font.lab = 2, cex.lab=0.8)
  title(xlab = "Worplaces within 60 minutes in PT (thousand)",line = 2.2, font.lab = 2, cex.lab=0.8)
  axis(side=1, font.lab=2,cex.axis=0.8)
  axis(side=2, font.lab=2, las=2,cex.axis=0.8)
  axis(side=4, labels=F)
  axis(side=3, labels=F)
  
  lines(par1/1000,0.1*par1/1000, 
        col="dodgerblue3",
        lwd = 2)
  text(max(par1)/1000-50,0.1*max(par1)/1000+20, 
       labels = c("cv=0.1"),
       font=2,
       cex = 0.9)
  
  lines(par1/1000,0.5*par1/1000, 
        col="dodgerblue3",
        lwd = 2)
  text(800,450, 
       labels = c("cv=0.5"),
       font=2,
       cex = 0.9)
  
  lines(par1/1000,par1/1000, 
        col="dodgerblue3",
        lwd = 2)
  text(350,450, 
       labels = c("cv=1.0"),
       font=2,
       cex = 0.9)
}


