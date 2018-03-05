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

#--Drucken--#
#Scatterplot CV
dev.off()
par(mfrow=c(1,1)) ##zwei Spalten, zwei Zeilen
par(mar=c(4.5,4.5,4.5,4.5)) ##Keine Raender
par()
visual.cv(OEVAP60.erg$OEV_AP60.mean,OEVAP60.erg$OEV_AP60.sd)
