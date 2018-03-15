#####################################
#Inhalt:  Visual-Function 	        #
#Datum: 	Januar 2018			          #
#Autor: 	mape			 	              #
#####################################

##Parameter
#par1 = Datentabelle
#par2 = Obergrenze der Darstellung
#par3 = Titel
#par4 = Indikator
#par5 = Mit Einwohnern
#par6 = Gebietstyp
  
Visual.mm <- function(par1,par2,par3,par4,par5,par6,par7){
  if(par6>0){par1 <- par1[par1$VZTyp==par6,]}
  
  bars <- barplot(summary(par1[,par7]),
                  # main = par3,
                  ylim = c(0,max(summary(par1[,par7]))*1.2),
                  xaxt = 'n',
                  yaxt='n',
                  xpd = T,
                  col=Farbe,
                  cex.names = 0.8);
  axis(1,at=seq(0.7,length(levels(par1[,par7]))*1.2,1.2),labels=levels(par1[,par7]),cex.axis=0.9);
  title(xlab = "cv",line = 2, font.lab = 2);
  title(par3, line = 1);
  axis(side=2,line = -0.4);
  mtext("n", side=2,cex=0.7,font=2, line=1.5)
  xlim0 <- par()$usr[1:2];
  text(par("usr")[2]-1,par("usr")[4],adj = c( 1, 1 ),
       labels = paste("n =",as.character(nrow(par1))),
       cex=1);
  text(par("usr")[2]-1,par("usr")[4],adj = c( 1, 2.5 ),
       labels = paste("'missings' =",as.character(sum(par1[,par7]=='missing'))),
       cex=1);
  text(par("usr")[2]-1,par("usr")[4],adj = c( 1, 4 ),
       labels = paste("mean cv =",
                      as.character(round(mean((data.frame(par1[par1[,par7]!='missing',])[par4])[[1]]),3))),
       cex=1);
  abline(v=1.3, col="royalblue4",lwd=2);
  abline(v=6.1, col="royalblue4",lwd=2,lty=2)
  if(par5==T){
    EW = tapply(par1$EW,par1[,par7],sum)
    EW[is.na(EW)]<-0
    par(new = T)
    plot.new()
    plot.window(xlim = xlim0, ylim = c(0,max(EW)+200000), xaxs = "i",yaxs="i",col.lab="darkred")
    points(EW ~ bars, col = "darkred",cex=1.4,pch=17)
    axis(side=4,col="darkred",col.axis="darkred",line = -0.4,
         at=c(0,250000,500000,max(EW)),
         labels=c("0","250","500",round(max(EW)/1000,0)))
    rug(x = c(125000,375000), ticksize = -0.02, side = 4,line = -0.4)
    mtext("Population (k)", side=4,cex=0.7,font=2, line=1.6, col="darkred")
  }
}


Visual.karte <- function(par1,par2,par3,par4,par5){
  #--Daten verschneiden und vorbereiten--#
  Area <- merge(VZellen, par1, by.x="NO", by.y="IDVZelle") ##VZ ohne Werte bekommen Wert 0
  Area$ctg <- cut(Area@data[,par2],breaks = breaks.ctg,labels = labels.ctg)
  levels(Area@data$ctg) = append(labels.ctg,"missing") ##Füge zusätzliche Kategorie ein, um anschl. missings vergeben zu können
  Area@data$ctg[Area@data[,par3]<par4] <- 'missing'
  
  #--Karte plotten--#
  if(par5>0){plot(Area[Area$TYPENO==par5,],col=Farbe[Area@data$ctg],border=NA);
    legend("right", legend=levels(Area@data$ctg),fill=Farbe,cex=0.65,box.col = "grey")
    }
  else{plot(Area,col=Farbe[Area@data$ctg],border=NA);
    legend("right", legend=levels(Area@data$ctg),fill=Farbe,cex=0.65,box.col = "grey")}
}
