#####################################
#Inhalt:  Visual-Function 	        #
#Datum: 	Januar 2018			          #
#Autor: 	mape			 	              #
#####################################

##Parameter
#par1 = Datentabelle
#par2 = Kategorien
#par3 = Obergrenze der Darstellung
#par4 = Titel
#par5 = Indikator
#par6 = Gebietstyp
#par7 = Mit Einwohnern


Visual.mm.g <- function(par1,par2,par3,par4,par5,par6,par7){
    bars <- barplot(table(subset(par1,(VZTyp==par6&ctg!='missing'))$ctg)[levels(par2)],
            ylim = c(0,par3),
            xaxt = 'n',
            yaxt='n',
            xpd = FALSE,
            col=Farbe,
            cex.names = 0.8);
  axis(side=2,line = -0.4);
  mtext("n", side=2,cex=0.7,font=2, line=1.5)
  axis(1,at=seq(0.7,18,1.2),labels=levels(par2),cex.axis=0.9);
  title(xlab = "cv",line = 2, font.lab = 2);
  title(par4, line = 1);
  xlim0 <- par()$usr[1:2];
  ylim0 <- c(0,1000000)
  text(par("usr")[2],par("usr")[4],adj = c( 1, 1 ),
       labels = paste("n =",as.character(nrow(subset(par1,VZTyp==par6)))),
       cex=1)
  text(par("usr")[2],par("usr")[4],adj = c( 1, 2 ),
       paste("'missings' =",as.character(sum(subset(par1,VZTyp==par6)$ctg=='missing'))),
       cex=1)
  text(par("usr")[2],par("usr")[4],adj = c( 1, 3 ),
       paste("mean cv =",
             as.character(round(mean((data.frame(par1[par1$ctg!='missing'&par1$VZTyp==par6,])[par5])[[1]]),3))),
                          cex=1)
  if(par7==T){
    EW = aggregate(EW ~ ctg, subset(par1,(VZTyp==par6&ctg!='missing')),sum) ##aufsummieren de rEinwohner ueber ctg
    EW[length(EW$ctg)+1,] <- EW[1,] ##neue Sortierung, kruecke!!
    EW <- EW[-1,]
    # text(x=seq(0.7,18,1.2),y=werte+200,label=EW$EW,font=2)
    par(new = T)
    plot.new()
    plot.window(xlim = xlim0, ylim = ylim0, xaxs = "i",yaxs="i",col.lab="darkred")
    points(EW$EW ~ bars, col = "darkred",cex=1.1,pch=17)
    axis(side=4,col="darkred",col.axis="darkred",line = -0.4,
         at=c(0,250000,500000,750000),
         labels=c("0","250","500","750"))
    rug(x = c(125000,375000,625000), ticksize = -0.02, side = 4,line = -0.4)
    mtext("Einwohner in Tsd.", side=4,cex=0.7,font=2, line=1.6, col="darkred")
    
    # text(par("usr")[2],par("usr")[4],adj = c( 1, 4 ),
    #      paste("Einwohner =",as.character(sum(EW$EW))),
    #      cex=1) 
  }
}
  
Visual.mm <- function(par1,par2,par3,par4,par5,par6){
  werte <- data.frame(table(par1[par1$ctg!='missing',]$ctg)[levels(par2)]);
  werte["Var1"] <- levels(par2)
  werte$Freq <- ifelse(is.na(werte$Freq),0, werte$Freq)
  
  if(par6==T){
    EW = aggregate(EW ~ ctg, par1[par1$ctg!='missing',],sum)
    EW <- merge(werte,EW,by.x="Var1",by.y="ctg",all.x = T)
    EW$EW <- ifelse(is.na(EW$EW),0, EW$EW)
  }
  
  data=werte$Freq
  names(data)=werte$Var1
  
  
  bars <- barplot(data,
            # main = par4,
            ylim = c(0,par3),
            xaxt = 'n',
            yaxt='n',
            xpd = T,
            col=Farbe,
            cex.names = 0.8);
  axis(1,at=seq(0.7,18,1.2),labels=levels(par2),cex.axis=0.9);
  title(xlab = "cv",line = 2, font.lab = 2);
  title(par4, line = 1);
  axis(side=2,line = -0.4);
  mtext("n", side=2,cex=0.7,font=2, line=1.5)
  xlim0 <- par()$usr[1:2];
  text(par("usr")[2]-1,par("usr")[4],adj = c( 1, 1 ),
     labels = paste("n =",as.character(nrow(par1))),
     cex=1);
  text(par("usr")[2]-1,par("usr")[4],adj = c( 1, 2 ),
     labels = paste("'missings' =",as.character(sum(par1$ctg=='missing'))),
     cex=1);
  text(par("usr")[2]-1,par("usr")[4],adj = c( 1, 3 ),
     labels = paste("mean cv =",
                    as.character(round(mean((data.frame(par1[par1$ctg!='missing',])[par5])[[1]]),3))),
     cex=1)
  if(par6==T){
    EW[length(EW$EW)+1,] <- EW[1,] ##neue Sortierung, kruecke!!
    EW <- EW[-1,]
    # text(x=seq(0.7,18,1.2),y=werte+200,label=EW$EW,font=2)
    par(new = T)
    plot.new()
    plot.window(xlim = xlim0, ylim = c(0,max(EW$EW)+200000), xaxs = "i",yaxs="i",col.lab="darkred")
    points(EW$EW ~ bars, col = "darkred",cex=1.1,pch=17)
    axis(side=4,col="darkred",col.axis="darkred",line = -0.4,
         at=c(0,250000,500000,750000),
         labels=c("0","250","500","750"))
    rug(x = c(125000,375000,625000), ticksize = -0.02, side = 4,line = -0.4)
    mtext("Einwohner in Tsd.", side=4,cex=0.7,font=2, line=1.6, col="darkred")
  }
}

