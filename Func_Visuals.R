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


Visual.mm.g <- function(par1,par2,par3,par4,par5,par6){
    barplot(table(subset(par1,(TYPENO==par6&ctg!='missing'))$ctg)[levels(par2)],
            names.arg = levels(par1),
            main = par4,
            ylim = c(0,par3),
            # xaxt = 'n',
            # yaxt='n',
            xlab = "Kategorie",
            xpd = FALSE,
            col=Farbe,
            cex.names = 0.8);
  text(par("usr")[2],par("usr")[4],adj = c( 1, 1 ),
       labels = paste("n =",as.character(nrow(subset(par1,TYPENO==par6)))),
       cex=1)
  text(par("usr")[2],par("usr")[4],adj = c( 1, 2 ),
       paste("'missings' =",as.character(sum(subset(par1,TYPENO==par6)$ctg=='missing'))),
       cex=1)
  text(par("usr")[2],par("usr")[4],adj = c( 1, 3 ),
       paste("mean cv =",
             as.character(round(mean((data.frame(par1[par1$ctg!='missing'&par1$TYPENO==par6,])[par5])[[1]]),3))),
                          cex=1)
  }
   
 
Visual.mm <- function(par1,par2,par3,par4,par5){
  barplot(table(par1[par1$ctg!='missing',]$ctg)[levels(par2)],
            names.arg = levels(par1),
            main = par4,
            ylim = c(0,par3),
            # xaxt = 'n',
            # yaxt='n',
            xlab = "Kategorie",
            xpd = FALSE,
            col=Farbe,
            cex.names = 0.8); 
  text(par("usr")[2],par("usr")[4],adj = c( 1, 1 ),
     labels = paste("n =",as.character(nrow(par1))),
     cex=1);
  text(par("usr")[2],par("usr")[4],adj = c( 1, 2 ),
     labels = paste("'missings' =",as.character(sum(par1$ctg=='missing'))),
     cex=1);
  text(par("usr")[2],par("usr")[4],adj = c( 1, 3 ),
     labels = paste("mean cv =",
                    as.character(round(mean((data.frame(par1[par1$ctg!='missing',])[par5])[[1]]),3))),
     cex=1)
  }
