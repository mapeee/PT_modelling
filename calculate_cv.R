#####################################
#Inhalt:  calculate cv-values       #
#Datum: 	Januar 2018			          #
#Autor: 	mape			 	              #
#####################################

calculate.cv <- function(par1,par2,par3){
  round(mean((data.frame(par1[par1[,par3]!='missing',])[par2])[[1]]),3)
}

#OEV
calculate.cv(AP.erg,"OEV_AP30.cv","OEV_AP30.ctg")
calculate.cv(AP.erg,"OEV_AP60.cv","OEV_AP60.ctg")
calculate.cv(AP.erg,"OEV_AP05.cv","OEV_AP05.ctg")
calculate.cv(AP.erg500,"OEV_AP30.cv","OEV_AP30.ctg")
calculate.cv(AP.erg500,"OEV_AP60.cv","OEV_AP60.ctg")
calculate.cv(AP.erg500,"OEV_AP05.cv","OEV_AP05.ctg")
calculate.cv(AP.geom500erg,"OEV_AP30.geom500.cv","OEV_AP30.ctg")
calculate.cv(AP.geom500erg,"OEV_AP60.geom500.cv","OEV_AP60.ctg")
calculate.cv(AP.geom500erg,"OEV_AP05.geom500.cv","OEV_AP05.ctg")

#MIV
calculate.cv(AP.erg,"Pkw_AP30.cv","Pkw_AP30.ctg")
calculate.cv(AP.erg,"Pkw_AP60.cv","Pkw_AP60.ctg")
calculate.cv(AP.erg,"Pkw_AP05.cv","Pkw_AP05.ctg")
calculate.cv(AP.erg500,"Pkw_AP30.cv","Pkw_AP30.ctg")
calculate.cv(AP.erg500,"Pkw_AP60.cv","Pkw_AP60.ctg")
calculate.cv(AP.erg500,"Pkw_AP05.cv","Pkw_AP05.ctg")
calculate.cv(AP.geom500erg,"Pkw_AP30.geom500.cv","Pkw_AP30.ctg")
calculate.cv(AP.geom500erg,"Pkw_AP60.geom500.cv","Pkw_AP60.ctg")
calculate.cv(AP.geom500erg,"Pkw_AP05.geom500.cv","Pkw_AP05.ctg")

#Rad
calculate.cv(AP.erg,"Rad_AP15.cv","Rad_AP15.ctg")
calculate.cv(AP.erg,"Rad_AP30.cv","Rad_AP30.ctg")
calculate.cv(AP.erg,"Rad_AP05.cv","Rad_AP05.ctg")
calculate.cv(AP.erg500,"Rad_AP15.cv","Rad_AP15.ctg")
calculate.cv(AP.erg500,"Rad_AP30.cv","Rad_AP30.ctg")
calculate.cv(AP.erg500,"Rad_AP05.cv","Rad_AP05.ctg")
calculate.cv(AP.geom500erg,"Rad_AP15.geom500.cv","Rad_AP15.ctg")
calculate.cv(AP.geom500erg,"Rad_AP30.geom500.cv","Rad_AP30.ctg")
calculate.cv(AP.geom500erg,"Rad_AP05.geom500.cv","Rad_AP05.ctg")

#Fuss
calculate.cv(AP.erg,"Fuss_AP15.cv","Fuss_AP15.ctg")
calculate.cv(AP.erg,"Fuss_AP30.cv","Fuss_AP30.ctg")
calculate.cv(AP.erg,"Fuss_AP05.cv","Fuss_AP05.ctg")
calculate.cv(AP.erg500,"Fuss_AP15.cv","Fuss_AP15.ctg")
calculate.cv(AP.erg500,"Fuss_AP30.cv","Fuss_AP30.ctg")
calculate.cv(AP.erg500,"Fuss_AP05.cv","Fuss_AP05.ctg")
calculate.cv(AP.geom500erg,"Fuss_AP15.geom500.cv","Fuss_AP15.ctg")
calculate.cv(AP.geom500erg,"Fuss_AP30.geom500.cv","Fuss_AP30.ctg")
calculate.cv(AP.geom500erg,"Fuss_AP05.geom500.cv","Fuss_AP05.ctg")