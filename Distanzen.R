###############################################
#Inhalt:  Reisezeit, Weite und Luftlinie MIV  #
#Datum: 	Januar 2018			                    #
#Autor: 	mape			 	                        #
###############################################

E.Arzt.dis <- E.Arzt[c("ID","Minuten_Pkw","Meter_Pkw","Luftlinie")]
E.Arzt.dis$Minuten_Pkw = E.Arzt.dis$Minuten_Pkw-5
# E.Arzt.dis[E.Arzt.dis$Minuten_Pkw<1.5,]$Minuten_Pkw = 1
# E.Arzt.dis[E.Arzt.dis$Luftlinie<600,]$Luftlinie = 600
E.Arzt.dis <- E.Arzt.dis[E.Arzt.dis$Minuten_Pkw<30,]

##Correlations
cor(E.Arzt.dis$Minuten_Pkw,E.Arzt.dis$Meter_Pkw)
cor(E.Arzt.dis$Luftlinie,E.Arzt.dis$Meter_Pkw)
cor(E.Arzt.dis$Luftlinie,E.Arzt.dis$Minuten_Pkw)

##Lineare Regression
linearMod <- lm(Meter_Pkw ~ Luftlinie, data=E.Arzt.dis)  # build linear regression model on full data


##New calculation
E.Arzt.dis$lm.Meter <- linearMod$coefficients["(Intercept)"]+linearMod$coefficients["Luftlinie"]*E.Arzt.dis$Luftlinie


summary(linearMod)$coefficients
