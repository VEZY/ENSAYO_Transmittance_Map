####################################################################################################
####---------------------------------------------Paramétrage Meteo Ensayo---------------------------
# Purpose: extract meteo variables, format as MAESPA input, write in met.dat (MAESPA input)
##Author Remi Vezy
####################################################################################################


library(Maeswrap)
source("F:/These/Projects/R-Functions/Maeswrap_cor_VR.R")
PathMeteo= "F:/These/Projects_classified/ENSAYO_Transmittance_Map/DATA/METEO"
Meteoraw=read.table(paste(PathMeteo,"/OutDF.GlobalMetFullSunEnsayo.csv", sep=''), h=T, sep=";")

Meteorawdata= Meteoraw[Meteoraw$DOY>65&Meteoraw$DOY<225,]
SWC= rowMeans(cbind(Meteorawdata$VW.1._m3H2Om3, Meteorawdata$VW.2._m3H2Om3, 
                    Meteorawdata$VW.3._m3H2Om3, Meteorawdata$VW.4._m3H2Om3), na.rm=T)
SWC[is.nan(SWC)]=NA

(nrow(Meteorawdata))/48
#Interpolate missing water content:
library(zoo)
SWC= round(na.approx(SWC),3)
Meteorawdata$DiffuseFrSpitters[is.na(Meteorawdata$DiffuseFrSpitters)]=1
Meteorawdata$RH_pc[Meteorawdata$RH_pc>100]=100
View(summary(is.na(Meteorawdata)))

MAESPA_var= data.frame(DOY=Meteorawdata$DOY, HOUR= rep(1:48, (length(Meteorawdata$DOY)/48)),
                       WIND= round(na.approx(Meteorawdata$WindSpeed3m),3), TAIR= round(na.approx(Meteorawdata$AirTC_degC), 3),
                       RHperc= round(na.approx(Meteorawdata$RH_pc),2), PAR= round(na.approx(Meteorawdata$PAR_micE),2), 
                       FBEAM= round(Meteorawdata$DiffuseFrSpitters,3),
                       PRESS= round(Meteorawdata$P*100,2), SWC, PPT=na.approx(Meteorawdata$Rain_mm_Tot))
summary(is.na(Meteorawdata$AirTC_degC))
plot(MAESPA_var$FBEAM)
summary(is.na(MAESPA_var$FBEAM))
View(sapply(MAESPA_var, head, n=49))

# ATTENTION: carefuly review the meteo variables before writing ------------------------------



# Replace met.dat -----------------------------------------------------------------------------

metdat= "F:/These/Projects_classified/ENSAYO_Transmittance_Map/DATA/Fichiers_dat/Originaux_Aquiares/met.dat"
newmetdat= "F:/These/Projects_classified/ENSAYO_Transmittance_Map/DATA/Fichiers_dat/Nouveaux_ENSAYO/met.dat"
replacemetdata_VR(metdfr=MAESPA_var, oldmetfile = newmetdat, newmetfile = newmetdat)
replacePAR_VR(datfile = newmetdat, namelist = "metformat", parname = "nocolumns", ncol(MAESPA_var))
replacePAR_VR(datfile = newmetdat, namelist = "metformat", parname = "startdate", Meteorawdata$Date_fac[1])
replacePAR_VR(datfile = newmetdat, namelist = "metformat", parname = "enddate", tail(Meteorawdata$Date_fac,1))

# Replace the RHperc by RH% for the columns names in the met.dat
RHloc= grep(paste("RH"), trim((colnames(MAESPA_var))))  #find the position of the character RH
columns= colnames(MAESPA_var)                           #Set the names of the columns
columns[RHloc]= "RH%"                                   #Replace the RHperc by RH%
replacePAR_VR(datfile = newmetdat, namelist = "metformat", parname = "columns", newval = columns)

