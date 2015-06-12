####################################################################################################
####---------------------------------------------Paramétrage Meteo Ensayo---------------------------
# Purpose: extract meteo variables, format as MAESPA input, write in met.dat (MAESPA input)
##Author Remi Vezy
####################################################################################################


library(Maeswrap)
PathMeteo= "F:/These/Projects_classified/ENSAYO_Transmittance_Map/DATA/METEO"
Meteoraw=read.table(paste(PathMeteo,"/OutDF.GlobalMetFullSunEnsayo.csv", sep=''), h=T, sep=";")
head(Meteoraw)

Meteorawdata= Meteoraw[Meteoraw$DOY>63&Meteoraw$DOY<159,]
head(Meteorawdata)
SWC= rowMeans(cbind(Meteorawdata$VW.1._m3H2Om3, Meteorawdata$VW.2._m3H2Om3, 
                    Meteorawdata$VW.3._m3H2Om3, Meteorawdata$VW.4._m3H2Om3), na.rm=T)
SWC[is.nan(SWC)]=NA


#Interpolate missing water content:
library(zoo)
SWC= na.approx(SWC)


MAESPA_var= data.frame(DATE= Meteorawdata$Date_fac, DOY=Meteorawdata$DOY, 
                       HOUR= Meteorawdata$Hour_fac, WIND= Meteorawdata$WindSpeed3m, 
                       TAIR= Meteorawdata$AirTC_degC, RHperc= Meteorawdata$RH_pc, 
                       PAR= Meteorawdata$PAR_micE, FBEAM= Meteorawdata$DiffuseFrSpitters,
                       PRESS= Meteorawdata$P, SWC, PPT=Meteorawdata$Rain_mm_Tot)


summary(is.na(MAESPA_var))
NAtable= which(is.na(MAESPA_var$PAR))
MAESPA_var$DOY[NAtable]
View(Meteoraw)




# ATTENTION: carefuly review the meteo variables before wrinting ------------------------------







# Replace met.dat -----------------------------------------------------------------------------

metdat= "F:/These/Projects_classified/ENSAYO_Transmittance_Map/DATA/Fichiers_dat/Originaux_Aquiares/met.dat"
newmetdat= "F:/These/Projects_classified/ENSAYO_Transmittance_Map/DATA/Fichiers_dat/Nouveaux_ENSAYO/met.dat"
replacemetdata(MAESPA_var, oldmetfile = metdat, newmetfile = newmetdat)

replacePAR_VR(datfile = newmetdat, namelist = "metformat", parname = "nocolumns", ncol(MAESPA_var))
replacePAR_VR(datfile = newmetdat, namelist = "metformat", parname = "startdate", MAESPA_var$Date[1])
replacePAR_VR(datfile = newmetdat, namelist = "metformat", parname = "enddate", tail(MAESPA_var$Date,1))

# Replace the RHperc by RH% for the columns names in the met.dat
RHloc= grep(paste("RH"), trim((colnames(MAESPA_var))))  #find the position of the character RH
columns= colnames(MAESPA_var)                           #Set the names of the columns
columns[RHloc]= "RH%"                                   #Replace the RHperc by RH%
replacePAR_VR(datfile = newmetdat, namelist = "metformat", parname = "columns", newval = columns)













