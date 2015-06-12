####################################################################################################
####---------------------------------------------Paramétrage Meteo Ensayo---------------------------

# Purpose: extract meteo variables, format as MAESPA input, write in met.dat (MAESPA input)
##Author Remi Vezy

####################################################################################################


library(Maeswrap)
PathMeteo= "F:/These/Projects_classified/ENSAYO_Transmittance_Map/DATA/METEO"
Meteoraw=read.table(paste(PathMeteo,"/OutDF.GlobalMetFullSunEnsayo.csv", sep=''), h=T, sep=";")
head(Meteoraw$Hour_fac)

Meteorawdata= Meteoraw[Meteoraw$DOY>63&Meteoraw$DOY<156,]
head(Meteorawdata)
SWC= rowMeans(cbind(Meteorawdata$VW.1._m3H2Om3, Meteorawdata$VW.2._m3H2Om3, 
                    Meteorawdata$VW.3._m3H2Om3, Meteorawdata$VW.4._m3H2Om3), na.rm=T)
SWC[is.nan(SWC)]=NA

MAESPA_var= data.frame(Meteorawdata$DateSemih, DOY=Meteorawdata$DOY, Wind= Meteorawdata$WindSpeed3m, 
                       TAIR= Meteorawdata$AirTC_degC, RHperc= Meteorawdata$RH_pc, 
                       PAR= Meteorawdata$PAR_micE, FBEAM= Meteorawdata$DiffuseFrSpitters,
                       PRESS= Meteorawdata$P, SWC, PPT=Meteorawdata$Rain_mm_Tot)
MAESPA_var$FBEAM[is.na(MAESPA_var$FBEAM)]=0 # NA's during nighttime, set to 0.
View(MAESPA_var)
summary(is.na(MAESPA_var))
NAtable= which(is.na(MAESPA_var$PAR))
MAESPA_var$DOY[NAtable]
# There are NA's in the DOY 95 and 96. MAESPA doesn't handle NA's.
# We fill these gaps with Aquiares meteo DATA (~15 km)




# Replace met.dat -----------------------------------------------------------------------------

metdat= "F:/These/Projects_classified/ENSAYO_Transmittance_Map/DATA/Fichiers_dat/Originaux_Aquiares/met.dat"
newmetdat= "F:/These/Projects_classified/ENSAYO_Transmittance_Map/DATA/Fichiers_dat/Nouveaux_ENSAYO/met.dat"
replacemetdata(MAESPA_var, oldmetfile = metdat, newmetfile = newmetdat)


