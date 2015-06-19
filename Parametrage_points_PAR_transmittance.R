#######################################################################################################################
####---------------------------------------------Paramétrage fichier point.dat-------------------------------------####
#######################################################################################################################



# Import --------------------------------------------------------------------------------------

library(Maeswrap)
library(rgdal)

Path_sensible_data= "F:/These/Projects_classified/ENSAYO_Transmittance_Map"
#Path to the folder that store all sensible data that I cannot publicly share.
source("F:/These/Projects/R-Functions/Maeswrap_cor_VR.R")
Coord_All_Coffees= read.table(paste(Path_sensible_data,"/DATA/Coord_All_coffes.csv", sep=""),
                              h=T, sep=";", numerals = "no.loss")


# Creation du fichier de points ---------------------------------------------------------------

Z_position= rep(2, nrow(Coord_All_Coffees))
minY= 1094942.737 #Y minimum du plot entr? dans MAESPA. Cr?e un nouveau rep?re spatial.
minX= 207104.504

Table_points= as.data.frame(cbind(X_position=round((Coord_All_Coffees$X-minX),3), 
                    Y_position=round((Coord_All_Coffees$Y-minY),3), 
                    Z_position))
# replacePAR(datfile = points, namelist = "CONTROL", parname = "NOPOINTS", newval = nrow(Coord_All_Coffees))
# replacePAR(datfile = points, namelist = "XYZ", parname = "COORDS", newval = Table_points)


# Days of simulation choice -------------------------------------------------------------------
# Two contrasted days in terms of FBEAM.

meteo= readmet("F:/These/Projects_classified/ENSAYO_Transmittance_Map/DATA/Fichiers_dat/Nouveaux_ENSAYO/met.dat")
meteo[which(meteo$FBEAM<0.25&meteo$PAR>2000),]
#Le jour 88 est un jour intéressant puisqu'il présente un PAR très élevé  et un FBEAM assez bas.
head(meteo[which(meteo$FBEAM>0.9&meteo$FBEAM<1&meteo$PAR>800),],10)
#Le jour 76 aussi car il a un FBEAM proche de 1 (mais pas =1, qui sont des gap-fil),et un PAR assez élevé.

# MAESPA Call ---------------------------------------------------------------------------------

confile= paste(Path_sensible_data, "/MAESPA_routine/MAESPA_Remko_VR/confile.dat", sep='')
points= paste(Path_sensible_data, "/MAESPA_routine/MAESPA_Remko_VR/points.dat", sep='')
NPoints= nrow(Coord_All_Coffees)
replacePAR_VR(datfile=confile, parname = "startdate", namelist = "dates", "17/03/2015")
replacePAR_VR(datfile=confile, parname = "enddate", namelist = "dates", "17/03/2015")
replacePAR_VR(datfile = points, namelist = "CONTROL", parname = "NOPOINTS", newval = NPoints)
replacePAR_VR(datfile = points, namelist = "XYZ", parname = "COORDS", newval = Table_points) 
system(paste(Path_sensible_data,'/MAESPA_routine/MAESPA_Remko_VR/MAESPA_Remko_VR', sep=''), show.output.on.console=T)

Results_29_03= readtestflx(paste(Path_sensible_data,"/MAESPA_routine/MAESPA_Remko_VR/testflx_29_03.dat", sep=""))
Results_17_03= readtestflx(paste(Path_sensible_data,"/MAESPA_routine/MAESPA_Remko_VR/testflx_17_03.dat", sep=""))


# Prepares the results ---------------------------------------------------------------------

library(raster)
#Rajouter les Y et X min de la parcelle en coordonn?es WGS 84 (cf Parametrage_Trees_ENSAYO_OR2 l.281)
Results_29_03$X= Results_29_03$X + minX
Results_29_03$Y= Results_29_03$Y + minY
Results_17_03$X= Results_17_03$X + minX
Results_17_03$Y= Results_17_03$Y + minY
# Defines the system projection:
crsplot="+proj=utm +zone=17 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
# Ref: http://spatialreference.org/ref/epsg/wgs-84-utm-zone-17n/proj4/
# Create an empty raster:
# Raster_plot= raster(nrows=350, ncols=518, xmn=minX, xmx=(518+minX), ymn=minY, ymx=(350+minY), 
#                    crs=crsplot) # Pour les plots et les représentations (plus propre)

Raster_plot= raster(nrows=3500, ncols=5180, xmn=minX, xmx=(518+minX), ymn=minY, ymx=(350+minY), 
                    crs=crsplot)
# nrows: on a 349.431 m en axe des X. Pour avoir une carte de resolution 10 cm, on prends un 
# pixel = 10 cm, donc il nous faudra 3494 (arrondi a 3500) pixels pour couvrir l'axe des Y a 
# cette resolution. ncol: idem


# Make a mean table by point (mean of the days and the hours) ---------------------------------

Results_jourmoy_29_03= aggregate(Results_29_03[,-c(1:3,6)], list(Results_29_03[,3]), mean)
Results_jourmoy_17_03= aggregate(Results_17_03[,-c(1:3,6)], list(Results_17_03[,3]), mean)

# Create a raster layer and plot it -----------------------------------------------------------

temp_Results_TTOT_29= cbind(x=Results_jourmoy_29_03$X, y=Results_jourmoy_29_03$Y, z=(Results_jourmoy_29_03$TTOT))
TTOT_plot_29= rasterize(temp_Results_TTOT_29[,1:2], Raster_plot, field= temp_Results_TTOT_29[,3])
plot(TTOT_plot_29, col=terrain.colors(100), main=expression(Incident~PAR~(µmol.m^-2~.s^-1)))
plot(TTOT_plot_29, col=rainbow(100, start=0, end=0.9), main=expression(Incident~PAR~(µmol.m^-2~.s^-1)))

temp_Results_TTOT_17= cbind(x=Results_jourmoy_17_03$X, y=Results_jourmoy_17_03$Y, z=(Results_jourmoy_17_03$TTOT))
TTOT_17_plot= rasterize(temp_Results_TTOT_17[,1:2], Raster_plot, field= temp_Results_TTOT_17[,3])
plot(TTOT_17_plot, col=terrain.colors(100), main=expression(Incident~PAR~(µmol.m^-2~.s^-1)))


# Control the legend's scale and color --------------------------------------------------------

x11()
breakpoints <- seq(0, 450, 35)
#colors <- rainbow(100, start=0, end=1)
colors= rev(terrain.colors(16))
# colors <- gray.colors(14, start=0, end=1)
plot(TTOT_plot,breaks=breakpoints,col=colors, main='TestFlux APAR (umol m-2 s-1)')

# Add the ENSAYO Trees on the map:
# TREES <- readOGR(dsn="F:/These/Projects_classified/GIS_ENSAYO", layer="Trees_B3_MS")
# par(mfrow=c(1,1))
# plot(TREES, add=T)
# par(new=T)



# Make maps by hour ---------------------------------------------------------------------------

Transmittance_29= Results_29_03$TTOT / Results_29_03$PAR
Transmittance_17= Results_17_03$TTOT / Results_17_03$PAR
mypath="F:/These/Projects_classified/ENSAYO_Transmittance_Map/Results/TIFF"
Transmittance_29_raster=Daily_testflux(Results_29_03, Transmittance_29, Filepath= mypath, 
               FileName= "Tranmittance_Hour_",
               VariableTitle= bquote(Transmittance~("[0-1]")~DOY~88~Low~FBEAM))
Diffuse_High= Daily_testflux(X= Results_29_03, Results_29_03$TD, Filepath= mypath, 
               FileName= "Diffuse_Tranmittance_29_03_2015", maxHour = min(Results_29_03$HR),
               VariableTitle= bquote(Diffuse~Transmittance~("[0-1]")~DOY~88~Low~FBEAM), writeIt = T)
#  maxHour = min(Results_29_03$HR) because we only need one hour as TD doesn't change over day time.
Daily_testflux(Results_17_03, Transmittance_17, Filepath= mypath, FileName= "Tranmittance_Hour_",
               VariableTitle= bquote(Transmittance~("[0-1]")~DOY~76~High~FBEAM), writeIt = F)
Diffuse_Low= Daily_testflux(X= Results_17_03, TargetVariable= Results_17_03$TD, 
               Filepath= mypath, FileName= "Diffuse_Tranmittance_17_03_2015",
               VariableTitle= bquote(Diffuse~Transmittance~("[0-1]")~DOY~76~High~FBEAM),
               maxHour= min(Results_17_03$HR), writeIt = F)
writeRaster(Diffuse_Low, filename= file.path(Filepath,paste(FileName, ".asc", sep="")),
            format="ascii")