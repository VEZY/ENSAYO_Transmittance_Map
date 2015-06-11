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
str(Coord_All_Coffees)


# Creation du fichier de points ---------------------------------------------------------------

Z_position= rep(2, nrow(Coord_All_Coffees))
minY= 1094942.737 #Y minimum du plot entr? dans MAESPA. Cr?e un nouveau rep?re spatial.
minX= 207104.504

Table_points= as.data.frame(cbind(X_position=round((Coord_All_Coffees$X-minX),3), 
                    Y_position=round((Coord_All_Coffees$Y-minY),3), 
                    Z_position))
# replacePAR(datfile = "points.dat", namelist = "CONTROL", parname = "NOPOINTS", newval = nrow(Coord_All_Coffees))
# replacePAR(datfile = "points.dat", namelist = "XYZ", parname = "COORDS", newval = Table_points)


NPoints= nrow(Coord_All_Coffees)
replacePAR_VR(datfile="confile.dat", parname = "startdate", namelist = "dates", "01/01/15")
replacePAR_VR(datfile="confile.dat", parname = "enddate", namelist = "dates", "01/01/15")
replacePAR_VR(datfile = "points.dat", namelist = "CONTROL", parname = "NOPOINTS", newval = NPoints)
replacePAR_VR(datfile = "points.dat", namelist = "XYZ", parname = "COORDS", newval = Table_points) 
system('MAESPA_routine/MAESPA_Remko_VR', show.output.on.console=T)
Results= readtestflx(paste(Path_sensible_data,"/MAESPA_routine/MAESPA_Remko_VR/testflx.dat", sep=""))


# Prepares the results ---------------------------------------------------------------------

library(raster)
#Rajouter les Y et X min de la parcelle en coordonn?es WGS 84 (cf Parametrage_Trees_ENSAYO_OR2 l.281)
Results$X= Results$X + minX
Results$Y= Results$Y + minY
# Defines the system projection:
crsplot="+proj=utm +zone=17 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
#ref: http://spatialreference.org/ref/epsg/wgs-84-utm-zone-17n/proj4/
# Create an empty raster:
Raster_plot= raster(nrows=350, ncols=518, xmn=minX, xmx=(518+minX), ymn=minY, ymx=(350+minY), 
                   crs=crsplot)
# nrows: on a 349.431 m en axe des X. Pour avoir une carte ? la r?solution des 10 cm, on prends un 
# pixel = 10 cm, donc il nous faudra 3494 pixels pour couvrir l'axe des Y ? cette r?solution.
# ncol: idem


# Make a mean table by point (mean of the days and the hours) ---------------------------------

Results_jourmoy= aggregate(Results[,-c(1:3,6)], list(Results[,3]), mean)
str(Results_jourmoy)


# Create a raster layer and plot it -----------------------------------------------------------

head(Results_jourmoy)
temp_Results_APAR= cbind(x=Results_jourmoy$X, y=Results_jourmoy$Y, z=(Results_jourmoy$TTOT))
APAR_plot=Raster_plot
cell1 <- cellFromXY(APAR_plot, temp_Results_APAR[,1:2])
# finds the corresponding coordinates of temp_Results in APAR_plot
APAR_plot[cell1] <- temp_Results_APAR[,3]
# writeRaster(APAR_plot, "Results/APAR_plot2.asc", format="ascii", overwrite=T)
APAR_plot=raster("Results/APAR_plot.asc")

plot(APAR_plot, col=terrain.colors(100), main=expression(Incident~PAR~(µmol.m^-2~.s^-1)))
plot(APAR_plot, col=rainbow(100, start=0, end=0.9), main=expression(Incident~PAR~(µmol.m^-2~.s^-1)))



# Control the legend's scale and color --------------------------------------------------------

x11()
breakpoints <- seq(0, 450, 30)
colors <- rainbow(15, start=0, end=1)
# colors= terrain.colors(15)
plot(APAR_plot,breaks=breakpoints,col=colors, main='TestFlux APAR (umol m-2 s-1)')

# Add the ENSAYO Trees on the map:
# TREES <- readOGR(dsn="F:/These/Projects_classified/GIS_ENSAYO", layer="Trees_B3_MS")
# par(mfrow=c(1,1))
# plot(TREES, add=T)
# par(new=T)