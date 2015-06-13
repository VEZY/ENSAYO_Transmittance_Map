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
# replacePAR(datfile = points, namelist = "CONTROL", parname = "NOPOINTS", newval = nrow(Coord_All_Coffees))
# replacePAR(datfile = points, namelist = "XYZ", parname = "COORDS", newval = Table_points)

confile= paste(Path_sensible_data, "/MAESPA_routine/MAESPA_Remko_VR/confile.dat", sep='')
points= paste(Path_sensible_data, "/MAESPA_routine/MAESPA_Remko_VR/points.dat", sep='')
NPoints= nrow(Coord_All_Coffees)
replacePAR_VR(datfile=confile, parname = "startdate", namelist = "dates", "07/03/2015")
replacePAR_VR(datfile=confile, parname = "enddate", namelist = "dates", "07/03/2015")
replacePAR_VR(datfile = points, namelist = "CONTROL", parname = "NOPOINTS", newval = NPoints)
replacePAR_VR(datfile = points, namelist = "XYZ", parname = "COORDS", newval = Table_points) 
system(paste(Path_sensible_data,'/MAESPA_routine/MAESPA_Remko_VR/MAESPA_Remko_VR', sep=''), show.output.on.console=T)
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
breakpoints <- seq(0, 450, 35)
#colors <- rainbow(100, start=0, end=1)
colors= rev(terrain.colors(16))
# colors <- gray.colors(14, start=0, end=1)
plot(APAR_plot,breaks=breakpoints,col=colors, main='TestFlux APAR (umol m-2 s-1)')

# Add the ENSAYO Trees on the map:
# TREES <- readOGR(dsn="F:/These/Projects_classified/GIS_ENSAYO", layer="Trees_B3_MS")
# par(mfrow=c(1,1))
# plot(TREES, add=T)
# par(new=T)



# Make maps by hour ---------------------------------------------------------------------------

head(Results)
NBHours= 

breakpoints <- seq(0, 1200, 50)
breakpoints= breakpoints[-c(2:5)]
colors <- rainbow(100, start=0, end=1)

Rasters_Trans= vector("list", length(levels(as.factor(Results$HR))))
Rasters_Trans[[1]]= test
minHour= min(Results$HR)
maxHour= max(Results$HR)
x11()
for (i in minHour:maxHour){
Results_Hour= Results[Results$HR==i,]
Results_Hour_APAR= cbind(x=Results_Hour$X, y=Results_Hour$Y, z=(Results_Hour$TTOT))
Hour_APAR_plot=Raster_plot
cell1 <- cellFromXY(Hour_APAR_plot, Results_Hour_APAR[,1:2])
Hour_APAR_plot[cell1] <- Results_Hour_APAR[,3]
filename= paste("Tranmittance_Hour_", i, ".tiff", sep="")
mypath=file.path("F:/These/Projects_classified/ENSAYO_Transmittance_Map/Results", filename)
tiff(file=mypath,  width= 1311 , height= 840, res=96)
Title = bquote(Semi-Hour~N~degree~.(i)~Incident~PAR~(µmol.m^-2~.s^-1))
plot(Hour_APAR_plot, main=Title)
dev.off()
}






